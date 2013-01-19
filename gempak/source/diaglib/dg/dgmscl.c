#include "dg.h"

#define RPOLE	( HALFPI - DTR )

void dg_mscl ( int *iret )
/************************************************************************
 * dg_mscl								*
 *									*
 * This subroutine computes the scale factors for both coordinate	*
 * directions and stores them in internal grids to which IXMSCL and	*
 * IYMSCL point.  The differential increments along the coordinate	*
 * directions are computed and stored in GDDX and GDDY in the grid	*
 * common area.								*
 *									*
 * dg_mscl ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-16 = grid projection error	*
 **									*
 * Log:									*
 * K. F. BRILL/GSC	 4/89 						*
 * K. Brill/NMC		 2/91	Fix for wrap-around lat/lon grid	*
 * K. Brill/EMC		 3/96	Changes for general projections		*
 * T. Lee/GSC		 1/97	Set xmsdy = 0 near poles for CED	*
 * K. Brill/HPC		 5/02	Use internal grids for lat/lon in Q;	*
 * 				Store results in internal grids		*
 * K. Brill/HPC		 3/04	Use Q coordinates to compute scale fctrs*
 *				if anglr1 or anglr3 indicate rotation	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float qix[2], qiy[2], dix, diy, angtst, yy, xmsave, psi;
    int swchxy, lxms, lyms, isrot;
    int two, igqlat, igqlon, ix, iy, i, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    two = 2;

    /*
     * Check to see if mapscale factors need to be computed.
     */
    dg_cndg ( "X_MAPSCLFCTR", &_mapscl.ixmscl, &lxms, iret );
    if ( *iret != 0 ) return;
    dg_cndg ( "Y_MAPSCLFCTR", &_mapscl.iymscl, &lyms, iret );
    if ( *iret != 0 ) return;

    if ( lxms == G_TRUE && lyms == G_TRUE ) return;

    /*
     * Compute actual lat/lon's of the grid points.
     */
    dg_ltln ( iret );
    if ( *iret != 0 ) return;

    /*
     * Compute I coordinate bounds for computing GDDX and GDDY.
     */
    qix[0] = 1.;
    qiy[0] = 1.;
    qix[1] = (float)_dgfile.kxd;
    qiy[1] = (float)_dgfile.kyd;
    gtrans ( sys_G, sys_I, &two, qix, qiy, qix, qiy, &ier,
             strlen(sys_G), strlen(sys_I) );
    if ( ier != 0 ) {
	*iret = -16;
	return;
    }
    dix = qix[1] - qix[0];
    diy = qiy[1] - qiy[0];

    /*
     * If the grid is rotated, Q coordinates differ from LAT/LON.
     */
    isrot = G_FALSE;
    if ( strcmp ( _dgfile.cprj, "CED" ) == 0 ||
         strcmp ( _dgfile.cprj, "MER" ) == 0 ||
	 strcmp ( _dgfile.cprj, "MCD" ) == 0 ) {
	if ( !G_DIFFT(_dgfile.anglr1, 0.0F, GDIFFD) ||
	     !G_DIFFT(_dgfile.anglr3, 0.0F, GDIFFD) ) {
	    isrot = G_TRUE;
	}
    } else {
	angtst = G_ABS ( G_ABS ( _dgfile.anglr1 ) - HALFPI );
	if ( angtst > 1.E-5 || !G_DIFFT(_dgfile.anglr3, 0.0F, GDIFFD) ) {
	    isrot = G_TRUE;
	}
    }

    if ( isrot == G_FALSE ) {
	/*
	 * The grid projection is not rotated.
	 */
	igqlat = _dgfile.idglat;
	igqlon = _dgfile.idglon;
    } else {
	/*
	 * Allocate internal grids for local scratch use.
	 */
	dg_nxts ( &igqlat, iret );
	if ( *iret != 0 ) return;
	dg_nxts ( &igqlon, iret );
	if ( *iret != 0 ) return;

	/*
	 * Compute lat/lon in the ROTATED lat/lon coordinate.
	 */
	i = 0;
	for ( iy = 1; iy <= _dgfile.kyd; iy++ ) {
	    yy = (float)iy;
	    for ( ix = 1; ix <= _dgfile.kxd; ix++ ) {
		_dggrid.dgg[igqlat-1].grid[i] = (float)ix;
		_dggrid.dgg[igqlon-1].grid[i] = yy;

	        i++;
	    }
	}
	gtrans ( sys_G, sys_Q, &_dgfile.kxyd, _dggrid.dgg[igqlat-1].grid,
	    _dggrid.dgg[igqlon-1].grid, _dggrid.dgg[igqlat-1].grid,
	    _dggrid.dgg[igqlon-1].grid, &ier, strlen(sys_G), strlen(sys_Q) );
	if ( ier != 0 ) {
	    *iret = -16;
	    return;
	}
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    if ( ! ERMISS ( _dggrid.dgg[igqlat-1].grid[i] ) &&
		 ! ERMISS ( _dggrid.dgg[igqlon-1].grid[i] ) ) {
		_dggrid.dgg[igqlat-1].grid[i] *= DTR;
		_dggrid.dgg[igqlon-1].grid[i] *= DTR;
	    }
	}
    }

    /*
     * Check for missing lat/lon.
     */
    for ( i = 0; i < _dgfile.kxyd; i++ ) {
	if ( ERMISS ( _dggrid.dgg[igqlat-1].grid[i] ) ||
	     ERMISS ( _dggrid.dgg[igqlon-1].grid[i] ) ) { 
	    *iret = -16;
	    return;
	}
    }

    /*
     * Check for transverse case of CED or MER.
     */
    if ( strcmp ( _dgfile.cprj, "CED" ) == 0 ||
         strcmp ( _dgfile.cprj, "MER" ) == 0 ) {
	swchxy = G_FALSE;
	if ( G_ABS ( _dggrid.dgg[igqlat-1].grid[0] -
	             _dggrid.dgg[igqlat-1].grid[_dgfile.kxd] ) < 1.E-5 ) {
	    swchxy = G_TRUE;
	}
    }

    if ( strcmp ( _dgfile.cprj, "CED" ) == 0 ) {
	/*
	 * Compute the scale factors on the cylindrical equidistant 
	 * projection.  Set xmscl to be constant near the poles.
	 */
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    if ( G_ABS ( _dggrid.dgg[igqlat-1].grid[i] ) >= RPOLE ) {
		_dggrid.dgg[_mapscl.ixmscl-1].grid[i] = 1. / cos ( RPOLE );
	    } else {
		_dggrid.dgg[_mapscl.ixmscl-1].grid[i] = 1. / cos ( _dggrid.dgg[igqlat-1].grid[i] );
	    }
	    _dggrid.dgg[_mapscl.iymscl-1].grid[i] = 1.;

	    if ( swchxy == G_TRUE ) {
		xmsave = _dggrid.dgg[_mapscl.ixmscl-1].grid[i];
		_dggrid.dgg[_mapscl.ixmscl-1].grid[i] = _dggrid.dgg[_mapscl.iymscl-1].grid[i];
		_dggrid.dgg[_mapscl.iymscl-1].grid[i] = xmsave;
	    }
	}

	/*
	 * Compute delta x and delta y for the grid.
	 */
	_mapscl.gddx = RADIUS * dix / (float)( _dgfile.kxd - 1 );
	_mapscl.gddy = RADIUS * diy / (float)( _dgfile.kyd - 1 );
    } else if ( strcmp ( _dgfile.cprj, "MER" ) == 0 ) {
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    _dggrid.dgg[_mapscl.ixmscl-1].grid[i] = 1. / cos ( _dggrid.dgg[igqlat-1].grid[i] );
	    _dggrid.dgg[_mapscl.iymscl-1].grid[i] = _dggrid.dgg[_mapscl.ixmscl-1].grid[i];
	}
	_mapscl.gddx = RADIUS * dix / (float)( _dgfile.kxd - 1 );
	_mapscl.gddy = RADIUS * diy / (float)( _dgfile.kyd - 1 );
    } else if ( strcmp ( _dgfile.cprj, "STR" ) == 0 ) {
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    if ( _dgfile.anglr1 > 0.0 ) {
		_dggrid.dgg[_mapscl.ixmscl-1].grid[i] = 1. / ( 1. + sin ( _dggrid.dgg[igqlat-1].grid[i] ) );
	    } else {
		_dggrid.dgg[_mapscl.ixmscl-1].grid[i] = 1. / ( 1. + sin ( -_dggrid.dgg[igqlat-1].grid[i] ) );
	    }
	    _dggrid.dgg[_mapscl.iymscl-1].grid[i] = _dggrid.dgg[_mapscl.ixmscl-1].grid[i];
	}
	_mapscl.gddx = RADIUS * dix / (float)( _dgfile.kxd - 1 );
	_mapscl.gddy = RADIUS * diy / (float)( _dgfile.kyd - 1 );
    } else if ( strcmp ( _dgfile.cprj, "LCC" ) == 0 ||
                strcmp ( _dgfile.cprj, "SCC" ) == 0 ) {
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    if ( _dgfile.anglr1 > 0.0 ) {
		psi = HALFPI - _dggrid.dgg[igqlat-1].grid[i];
	    } else {
		psi = HALFPI + _dggrid.dgg[igqlat-1].grid[i];
	    }
	    _dggrid.dgg[_mapscl.ixmscl-1].grid[i] = pow ( tan ( psi / 2. ), _dgfile.concon ) / sin ( psi );
	    _dggrid.dgg[_mapscl.iymscl-1].grid[i] = _dggrid.dgg[_mapscl.ixmscl-1].grid[i];
	}
	_mapscl.gddx = ( RADIUS / _dgfile.concon ) * dix / (float) ( _dgfile.kxd - 1 );
	_mapscl.gddy = ( RADIUS / _dgfile.concon ) * diy / (float) ( _dgfile.kyd - 1 );
    } else {
	*iret = -16;
	strcpy ( _dgerr.errst, _dgfile.cprj );
	return;
    }

    /*
     * Free local scratch grids.
     */
    if ( isrot == G_TRUE ) {
        dg_frig ( &igqlat, &ier );
	dg_frig ( &igqlon, &ier );
    }

    /*
     * Make sure grid increments are positive.
     */
    _mapscl.gddx = G_ABS ( _mapscl.gddx );
    _mapscl.gddy = G_ABS ( _mapscl.gddy );

    return;
}
