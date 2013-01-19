#include "dg.h"

#define RPOLE	( HALFPI - DTR )

void dg_tscl ( int *ixtms, int *iytms, float *tgdx, float *tgdy,
               float *cone, int *iret )
/************************************************************************
 * DG_TSCL								*
 *									*
 * This subroutine computes the transfer navigation scale factors for	*
 * both coordinate directions and stores them in internal grids to	*
 * which IXTMS and IYTMS point.  The differential increments along the	*
 * coordinate directions are computed and returned in TGDX and TGDY.	*
 * The constant of the cone for the transfer navigation is computed and *
 * returned.								*
 *									*
 * The scale factors apply to the tranfer navigation, but are returned  *
 * at points on the internal grid.					*
 *									*
 * NOTE:  DG_LTLN must be called before this subroutine is called, and	*
 *        the transfer navigation is assumed to be set in GPLT.		*
 *									*
 * DG_TSCL  ( IXTMS, IYTMS, TGDX, TGDY, CONE, IRET )			*
 *									*
 * Output parameters:							*
 *	IXTMS		INTEGER		Internal grid # of transfer	*
 *					navigation x scale factors	*
 *	IYTMS		INTEGER		Internal grid # of transfer	*
 *					navigation x scale factors	*
 *	TGDX		REAL		Increment along X		*
 *	TGDY		REAL		Increment along Y		*
 *	CONE		REAL		Constant of the cone		*
 *	IRET		INTEGER		Return code			*
 *					  0 = normal return		*
 *					-16 = grid projection error	*
 **									*
 * Log:									*
 * K. Brill/HPC		 3/04	Created from DG_MSCL			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char gprj[5];
    float qix[2], qiy[2], angr1, angr3, dix, diy, angtst, rlt1, rlt2,
        xmsave, psi;
    int swchxy, isrot;
    int nc, mx, my, igqlat, igqlon, two, k, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    two = 2;

    /*
     * Get internal grids for the tranfer navigation map scale factors.
     */
    dg_nxts ( ixtms, iret );
    if ( *iret != 0 ) return;
    dg_nxts ( iytms, iret );
    if ( *iret != 0 ) return;

    /*
     * Get the require reference navigation information.
     */
    cst_itos ( (int *)(&_hintrp.tfrnav[1]), 1, &nc, gprj, &ier );
    cst_rmbl ( gprj, gprj, &nc, &ier );
    mx = G_NINT ( _hintrp.tfrnav[4] );
    my = G_NINT ( _hintrp.tfrnav[5] );
    if ( _hintrp.adcltg == G_TRUE ) {
	mx += 1;
    }
    angr1 = _hintrp.tfrnav[10] * DTR;
    angr3 = _hintrp.tfrnav[12] * DTR;

    /*
     * Compute the constant of the cone.
     */
    dg_cone ( gprj, &angr1, &angr3, cone, iret );

    /*
     * Compute I coordinate bounds for computing GDDX and GDDY.
     */
    qix[0] = 1.;
    qiy[0] = 1.;
    qix[1] = (float)mx;
    qiy[1] = (float)my;
    gtrans ( sys_G, sys_I, &two, qix, qiy, qix, qiy, &ier,
             strlen(sys_G), strlen(sys_I) );
    if ( ier != 0 ) {
	*iret = -16;
	return;
    }

    /*
     * Compute total linear coordinate distances on projection
     * plane.
     */
    dix = qix[1] - qix[0];
    diy = qiy[1] - qiy[0];

    /*
     * If the grid is rotated, Q coordinates differ from LAT/LON.
     */
    isrot = G_FALSE;
    if ( strcmp ( gprj, "CED" ) == 0 || strcmp ( gprj, "MER" ) == 0 ||
	 strcmp ( gprj, "MCD" ) == 0 ) {
	if ( !G_DIFFT(angr1, 0.0F, GDIFFD) || !G_DIFFT(angr3, 0.0F, GDIFFD) ) {
	    isrot = G_TRUE;
	}
    } else {
	angtst = G_ABS ( G_ABS (angr1) - HALFPI );
	if ( angtst > 1.E-5 || !G_DIFFT(angr3, 0.0F, GDIFFD) ) {
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
	 * Convert lat/lons to degrees.
	 */
	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    if ( ! ERMISS ( _dggrid.dgg[_dgfile.idglat-1].grid[k] ) )
		_dggrid.dgg[_dgfile.idglat-1].grid[k] *= RTD;
	    if ( ! ERMISS ( _dggrid.dgg[_dgfile.idglon-1].grid[k] ) )
		_dggrid.dgg[_dgfile.idglon-1].grid[k] *= RTD;
	}

	/*
	 * Compute lat/lon in the ROTATED lat/lon coordinate.
	 */
	gtrans ( sys_M, sys_Q, &_dgfile.kxyd,
	    _dggrid.dgg[_dgfile.idglat-1].grid,
	    _dggrid.dgg[_dgfile.idglon-1].grid,
	    _dggrid.dgg[igqlat-1].grid, _dggrid.dgg[igqlon-1].grid, &ier,
	    strlen(sys_M), strlen(sys_Q) );
	if ( ier != 0 ) {
	    *iret = -16;
	    return;
	}

	/*
	 * Convert lat/lons to radians.
	 */
	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    if ( ! ERMISS ( _dggrid.dgg[_dgfile.idglat-1].grid[k] ) )
		_dggrid.dgg[_dgfile.idglat-1].grid[k] *= DTR;
	    if ( ! ERMISS ( _dggrid.dgg[_dgfile.idglon-1].grid[k] ) )
		_dggrid.dgg[_dgfile.idglon-1].grid[k] *= DTR;
	}

	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    if ( ! ERMISS ( _dggrid.dgg[igqlat-1].grid[k] ) &&
		 ! ERMISS ( _dggrid.dgg[igqlon-1].grid[k] ) ) {
		_dggrid.dgg[igqlat-1].grid[k] *= DTR;
		_dggrid.dgg[igqlon-1].grid[k] *= DTR;
	    }
	}
    }

    /*
     * Check for missing lat/lon.
     */
    for ( k = 0; k < _dgfile.kxyd; k++ ) {
	if ( ERMISS ( _dggrid.dgg[igqlat-1].grid[k] ) ||
	     ERMISS ( _dggrid.dgg[igqlon-1].grid[k] ) ) {
	    *iret = -16;
	    return;
	}
    }

    /*
     * Check for transverse case of CED or MER.
     */
    if ( strcmp ( gprj, "CED" ) == 0 || strcmp ( gprj, "MER" ) == 0 ) {
	swchxy = G_FALSE;
	qix[0] = 1.;
	qiy[0] = 1.;
	qix[1] = 1.;
	qiy[1] = 2.;
	gtrans ( sys_G, sys_Q, &two, qix, qiy, qix, qiy, &ier,
	         strlen(sys_G), strlen(sys_Q) );
	rlt1 = qix[0] * DTR;
	rlt2 = qix[1] * DTR;

	/*
	 * Is latitude constant in y direction?
	 */
	if ( G_ABS ( rlt1 - rlt2 ) < 1.E-5 ) {
	    swchxy = G_TRUE;
	}
    }

    if ( strcmp ( gprj, "CED" ) == 0 ) {
	/*
	 * Compute the scale factors on the cylindrical equidistant 
	 * projection.  Set xmscl to be constant near the poles.
	 */
	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    if ( G_ABS ( _dggrid.dgg[igqlat-1].grid[k] ) >= RPOLE ) {
		_dggrid.dgg[(*ixtms)-1].grid[k] = 1. / cos ( RPOLE );
	    } else {
		_dggrid.dgg[(*ixtms)-1].grid[k] = 1. / cos ( _dggrid.dgg[igqlat-1].grid[k] );
	    }
	    _dggrid.dgg[(*iytms)-1].grid[k] = 1.;
	    if ( swchxy == G_TRUE ) {
		xmsave = _dggrid.dgg[(*ixtms)-1].grid[k];
		_dggrid.dgg[(*ixtms)-1].grid[k] = _dggrid.dgg[(*iytms)-1].grid[k];
		_dggrid.dgg[(*iytms)-1].grid[k] = xmsave;
	    }
	}

	/*
	 * Compute delta x and delta y for the grid.
	 */
	*tgdx = RADIUS * dix / (float)( mx - 1 );
	*tgdy = RADIUS * diy / (float)( my - 1 );
    } else if ( strcmp ( gprj, "MER" ) == 0 ) {
	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    _dggrid.dgg[(*ixtms)-1].grid[k] = 1. / cos ( _dggrid.dgg[igqlat-1].grid[k] );
	    _dggrid.dgg[(*iytms)-1].grid[k] = _dggrid.dgg[(*ixtms)-1].grid[k];
	}
	*tgdx = RADIUS * dix / (float)( mx - 1 );
	*tgdy = RADIUS * diy / (float)( my - 1 );
    } else if ( strcmp ( gprj, "STR" ) == 0 ) {
	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    if ( angr1 > 0.0 ) {
		_dggrid.dgg[(*ixtms)-1].grid[k] = 1. / ( 1. + sin ( _dggrid.dgg[igqlat-1].grid[k] ) );
	    } else {
		_dggrid.dgg[(*ixtms)-1].grid[k] = 1. / ( 1. + sin ( -_dggrid.dgg[igqlat-1].grid[k] ) );
	    }
	    _dggrid.dgg[(*iytms)-1].grid[k] = _dggrid.dgg[(*ixtms)-1].grid[k];
	}
	*tgdx = RADIUS * dix / (float)( mx - 1 );
	*tgdy = RADIUS * diy / (float)( my - 1 );
    } else if ( strcmp ( gprj, "LCC" ) == 0 || strcmp ( gprj, "SCC" ) == 0 ) {
	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    if ( angr1 > 0.0 ) {
		psi = HALFPI - _dggrid.dgg[igqlat-1].grid[k];
	    } else {
		psi = HALFPI + _dggrid.dgg[igqlat-1].grid[k];
	    }
	    _dggrid.dgg[(*ixtms)-1].grid[k] = pow ( tan ( psi / 2. ), (*cone) ) / sin ( psi );
	    _dggrid.dgg[(*iytms)-1].grid[k] = _dggrid.dgg[(*ixtms)-1].grid[k];
	}
	*tgdx = ( RADIUS / (*cone) ) * dix / (float)( mx - 1 );
	*tgdy = ( RADIUS / (*cone) ) * diy / (float)( my - 1 );
    } else {
	*iret = -16;
	strcpy ( _dgerr.errst, gprj );
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
    *tgdx = G_ABS ( *tgdx );
    *tgdy = G_ABS ( *tgdy );

    return;
}
