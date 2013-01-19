#include "dg.h"

#define RPOLE		( HALFPI - DTR )

void dg_dmsf ( int *iret )
/************************************************************************
 * dg_dmsf								*
 *									*
 * This subroutine computes computes the map scale factor derivative	*
 * coefficients and stores them in the internal grids to which IYMSDX	*
 * and IXMSDY point:							*
 *									*
 *               ymsdx = (mx/my)*[d(my)/dx]				*
 *	  	 xmsdy = (my/mx)*[d(mx)/dy]				*
 *									*
 * where mx and my represent the scale factors along x and y,		*
 * respectively.							*
 *									*
 * dg_dmsf ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-16 = grid projection error	*
 **									*
 * Log:									*
 * K. Brill/HPC		 5/02						*
 * K. Brill/HPC		 3/04	Compute analytic scl fctr derivatives	*
 *				in Q coordinates if anglr1 or anglr3	*
 *				indicate rotation			*
 * K. Brill/HPC		 3/04	Remove check for missing LAT/LON	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float dx2, dy2, rmscl, qmscl;
    int swchxy, lymdx, lxmdy, isrot;
    int igqlat, igqlon, ix, iy, i, j, kxm1, kym1, ip1, im1, indx, im,
        ip, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check to see if mapscale factors need to be computed.
     */
    dg_cndg ( "MX_MY_DMY_DX", &_mapscl.iymsdx, &lymdx, iret );
    if ( *iret != 0 ) return;
    dg_cndg ( "MY_MX_DMX_DY", &_mapscl.ixmsdy, &lxmdy, iret );
    if ( *iret != 0 ) return;
    if ( lymdx == G_TRUE && lxmdy == G_TRUE ) return;

    /*
     * Compute the latitude and longitude at all grid points.
     */
    dg_ltln ( iret );
    if ( *iret != 0 ) return;

    /*
     * Compute the map scale factors.
     */
    dg_mscl ( iret );
    if ( *iret != 0 ) return;

    /*
     * If the grid is rotated, Q coordinates differ from LAT/LON.
     * Lat/Lon is only needed for the cylindrical case; all others
     * are computed by finite differences.
     */
    isrot = G_FALSE;
    if ( strcmp ( _dgfile.cprj, "CED" ) == 0 ) {
	if ( !G_DIFFT(_dgfile.anglr1, 0.0F, GDIFFD) || !G_DIFFT(_dgfile.anglr3, 0.0F, GDIFFD) ) {
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
	    for ( ix = 1; ix <= _dgfile.kxd; ix++ ) {
		_dggrid.dgg[igqlat-1].grid[i] = (float)ix;
		_dggrid.dgg[igqlon-1].grid[i] = (float)iy;
		i++;
	    }
	}

	gtrans ( sys_G, sys_Q, &_dgfile.kxyd,
	    _dggrid.dgg[igqlat-1].grid, _dggrid.dgg[igqlon-1].grid,
	    _dggrid.dgg[igqlat-1].grid, _dggrid.dgg[igqlon-1].grid, &ier,
	    strlen(sys_G), strlen(sys_Q) );
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

    /*
     * The CED case can be done analytically:
     */
    if ( strcmp ( _dgfile.cprj, "CED" ) == 0 && swchxy == G_FALSE ) {
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    _dggrid.dgg[_mapscl.iymsdx-1].grid[i] = 0.0;
	    if ( G_ABS ( _dggrid.dgg[igqlat-1].grid[i] ) >= RPOLE ) {
		_dggrid.dgg[_mapscl.ixmsdy-1].grid[i] = 0.;
	    } else {
		_dggrid.dgg[_mapscl.ixmsdy-1].grid[i] = 
		    tan ( _dggrid.dgg[igqlat-1].grid[i] ) / RADIUS;
	    }
	}

	/*
	 * Free local scratch grids.
	 */
        if ( isrot == G_TRUE ) {
            dg_frig ( &igqlat, &ier );
            dg_frig ( &igqlon, &ier );
        }

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
     * Compute the partial derivative of my with respect to x and multiply
     * it by (mx/my).
     */
    dx2 = 2. * _mapscl.gddx;
    kxm1 = _dgfile.kxd - 1;

    /*
     * Loop over all grid rows.
     */
    for ( j = 1; j <= _dgfile.kyd; j++ ) {
        /*	   
         * Loop over interior grid points in row j.
         */
        for ( i = 2; i <= kxm1; i++ ) {
	    ip1 = i + 1 + (j - 1) * _dgfile.kxd;
	    im1 = ip1 - 2;
	    indx = ip1 - 1;
	    _dggrid.dgg[_mapscl.iymsdx-1].grid[indx-1] =
	        ( _dggrid.dgg[_mapscl.iymscl-1].grid[ip1-1] -
	          _dggrid.dgg[_mapscl.iymscl-1].grid[im1-1] ) *
	          _dggrid.dgg[_mapscl.ixmscl-1].grid[indx-1] /
	        ( _dggrid.dgg[_mapscl.iymscl-1].grid[indx-1] * dx2 );
	}

        /*
         * Compute one-sided difference at the beginning of row j.
         */
        im1 = 1 + (j - 1) * _dgfile.kxd;
	ip1 = im1 + 1;
	rmscl  = .5 * ( _dggrid.dgg[_mapscl.ixmscl-1].grid[im1-1] +
                        _dggrid.dgg[_mapscl.ixmscl-1].grid[ip1-1] );
	qmscl  = .5 * ( _dggrid.dgg[_mapscl.iymscl-1].grid[im1-1] +
                        _dggrid.dgg[_mapscl.iymscl-1].grid[ip1-1] );
	_dggrid.dgg[_mapscl.iymsdx-1].grid[im1-1] =
            ( _dggrid.dgg[_mapscl.iymscl-1].grid[ip1-1] -
	      _dggrid.dgg[_mapscl.iymscl-1].grid[im1-1] ) *
	    rmscl / ( qmscl * _mapscl.gddx );

        /*
         * Compute one-sided difference at the end of row j.
         */
	ip1 = j * _dgfile.kxd;
	im1 = ip1 - 1;
	rmscl  = .5 * ( _dggrid.dgg[_mapscl.ixmscl-1].grid[im1-1] +
		        _dggrid.dgg[_mapscl.ixmscl-1].grid[ip1-1] );
	qmscl  = .5 * ( _dggrid.dgg[_mapscl.iymscl-1].grid[im1-1] +
    		        _dggrid.dgg[_mapscl.iymscl-1].grid[ip1-1] );
	_dggrid.dgg[_mapscl.iymsdx-1].grid[ip1-1] =
            ( _dggrid.dgg[_mapscl.iymscl-1].grid[ip1-1] -
	      _dggrid.dgg[_mapscl.iymscl-1].grid[im1-1] ) *
	    rmscl / ( qmscl * _mapscl.gddx );
    }

    /*
     * Compute the partial derivative of mx with respect to y and multiply
     * by (my/mx).
     */
    dy2  = 2. * _mapscl.gddy;
    kym1 = _dgfile.kyd - 1;

    /*
     * Compute derivative on internal rows.
     */
    for ( j = 2; j <= kym1; j++ ) {
	for ( i = 1; i <= _dgfile.kxd; i++ ) {
	    im = i + (j - 2) * _dgfile.kxd;
	    ip = im + 2 * _dgfile.kxd;
	    indx = im + _dgfile.kxd;

	    _dggrid.dgg[_mapscl.ixmsdy-1].grid[indx-1] = 
	        ( _dggrid.dgg[_mapscl.ixmscl-1].grid[ip-1] -
	          _dggrid.dgg[_mapscl.ixmscl-1].grid[im-1] ) *
	          _dggrid.dgg[_mapscl.iymscl-1].grid[indx-1] /
	        ( _dggrid.dgg[_mapscl.ixmscl-1].grid[indx-1] * dy2 );
	}
    }

    /*
     * Compute one-sided derivatives along bottom row.
     */
    for ( i = 1; i <= _dgfile.kxd; i++ ) {
        im = i;
        ip = i + _dgfile.kxd;
        indx = i;
        rmscl  = .5 * ( _dggrid.dgg[_mapscl.iymscl-1].grid[ip-1] +
     		        _dggrid.dgg[_mapscl.iymscl-1].grid[im-1] );
        qmscl  = .5 * ( _dggrid.dgg[_mapscl.ixmscl-1].grid[ip-1] +
    		        _dggrid.dgg[_mapscl.ixmscl-1].grid[im-1] );
        _dggrid.dgg[_mapscl.ixmsdy-1].grid[indx-1] = 
            ( _dggrid.dgg[_mapscl.ixmscl-1].grid[ip-1] -
	      _dggrid.dgg[_mapscl.ixmscl-1].grid[im-1] ) *
 	    rmscl / ( qmscl * _mapscl.gddy );

        /*
         * Compute one-sided derivative along top row.
         */  
        im = i + (kym1 - 1) * _dgfile.kxd;
        ip = im + _dgfile.kxd;
        indx = ip;
        rmscl  = .5 * ( _dggrid.dgg[_mapscl.iymscl-1].grid[ip-1] +
       		        _dggrid.dgg[_mapscl.iymscl-1].grid[im-1] );
        qmscl  = .5 * ( _dggrid.dgg[_mapscl.ixmscl-1].grid[ip-1] +
    		        _dggrid.dgg[_mapscl.ixmscl-1].grid[im-1] );
	_dggrid.dgg[_mapscl.ixmsdy-1].grid[indx-1] =
            ( _dggrid.dgg[_mapscl.ixmscl-1].grid[ip-1] -
	      _dggrid.dgg[_mapscl.ixmscl-1].grid[im-1] ) *
	    rmscl / ( qmscl * _mapscl.gddy );
    }

    return;
}
