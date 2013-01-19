#include "dg.h"

void dg_grot ( int *iret )
/************************************************************************
 * dg_grot								*
 *									*
 * This subroutine computes the elements of the rotation matrix		*
 * and its inverse to convert from grid relative to north relative	*
 * coordinates and vice versa.  These elements are the sines and	*
 * cosines of the rotation angle and are stored in internal grids to	*
 * which IRTSIN and IRTCOS point.					*
 *									*
 * dg_grot ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-16 = grid projection error	*
 **									*
 * Log:									*
 * K. Brill/GSC		 9/89						*
 * K. Brill/EMC		 3/96	Generalize for arbitrary projections	*
 * K. Brill/HPC		 5/02	Store sines and cosines in internal grd *
 * R. Tian/SAIC		 5/05	Swap the order of checking icase = 2 	*
 *				and grid line conincide with longitude	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float angtst, rcone, theta, phin, rlmn, sign, vr2nx, vr2ny, prdmag;
    int lcos, lsin;
    int icase, one, ix, iy, i, isn, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    one = 1;

    /*
     * Check to see if the matrix elements already exist.
     */
    dg_cndg ( "WIND_ROT_COS", &_dgrtwd.irtcos, &lcos, iret );
    if ( *iret != 0 ) return;
    dg_cndg ( "WIND_ROT_SIN", &_dgrtwd.irtsin, &lsin, iret );
    if ( *iret != 0 ) return;

    if ( lsin == G_TRUE && lcos == G_TRUE ) return;

    /*
     * Compute the lat/lon at all grid points.
     */
    dg_ltln ( iret );
    if ( *iret != 0 ) return;

    /*
     * Call DG_MSCL GDDX, GDDY, and the map scale factors,
     * XMSCL and YMSCL.
     */
    dg_mscl ( iret );
    if ( *iret != 0 ) return;

    icase = 0;
    if ( strcmp ( _dgfile.cprj, "CED" ) == 0 ||
         strcmp ( _dgfile.cprj, "MER" ) == 0 ||
	 strcmp ( _dgfile.cprj, "MCD" ) == 0 ) {
	if ( G_DIFFT(_dgfile.anglr1, 0.0F, GDIFFD) &&
	     G_DIFFT(_dgfile.anglr3, 0.0F, GDIFFD) ) {
	    icase = 1;
	}
    } else {
	angtst = G_ABS ( G_ABS ( _dgfile.anglr1 ) - HALFPI );
	if ( angtst < 1.E-5 && G_DIFFT(_dgfile.anglr3, 0.0F, GDIFFD) ) {
	    icase = 2;

	    /*
	     * Get cone constant, checking for southern hemisphere
	     */
	    if ( ( strcmp ( _dgfile.cprj, "STR" ) == 0 &&
	         G_DIFF(_dgfile.anglr1, (-HALFPI)) ) ||
		 ( strcmp ( _dgfile.cprj, "SCC" ) == 0 ) ) {
		rcone = -_dgfile.concon;
	    } else {
		rcone = _dgfile.concon;
	    }
	}
    }

    i = 0;
    isn = 1;
    for ( iy = 1; iy <= _dgfile.kyd; iy++ ) {
	if ( iy == _dgfile.kyd ) isn = -1;
	for ( ix = 1; ix <= _dgfile.kxd; ix++ ) {
	    if ( ERMISS ( _dggrid.dgg[_dgfile.idglon-1].grid[i] ) ) {
		_dggrid.dgg[_dgrtwd.irtsin-1].grid[i] = RMISSD;
		_dggrid.dgg[_dgrtwd.irtcos-1].grid[i] = RMISSD;
	    } else if ( icase == 1 ) {
		/*
		 * No rotation needed for unrotated cylindrical grids.
		 */
		_dggrid.dgg[_dgrtwd.irtcos-1].grid[i] = 1.;
		_dggrid.dgg[_dgrtwd.irtsin-1].grid[i] = 0.;
	    } else if ( icase == 2 ) {
		/*
		 * Simple rotation for unrotated conic and
		 * stereographic grids.
		 */
		theta = ( _dggrid.dgg[_dgfile.idglon-1].grid[i] - _dgfile.anglr2 ) * rcone;
		_dggrid.dgg[_dgrtwd.irtcos-1].grid[i] = cos ( theta );
		_dggrid.dgg[_dgrtwd.irtsin-1].grid[i] = sin ( theta );
	    } else if ( G_ABS ( _dggrid.dgg[_dgfile.idglon-1].grid[i] -
	        _dggrid.dgg[_dgfile.idglon-1].grid[i+isn*_dgfile.kxd] ) < 1.E-5 ) {
		/*
		 * Grid lines and longitude lines coincide; so, no
		 * rotation is needed.
		 */
		_dggrid.dgg[_dgrtwd.irtcos-1].grid[i] = 1.;
		_dggrid.dgg[_dgrtwd.irtsin-1].grid[i] = 0.;
	    } else {
		/*
		 * Do GENERAL rotation:
		 *
		 * Form two unit vectors in grid coordinates-- one
		 * along constant longitude (VR2N), the other along
		 * the grid y direction (VR2G).  Use unit vector (0,1)
		 * for the latter.
		 *
		 * Note:  The choice of .05 degrees is arbitrary.
		 */
		phin = _dggrid.dgg[_dgfile.idglat-1].grid[i] * RTD + .05;
		rlmn = _dggrid.dgg[_dgfile.idglon-1].grid[i] * RTD;
		if ( phin > 90.0 ) {
		    phin = _dggrid.dgg[_dgfile.idglat-1].grid[i] * RTD - .05;
		    sign = -1.;
		} else {
		    sign = 1.;
		}
		gtrans ( sys_M, sys_G, &one, &phin, &rlmn, &vr2nx, &vr2ny, &ier,
							strlen(sys_M), strlen(sys_G) );
		vr2nx = ( vr2nx - (float)ix ) * _mapscl.gddx / _dggrid.dgg[_mapscl.ixmscl-1].grid[i];
		vr2ny = ( vr2ny - (float)iy ) * _mapscl.gddy / _dggrid.dgg[_mapscl.iymscl-1].grid[i];
		prdmag = sqrt ( vr2nx * vr2nx + vr2ny * vr2ny );

		/*
		 * Use DOT product to get COS (rotation angle).
		 */
		_dggrid.dgg[_dgrtwd.irtcos-1].grid[i] = sign * vr2ny / prdmag;

		/*
		 * Use CROSS product to get SIN (rotation angle).
		 * The CROSS required is VR2G X VR2N.
		 */
		_dggrid.dgg[_dgrtwd.irtsin-1].grid[i] = - sign * vr2nx / prdmag;
	    }
	    i++;
	}
    }

    return;
}
