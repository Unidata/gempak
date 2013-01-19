#include "dg.h"

void dg_trot ( int *iret )
/************************************************************************
 * dg_trot								*
 *									*
 * This subroutine computes the elements of the rotation matrix		*
 * and its inverse to convert from transfer grid relative to north	*
 * relative coordinates and vice versa.  These elements are the sines   *
 * and cosines of the rotation angle and are stored in internal grids 	*
 * to which ISNROT and ICSROT point.					*
 *									*
 * dg_trot ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-16 = grid projection error	*
 **									*
 * Log:									*
 * K. Brill/HPC		 3/04	Created from DG_GROT			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char gprj[5];
    float agln2, angr1, angr2, angr3, tgdx, tgdy, conec, rcone, angtst,
        theta, phin, phin2, rlmn, rix, riy, sign, vr2nx, vr2ny, prdmag;
    int lcos, lsin;
    int nc, mx, my, ixtms, iytms, icase, one, navsz, ix, iy, i, ier, ierr;
/*----------------------------------------------------------------------*/
    *iret = 0;
    one = 1;

    /*
     * Check to see if the matrix elements already exist.
     */
    dg_cndg ( "TG_ICSROT", &_hintrp.icsrot, &lcos, iret );
    if ( *iret != 0 ) return;
    dg_cndg ( "TG_ISNROT", &_hintrp.isnrot, &lsin, iret );
    if ( *iret != 0 ) return;
    if ( lsin == G_TRUE && lcos == G_TRUE ) return;

    /*
     * Compute the lat/lon at all grid points.
     */
    dg_ltln ( iret );
    if ( *iret != 0 ) return;

    /*
     * Set the grid navigation of the transfer grid.
     */
    cst_itos ( (int *)(&_hintrp.tfrnav[1]), 1, &nc, gprj, &ier );
    cst_rmbl ( gprj, gprj, &nc, &ier );
    mx = G_NINT ( _hintrp.tfrnav[4] );
    my = G_NINT ( _hintrp.tfrnav[5] );
    if ( _hintrp.adcltg == G_TRUE ) {
	mx += 1;
	agln2 = _hintrp.tfrnav[7];
    } else {
	agln2 = _hintrp.tfrnav[9];
    }
    angr1 = _hintrp.tfrnav[10] * DTR;
    angr2 = _hintrp.tfrnav[11] * DTR;
    angr3 = _hintrp.tfrnav[12] * DTR;
    gsgprj ( gprj, &_hintrp.tfrnav[10], &_hintrp.tfrnav[11],
             &_hintrp.tfrnav[12], &mx, &my, &_hintrp.tfrnav[6],
	     &_hintrp.tfrnav[7], &_hintrp.tfrnav[8], &agln2, &ier,
	     strlen(gprj) );
    if ( ier != 0 ) {
	er_wmsg ( "GEMPLT", &ier, " ", &ierr, strlen("GEMPLT"), strlen(" ") );
	*iret = -67;
	return;
    }

    /*
     * Call DG_TSCL to compute reference grid map scale factors,
     * constant of the cone, and grid spacing.
     */
    dg_tscl ( &ixtms, &iytms, &tgdx, &tgdy, &conec, iret );
    if ( *iret != 0 ) return;

    icase = 0;
    if ( strcmp ( gprj, "CED" ) == 0 || strcmp ( gprj, "MER" ) == 0 ||
	 strcmp ( gprj, "MCD" ) == 0 ) {
	if ( G_DIFFT(angr1, 0.0F, GDIFFD) && G_DIFFT(angr3, 0.0F, GDIFFD) ) {
	    icase = 1;
	}
    } else {
	angtst = G_ABS ( G_ABS (angr1) - HALFPI );
	if ( angtst < 1.E-5 && G_DIFFT(angr3, 0.0F, GDIFFD) ) {
	    icase = 2;

	    /*
	     * Get cone constant, checking for southern hemisphere
	     */
	    if ( ( strcmp ( gprj, "STR" ) == 0 && G_DIFF(angr1, (-HALFPI)) ) ||
		 ( strcmp ( gprj, "SCC" ) == 0 ) ) {
		rcone = -conec;
	    } else {
		rcone = conec;
	    }
	}
    }

    i = 0;
    for ( iy = 1; iy <=_dgfile. kyd; iy++ ) {
	for ( ix = 1; ix <= _dgfile.kxd; ix++ ) {
	    if ( ERMISS ( _dggrid.dgg[_dgfile.idglon-1].grid[i] ) ) {
		_dggrid.dgg[_hintrp.isnrot-1].grid[i] = RMISSD;
		_dggrid.dgg[_hintrp.icsrot-1].grid[i] = RMISSD;
	    } else if ( icase == 1 ) {
		/*
		 * No rotation needed for unrotated cylindrical grids.
		 */
		_dggrid.dgg[_hintrp.icsrot-1].grid[i] = 1.;
		_dggrid.dgg[_hintrp.isnrot-1].grid[i] = 0.;
	    } else if ( icase == 2 ) {
		/*
		 * Simple rotation for unrotated conic and
		 * stereographic grids.
		 */
		theta = ( _dggrid.dgg[_dgfile.idglon-1].grid[i] - angr2 ) * rcone;
		_dggrid.dgg[_hintrp.icsrot-1].grid[i] = cos ( theta );
		_dggrid.dgg[_hintrp.isnrot-1].grid[i] = sin ( theta );
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
		phin = _dggrid.dgg[_dgfile.idglat-1].grid[i] * RTD;
		rlmn = _dggrid.dgg[_dgfile.idglon-1].grid[i] * RTD;
		gtrans ( sys_M, sys_G, &one, &phin, &rlmn, &rix, &riy, &ier,
		         strlen(sys_M), strlen(sys_G) );
		phin2 = phin + .05;
		if ( phin2 > 90.0 ) {
		    phin2 = phin - .05;
		    sign = -1.;
		} else {
		    sign = 1.;
		}
		gtrans ( sys_M, sys_G, &one, &phin2, &rlmn, &vr2nx, &vr2ny,
		         &ier, strlen(sys_M), strlen(sys_G) );
		vr2nx = ( vr2nx - rix ) * tgdx / _dggrid.dgg[ixtms-1].grid[i];
		vr2ny = ( vr2ny - riy ) * tgdy / _dggrid.dgg[iytms-1].grid[i];
		prdmag = sqrt ( vr2nx * vr2nx + vr2ny * vr2ny );

		/*
		 * Use DOT product to get COS (rotation angle).
		 */
		_dggrid.dgg[_hintrp.icsrot-1].grid[i] = sign * vr2ny / prdmag;

		/*
		 * Use CROSS product to get SIN (rotation angle).
		 * The CROSS required is VR2G X VR2N.
		 */
		_dggrid.dgg[_hintrp.isnrot-1].grid[i] = - sign * vr2nx / prdmag;
	    }
	    i++;
	}
    }

    /*
     * Reset navigation to that of internal grid.
     */
    navsz = LLNNAV;
    grc_snav ( &navsz, _dgfile.snav, &ier );

    /*
     * Free the scale factor grids.
     */
    dg_frig ( &ixtms, &ier );
    dg_frig ( &iytms, &ier );

    return;
}
