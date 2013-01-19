#include "dv.h"

void dv_gcwv ( int *iret )
/************************************************************************
 * dv_gcwv								*
 *									*
 * This routine computes, for every gridpoint on a grid, a unit vector, *
 * in the direction away from the home point and toward its antipodal	*
 * point, and along a great circle arc. All land gridpoints and all     *
 * water gridpoints whose view of the home point is blocked by land     *
 * will have their vector components set to RMISSD.                     *
 *									*
 * dv_gcwv ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * G. McFadden/SAIC	 9/09						*
 ***********************************************************************/
{
    int		i, ier, kxd, kyd, ksub1, ksub2, zero=0;
    int		iazst, numu, numv, ilat, ilon, msk, one=1;
    int		io, jo;
    int		igx1, igy1, igx2, igy2;
    int		ihomept;	
    int		corner1, corner2, corner3, corner4;
    int		idglat, idglon, ishdw;

    float	*grazst, *gru, *grv, *grlat, *grlon, *grmask;
    float	*gptlat, *gptlon, *grshdw;
    float	ptlat, ptlon, gplat, gplon, rio, rjo;
    float	nplat, nplon, rgx, rgy;
    float	mag = 1.0F;
    float	dir, deldist = 10000.0F, distance;

/*---------------------------------------------------------------------*/

    *iret = 0;
    dg_ssub ( iret );

    /*
     * Check if the navigation parameters have been computed.
     */
    dg_ltln ( iret );
    if ( *iret != 0 ) return;

    /*
     * Get the grid numbers for the grids of latitudes and
     * longitudes of the gridpoints, then get the actual
     * grids of latitudes and longitudes themselves.
     */
    dg_iget ( "IDGLAT", &one, &idglat, iret );
    dg_iget ( "IDGLON", &one, &idglon, iret );
    dg_getg( &idglat, &grlat, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg( &idglon, &grlon, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get grid numbers for user input, which is the home point
     * and mask.
     */
    dg_gets ( &ilat, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &ilon, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &msk, iret );
    if ( *iret != 0 ) return;

    /*
     * Allocate space for new grids and get new grid numbers.
     */
    dg_nxts  ( &iazst, iret );
    if ( *iret != 0 ) return;
    dg_nxtv  ( &numu, &numv, iret );
    if ( *iret != 0 ) return;
    
    /*
     * Calculate the azimuth angle at each grid point 
     * measured from north to the great circle arc.
     */
    dg_azst ( &ilat, &ilon, &iazst, iret );
    if ( *iret != 0 ) return;

    /*
     * Get the output vector component and angle (direction)	
     * grids and mask grid so that we can work with them.
     */
    dg_getg ( &iazst, &grazst, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &msk, &grmask, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get the latitude and longitude of the home point.
     */
    dg_getg ( &ilat, &gptlat, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &ilon, &gptlon, &kxd, &kyd, &ksub1, &ksub2, iret );
    ptlat = gptlat[ksub1];
    ptlon = gptlon[ksub1];
        
    /*
     * Get the grid indices of the home point. If the home point
     * is not on the grid, complain and exit.
     */
    gtrans ( sys_M, sys_G, &one, &ptlat, &ptlon, &rio, &rjo, &ier,
	     strlen(sys_M), strlen(sys_G) );

    io = (int) rio;
    jo = (int) rjo;
    ihomept = ( jo - 1 ) * kxd + io - 1;
    if ( ! ( ksub1 <= ihomept && ihomept < ksub2 ) ) {
	ier = -74;
	return;
    }

    /*
     * Initialize the array used to indicate whether or not 
     * water gridpoints are in a land shadow.
     */
    dg_nxts  ( &ishdw, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &ishdw, &grshdw, &kxd, &kyd, &ksub1, &ksub2, iret );
    for ( i = ksub1 ; i < ksub2; i++ ) {
	grshdw[i] = RMISSD;
    }

    /*
     * Compute the components of the unit vectors that we need.
     */
    for ( i = ksub1-1; i < ksub2; i++ ) {
	if ( ! ERMISS ( grazst[i] ) ) {
	    if ( ERMISS ( grmask[i] ) ) {
		gru[i] = RMISSD; 
		grv[i] = RMISSD;
	    }
	    else {
		gru[i] = -sin ( grazst[i] ) * mag; 
		grv[i] = -cos ( grazst[i] ) * mag;    
	    }
	} 
	else {
	    gru[i] = RMISSD;
	    grv[i] = RMISSD;
	}
    }

    /*
     * Process entire grid
     */
    for ( i = ksub1-1; i < ksub2; i++ ) {

        /*
         * Only check water gridpoints 
	 */
	if ( ! ERMISS (grmask[i]) ) {

	    /*
	     * Get the lat,lon of this water gridoint
	     */
	    gplat = grlat[i] * RTD;
	    gplon = grlon[i] * RTD;

	    /*
	     * Backtrack from this water gridpoint to the home point
	     */
	    while ( 1 ) {

		/*
		 * Get the distance from the current lat,lon location
		 * to the home point. If the distance found is less
		 * than deldist, you're home, terminate backtrack,
		 * because this gridpoint is not in a land shadow.      
		 */
		clo_dist( &gplat, &gplon, &one, &ptlat, &ptlon,
		       	  &distance, iret );
		if ( distance <= deldist ) {
		    break;
		}
        
		/*
		 * Not home yet... get the direction (angle) from the
		 * current lat,lon location to the home point
		 */
		clo_direct( &ptlat, &ptlon, &gplat, &gplon, &dir, iret );

		/*
		 * Get the lat,lon location of the next earth surface
		 * point closest to the home point
		 */
		clo_dltln( &gplat, &gplon, &deldist, &dir,
		           &nplat, &nplon, iret );

		/*
		 * Generate the corner point grid indices of the
		 * corner points containing this location 
		 */
		gtrans ( sys_M, sys_G, &one, &nplat, &nplon,
		         &rgx, &rgy, &ier,
			 strlen(sys_M), strlen(sys_G) );
		igx1 = (int) rgx;
		igy1 = (int) rgy;
		igx2 =  igx1 + 1;
		igy2 =  igy1 + 1;

		/*
		 * If all 4 corner points are on the grid, and one or
		 * more corner points has a RMISSD value, and all of
		 * the corner RMISSD values are land points, then set
		 * this gridpoint's vector components to RMISSD
		 * (because it's in a land shadow). Next, explicitly
		 * indicate that this gridpoint is in a land shadow
		 * and then go on to the next gridpoint.
		 */
		corner1 = ( igy1 - 1 ) * kxd + igx1 - 1;
		corner2 = ( igy1 - 1 ) * kxd + igx2 - 1;
		corner3 = ( igy2 - 1 ) * kxd + igx1 - 1;
		corner4 = ( igy2 - 1 ) * kxd + igx2 - 1;

		if ( ( corner1 >= ksub1-1 && corner1 < ksub2 ) &&
		     ( corner2 >= ksub1-1 && corner2 < ksub2 ) &&
		     ( corner3 >= ksub1-1 && corner3 < ksub2 ) &&
		     ( corner4 >= ksub1-1 && corner4 < ksub2 ) &&
		     ( (ERMISS(gru[corner1]) && ERMISS(grshdw[corner1])) ||
		       (ERMISS(gru[corner2]) && ERMISS(grshdw[corner2])) ||
		       (ERMISS(gru[corner3]) && ERMISS(grshdw[corner3])) ||
		       (ERMISS(gru[corner4]) && ERMISS(grshdw[corner4])) ) ) { 
		    gru[i] = RMISSD;
		    grv[i] = RMISSD;
		    grshdw[i] = 1.0F;
		    break;
		}
		/*
		 * This gripoint in not in a land shadow.
		 * Continue the backtracking.	
		 */
		gplat = nplat;
	       	gplon = nplon;
	    }
	}
    }

    /*
     * Return them as a vector. Make a name of the form 'GCWV'//S1//S2
     * and update the headers; update the stack.
     */
    dg_updv ( "GCWV", &numu, &numv, &numu, &numv, iret );
    dg_putv ( &numu, &numv, iret );
    dg_esub ( &numu, &numv, &zero, &zero, &ier );

    if ( ier != 0 ) *iret = ier;

    return;
}
