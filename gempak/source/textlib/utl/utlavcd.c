#include "geminc.h"
#include "gemprm.h"

void utl_avcd ( char *locnam, float *plat, float *plon, 
		char *disdir, char *stn, int *iret )
/************************************************************************
 * utl_avcd                                                    		*
 *                                                                      *
 * This function creates the distance, direction and station text       *
 * strings for a given point.						*
 *                                                                      *
 * void utl_avcd ( locnam, plat, plon, disdir, stn, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*locnam		char		Locator type			*
 *      *plat		float		Input latitude			*
 *      *plon		float		Input longitude			*
 *									*
 * Output parameters:                                                   *
 *	*disdir		char		Returned dist. and dir. string	*
 *	*stn		char		Returned station string		*
 *	*iret		int		Return value			*
 *					   -3 = Latitude not in range	*
 *					   -4 = Longitude not in range	*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 5/03	Copied from VFAVCD			*
 * A. Hardy/NCEP	 8/03	make locnam all caps; improved check	*
 *				for proper lat/lon values		* 
 ************************************************************************/
{
	int	npt, idist, nclose, ier, icmp, npx, pos;
	float	vlat, vlon, dir, dist;
	char	cmpdir[4], idx[20];
	Boolean good;
/*---------------------------------------------------------------------*/

	*iret = 0;
	good = False;
	npx = 1;
	npt = 1;

       /*
        * Check for valid inputs.
	*/

	if ( ( *plat >= -90.0 ) && ( *plat <= 90.0 ) ) {
	    good = True;
	}
	else {
	    *iret = -3;
	    return;
	}

	if ( ( good ) && ( *plon >= -180.0 ) && ( *plon <= 180.0 ) ) {
	    good = True;
	}
	else {
	    *iret = -4;
	    return;
	}
	/*
	 *  Consider all VOR points
	 */

	if ( good ) {
            nclose = 1;
	    clo_init ( &ier );
	    cst_lcuc ( locnam, locnam, &ier );

            clo_tclosest( locnam, *plat, *plon, nclose, &ier);
	    clo_tgltln( locnam, 1, &npt, &vlat, &vlon, &ier );

	    clo_dist ( plat, plon, &npx, &vlat, &vlon, &dist, &ier );

	    /*
	     *  Distance must be in nautical miles
	     */

	    idist = G_NINT( dist * M2NM );

	    clo_direct ( plat, plon, &vlat, &vlon, &dir, &ier );
            clo_compass ( &dir, cmpdir, &icmp, &ier );
	    clo_tgid( locnam, 1, sizeof(idx), &npt, idx, &ier );
	    cst_rmst(idx,";", &pos, stn, &ier);

	    if ( idist == 0 ){
	        cst_ncpy( disdir, CHNULL, 1, &ier );
	    }
	    else {
	        sprintf( disdir, "%d %s", idist, cmpdir );
	    }
        }
}
