#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern 	SpcInfo_t      spcinfo;

void vfavcd ( char *locnam, char *type, float plat, float plon, 
				char *disdir, char *stn, int *iret )
/************************************************************************
 * vfavcd                                                    		*
 *                                                                      *
 * This function creates the distance, direction and station text       *
 * strings for a given point.						*
 *                                                                      *
 * void vfavcd ( locnam, type, plat, plon, disdir, stn, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*locnam		char	Locator type				*
 *	*type		char	Type of text product			*
 *      plat		float	Input latitude				*
 *      plon		float	Input longitude				*
 *									*
 * Output parameters:                                                   *
 *	*disdir	char		Returned distance and direction string	*
 *	*stn	char		Returned station string			*
 *	*iret	int		Return value				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC         03/00   Created					*
 * A. Hardy/GSC         04/00   Added WCP if test			*
 * A. Hardy/GSC         12/00   Removed '&' from iret in clo_init       *
 ************************************************************************/
{
	int	npt, idist, nclose, ier, icmp, npx, pos;
	float	vlat, vlon, dir, dist;
	char	cmpdir[4], idx[20];
/*---------------------------------------------------------------------*/

	*iret = 0;

	npx =1;
	npt = 1;

	/*
	 *  Consider all VOR points
	 */

        nclose = 1;
	clo_init ( iret );
        clo_tclosest( locnam, plat, plon, nclose, &ier);
	clo_tgltln( locnam, 1, &npt, &vlat, &vlon, &ier );

	clo_dist ( &plat, &plon, &npx, &vlat, &vlon, &dist, &ier );

	/*
	 *  Distance must be in nautical miles
	 */

	idist = G_NINT( dist * M2NM );

	clo_direct ( &plat, &plon, &vlat, &vlon, &dir, &ier );
        clo_compass ( &dir, cmpdir, &icmp, &ier );
	clo_tgid( locnam, 1, sizeof(idx), &npt, idx, &ier );

	if ( idist == 0 ){
	    strcpy( disdir, "\0" );
	}
	else {
	    if ( strcmp ( locnam, "VOR") == 0 ) {
	        if ( strcmp ( type, "AWN") == 0 ) {
	            sprintf( disdir, "..%d%s", idist, cmpdir );
	            cst_rmst(idx,";", &pos, stn, &ier);
		}
		else if ( ( strcmp ( type, "PWN") == 0 ) ||
		          ( strcmp ( type, "WCP") == 0 ) ) {
	            sprintf( disdir, "%d %s", idist, cmpdir );
		    strcpy (stn, idx);
		}
	    }
	}
}
