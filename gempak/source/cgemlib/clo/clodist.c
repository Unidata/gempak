#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

void clo_dist ( float *lat1, float *lon1, int *np, float lat2[], 
				float lon2[], float dist[], int *iret )
/************************************************************************
 * clo_dist                                                    		*
 *                                                                      *
 * This function calculates the great-circle distance between two	*
 * Earth points.							*
 *                                                                      *
 * clo_dist ( lat1, lon1, np, lat2, lon2, dist, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*lat1		float		Point #1 Latitude		*
 *	*lon1		float		Point #1 Longitude		*
 *	*np		int		Number of points other than #1	*
 *	lat2[np]	float		Latitude array	 		*
 *	lon2[np]	float		Longitude array			*
 *									*
 * Output parameters:                                                   *
 *	dist[np]	float		Distances between points(meters)*
 *	*iret		int		Return value			*
 *					=  0 - OK			*
 *					= -1 - error			*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 4/98	Create					*
 * S. Jacobs/NCEP	10/98	Simplified calculation			*
 * D.W.Plummer/NCEP	 6/99	Add check for invalid acos input	*
 * T. Piper/GSC		09/99	Modified to use THETA macro		*
 * M. Li/GSC		10/99	Added checks for invalid lat/lon input;	*
 *                              Modified to be called by FORTRAN	*
 * M. Li/GSC		10/99	Calculated distances between multipoints*
 * S. Jacobs/NCEP	12/99	Removed THETA macro, added error checks	*
 * M. Li/SAIC		11/01	Removed the check for small delta	*
 ***********************************************************************/
{
    int     i;
    double  rlat1, rlat2, rlon1, rlon2, dlon, val;

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Check the input latitude/longitude for out of range.
     */
    if  ( *lat1 < -90.0F || *lat1 > 90.0F || *lon1 < -180.0F 
					|| *lon1 >  180.0F ) {
        *iret = -1;
        for ( i = 0; i < *np; i++ ) dist[i] = RMISSD;
        return;
    }

    for ( i = 0; i < *np; i++ ) {

        /*  Check the input latitude for out of range. */
        if  ( lat2[i] < -90.0F || lat2[i] > 90.0F )
            dist[i] = RMISSD;
 
        /*  Check the input longitude for out of range.*/      
        else if ( lon2[i] < -180.0F || lon2[i] > 180.0F )
            dist[i] = RMISSD;

        else {
	    rlat1 = DTR * (double)(*lat1);
	    rlat2 = DTR * (double)(lat2[i]);
	    rlon1 = DTR * (double)(*lon1);
	    rlon2 = DTR * (double)(lon2[i]);

	    dlon = rlon1 - rlon2;

	    /*
	     * Compute the distance using spherical geometry.
	     */
	    val = (double)( sin(rlat1) * sin(rlat2) +
		    cos(rlat1) * cos(rlat2) * cos(dlon) );

	    if  ( -1.0 <= val && val <= 1.0 )  {
		dist[i] = (RADIUS * (float)acos(val));
	    }
	    else {
		dist[i] = 0.0F;
	    }

	}
    }
}
