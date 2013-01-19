#include "geminc.h" 
#include "gemprm.h"

void clo_track ( float lat1, float lon1, fdttms_t time1, float lat2, 
		float lon2, fdttms_t time2, int inc, int ntimes,
		float *spd, float *dir, float *lat, float *lon, 
		fdttms_t *time, int *iret )
/************************************************************************
 * clo_track								*
 *									*
 * This function calculates extrapolated track information from two	*
 * points in space/time.  The track increment may only be 30 or 60.	*
 *									*
 * clo_track (lat1, lon1, time1, lat2, lon2, time2, inc, ntimes,	*
 *            spd, dir, lat, lon, time, iret)				*
 *									*
 * Input parameters:							*
 *	lat1		float		Point #1 Latitude		*
 *	lon1		float		Point #1 Longitude		*
 *	time1		fdttms_t	Point #1 Time (YYMMDD/HHMM)	*
 *	lat2		float		Point #2 Latitude		*
 *	lon2		float		Point #2 Longitude		*
 *	time2		fdttms_t	Point #2 Time (YYMMDD/HHMM)	*
 *	inc		int		Track increment in minutes	*
 *	ntimes		int		Number of future times		*
 *									*
 * Output parameters:                                                   *
 *	*spd		float		Speed (meters per second)	*
 *	*dir		float		Direction			*
 *	*lat		float		Latitude array			*
 *	*lon		float		Longitude array			*
 *	*time		fdttms_t	Time array (YYMMDD/HHMM)	*
 *	*iret		int		Return value			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 5/99						*
 * S. Law/GSC		05/99	move times computation to clo_times	*
 * M. Li/GSC		10/99	modified clo_direct, clo_dltln 		*
 *				and clo_dist codes			*
 * M. Li/GSC		10/99	added multi-point cal. to clo_dist	*
 * S. Law/GSC		07/00	changed to use GEMPAK times		*
 ***********************************************************************/
{
    int		ii, ier, dt, et, npx;
    float	dx, td, ddir;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Compute time difference in minutes.
     */
    ti_diff (time2, time1, &dt, &ier, strlen(time2), strlen(time1));

    if (dt == 0)  {
	*iret = -1;
	return;
    }

    /*
     *  Get distance difference in meters.
     */
    npx = 1;
    clo_dist ( &lat1, &lon1, &npx, &lat2, &lon2, &dx, &ier );

    /*
     *  Compute speed in meters per minute.
     */
    *spd = dx / (float)dt;

    /*
     *  Get direction in degrees from north.
     */
    clo_direct ( &lat2, &lon2, &lat1, &lon1, &ddir, &ier );
    *dir = ddir;

    /*
     *  Get times.
     */
    clo_times (time2, inc, ntimes, time, iret);
    if (*iret != 0) return;

    /*
     * Compute latitudes and longitudes.
     */
    for (ii = 0; ii < ntimes; ii++) {

	/*
	 *  Compute elapsed time in minutes.
	 */
	if (ii == 0) {
	    ti_diff (time[0], time2, &et, &ier, 
		     strlen(time[0]), strlen(time2));
	}
	else {
	    et += inc;
	}

	/*
	 *  Compute total distance in meters.
	 */
	td = *spd * (float) et;

	/*
	 *  Get lat,lon at track point.
	 */
	clo_dltln (&lat2, &lon2, &td, &ddir, &(lat[ii]), &(lon[ii]), &ier);
    }

    /*
     *  Convert speed in meters per minute to meters per second.
     */

    *spd /= 60.0F;

    return;
}
