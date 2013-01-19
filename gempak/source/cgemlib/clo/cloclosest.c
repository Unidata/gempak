#include "geminc.h"
#include "gemprm.h"

#ifndef FLT_MAX
#define FLT_MAX 1E+37
#endif

void clo_closest ( float *lat, float *lon, int npts, float plat, 
			float plon, int nclose, int *order, int *iret )
/************************************************************************
 * clo_closest								*
 *									*
 * This function returns the order of the lat/lon arrays in terms of	*
 * their geographical closeness to the point in question, ie., the pair	*
 * (lat[order[0]],lon[order[0]]) is closest to point (plat,plon).	*
 * Only the closest nclose pairs will be determined.			*
 *									*
 * clo_closest  ( lat, lon, npts, plat, plon, nclose, order, iret )	*
 *									*
 * Input parameters:							*
 *	*lat		float		Latitude array			*
 *	*lon		float		Longitude array			*
 *	npts		int		Number of lat/lon pairs		*
 *	plat		float		Latitude of point		*
 *	plon		float		Longitude of point		*
 *	nclose		int		Return this number of indices	*
 *									*
 * Output parameters:							*
 *	*order		int		Index into lat/lon arrays	*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/99	Add nclose to calling sequence;		*
 *				re-write algorithm to use clo_dist.	*
 * D.W.Plummer/NCEP	 4/99	Added check for npts == 0 		*
 * M. Li/GSC		10/99	Modified clo_dist code			*
 * M. Li/GSC		10/99	Added multi-points cal. to clo_dist	*
 * D.W.Plummer/NCEP	 1/00	bug fix when some lat/lons are invalid	*
 ***********************************************************************/
{
int	i, j, which, ier;
float	*dist, mindist;
/*---------------------------------------------------------------------*/
    *iret = 0;

    if ( npts == 0 )  {
	*iret = -1;
	return;
    }
    
    dist = (float *) malloc ( (size_t)npts * sizeof(float) );
    clo_dist( &plat, &plon, &npts, lat, lon, dist, &ier );

    for ( i = 0 ; i < nclose ; i++ )  {

	mindist = FLT_MAX;
	which = IMISSD;
	for ( j = 0 ; j < npts ; j++ )  {

	    if ( !ERMISS(dist[j]) && dist[j] <= mindist )  {
		which = j;
		order[i] = which;
		mindist = dist[which];
	    }

	}

	if ( which != IMISSD )  {
	    dist[which] = RMISSD;
	}
	else  {
	    order[i] = IMISSD;
	}

    }

    free ( dist );

}
