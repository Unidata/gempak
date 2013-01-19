#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t   	clo;

void clo_tinltln ( char *name, float lat1, float lat2, float lon1, 
						float lon2, int *iret )
/************************************************************************
 * clo_tinltln								*
 *									*
 * This function extracts an array of integers which are		*
 * offsets into the lat/lon arrays.  They equate to the lat/lon 	*
 * array entries which are within the area bounded by the 		*
 * parameters of this call.						*
 *									*
 * clo_tinltln  ( name, lat1, lat2, lon1, lon2, iret )			*
 *									*
 * Input parameters:							*
 *	*name		char	Location name				*
 *	lat1		float	Latitude bound #1			*
 *	lat2		float	Latitude bound #2			*
 *	lon1		float	Longitude bound #1			*
 *	lon2		float	Longitude bound #2			*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *				= -1 - Invalid name			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/99	Created					*
 * D.W.Plummer/NCEP	 4/99	Updated for MARINE and COASTAL types	*
 * D.W.Plummer/NCEP	 9/00   Added call to clo_sortstn		*
 ***********************************************************************/
{
int	i, ii, nh, npts, which, ier;
float	latmn, latmx;
float	*lons, *lats;
/*---------------------------------------------------------------------*/

    latmn = G_MIN( lat1, lat2 );
    latmx = G_MAX( lat1, lat2 );

    which = clo_which ( name );

    switch ( clo.loc[which].format )  {

	case 0  	:

	    clo_sortstn ( name, STN_LON, &ier );

	    npts = clo.loc[which].stn.nstn;
	    lats = (float *)malloc( (size_t)npts * sizeof(float) );
	    lons = (float *)malloc( (size_t)npts * sizeof(float) );

	    for ( ii = 0; ii < npts; ii++ )  {
		lats[ii] = clo.loc[which].stn.station[ii].lat;
		lons[ii] = clo.loc[which].stn.station[ii].lon;
	    }

            clo_lonin ( lons, npts, lon1, lon2, MAXHOT, 
		        hotlist, &nh, iret );

            nhot = 0;
            for ( i = 0; i < nh; i++ )  {
	        if ( lats[hotlist[i]] >= latmn && 
	             lats[hotlist[i]] <= latmx )  {
	            hotlist[nhot] = hotlist[i];
	            (nhot)++;
	        }
            }

	    free ( lons );
	    free ( lats );

	    break;

	default :

	    *iret = -1;

	    break;

    }

}
