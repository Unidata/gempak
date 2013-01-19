#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_tgltln ( char *name, int maxltln, int *nltln, float *lat, 
						float *lon, int *iret )
/************************************************************************
 * clo_tgltln								*
 *									*
 * This function returns lat and lon information currently indexed    	*
 * by the hotlist.							*
 *									*
 * clo_tgltln  ( name, maxltln, nltln, lat, lon, iret )			*
 *									*
 * Input parameters:							*
 *	*name		char	Data type				*
 *	maxltln		int	Maximum allowed in the returned arrays	*
 *									*
 * Output parameters:							*
 *	*nltln		int	# of indices in hotlist			*
 *	*lat		float	Latitude array				*
 *	*lon		float	Longitude array				*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/99	Created					*
 * D.W.Plummer/NCEP	 4/99	Updated for MARINE and COASTAL types	*
 * D.W.Plummer/NCEP	 8/00	changes for updated CLO library		*
 * D.W.Plummer/NCEP	 7/01	bug fix for number of items returned	*
 ***********************************************************************/
{
int	i, ii, which;
/*---------------------------------------------------------------------*/
    *iret = 0;

    which = clo_which ( name );

    switch ( clo.loc[which].format )  {

        case 0:		/*  Standard station table format	*/

	    for ( i = 0; i < nhot; i++ )  {

	        if ( i < maxltln )  {
	            lat[i] = clo.loc[which].stn.station[hotlist[i]].lat;
	            lon[i] = clo.loc[which].stn.station[hotlist[i]].lon;
	        }

            }
    	    *nltln = G_MIN( nhot, maxltln );

	    break;

        case 1:		/*  Extract centroid info from bounds struct */

	    for ( ii = 0; ii < nhot; ii++ )  {

	      if ( ii < maxltln )  {
	        lat[ii] = clo.loc[which].bnd.bound[hotlist[ii]].cenlat;
	        lon[ii] = clo.loc[which].bnd.bound[hotlist[ii]].cenlon;
	      }

            }
    	    *nltln = G_MIN( nhot, maxltln );

	    break;

    }

}
