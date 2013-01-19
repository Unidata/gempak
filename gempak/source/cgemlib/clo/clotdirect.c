#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

void clo_tdirect ( char *name, float lat, float lon, char *stn, 
					float *dist, float *dir, int *iret )
/************************************************************************
 * clo_tdirect                                                    	*
 *                                                                      *
 * This function takes a latitude/longitude point and computes		*
 * the nearest station (type loctyp), its distance from the point in	*
 * statute miles and the direction.					*
 *                                                                      *
 * clo_tdirect ( name, lat, lon, stn, dist, dir, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*name	char	Location name					*
 *	lat	float	Latitude					*
 *	lon	float	Longitude					*
 *									*
 * Output parameters:                                                   *
 *	*stn	char	Station						*
 *	*dist	float	Distance from point (statute miles)		*
 *	*dir	float	Direction from point (degrees from N)		*
 *	*iret	int	Return value					*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 4/98	Create					*
 * T. Piper/GSC		10/98	Prolog update				*
 * D.W.Plummer/NCEP	12/98	Rename from clo_direct to clo_tdirect	*
 * D.W.Plummer/NCEP	12/98	Change clo_tclsst to clo_tclosest	*
 * D.W.Plummer/NCEP	12/98	Changed how clo_tgid is called		*
 * S. Law/GSC		01/99	Moved direction stuff to clo_direct	*
 * M. Li/GSC		10/99	Modified clo_direct and clo_dist codes	*
 * M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
 * D.W.Plummer/NCEP	12/99	Add check when no points are returned	*
 ***********************************************************************/
{
int	npt, nclose, ier, np;
float	vlat, vlon, ddist, ddir;
char	id[20];
/*---------------------------------------------------------------------*/

	*iret = 0;

        /*
         *  Consider all points of type name
         */
	nclose = 1;
        clo_tclosest( name, lat, lon, nclose, &ier);

        /*
         *  Get station id.
         */
	clo_tgid( name, 1, sizeof(id), &npt, id, &ier );

	if ( npt == 0 )  {

	    *iret = -1;
	    stn[0] = CHNULL;
	    *dist = RMISSD;
	    *dir  = RMISSD;

	}
	else  {

	    strcpy( stn, strtok( id, ";" ) );

            /*
             *  Get lat/lon.
             */
            clo_tgltln( name, 1, &npt, &vlat, &vlon, &ier );

            /*
             *  Compute distance between two earth points.
             */
	    np = 1;
	    clo_dist( &lat, &lon, &np, &vlat, &vlon, &ddist, iret );
	    *dist = ddist;
            /*
             *  Compute directional angle (degrees from N).
             */
	    if ( G_DIFF(*dist, 0.0F) )  {

	        *dir = 0.0F;

	    }
	    else  {

	        clo_direct (&lat, &lon, &vlat, &vlon, &ddir, &ier);
                *dir = ddir;

	    }

	}

	return;

}
