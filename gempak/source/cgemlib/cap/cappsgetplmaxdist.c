#include "capcmn.h"

void cap_psgetplmaxdist(PlacementSet placements, PlaceInfoContainer *placeinfo, 
    float bbox[4], float *max_dist, int *iret)
/*****************************************************************************
 * cap_psgetplmaxdist
 *
 * Computes the maximum distance away from the reference that the placement
 * object could be located.
 *
 * Input parameters:
 *  placements  PlacementSet        Handle to PlacementSet
 *  placeinfo   *PlaceInfoContainer Placement object
 *  bbox        float[4]            Bounding box for the Placement object
 *                                  in the order, minx, maxx, miny, maxy
 *
 * Output parameters:
 *  *max_dist   float               Maximum distance between object and ref
 *  *iret       int                 Return code
 *                                       0 = Results are valid
 *                                      -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlacementSetContainer   *p;
    float                   def_dist, diag;
/*---------------------------------------------------------------------*/

    if (placements && placeinfo && max_dist) {
        p = (PlacementSetContainer*)placements;

        /* 
         * Use a default distance if the client did not specify,
         * include 1/2 the diagonal of the bounding box so that the 
         * visible line outside the object will be approx. the distance 
         * requested
         */
        diag = (float)sqrt((double)((bbox[1]-bbox[0])*(bbox[1]-bbox[0])+
                (bbox[3]-bbox[2])*(bbox[3]-bbox[2]))
            ) / 2.0;

        if (p->distance > 0) {
            def_dist = p->distance;  
        } else {
            def_dist = p->default_dist;
        }

        *max_dist = diag + placeinfo->dist_offset * def_dist +
                def_dist * placeinfo->dist_incr * (placeinfo->max_attempts - 1);
        
    } else {
        *iret = -1;
    }

    return;
}

