#include "capcmn.h"

void cap_psmarkint(PlacementSet placements, CMDObjectSet objects, 
    float *bbox, int *found, float inf_bbox[4], int *iret)
/*****************************************************************************
 * cap_psmarkint
 *
 * Marks all the placement objects that are intersected by the area given
 * as not placed so they are updated on the next placement update.
 * Also determine the total extent of the area of impace for all the
 * objects that could be updated when placement runs again.
 *
 * Input parameters:
 *  placements      PlacementSet    Handle to PlacementSet to be checked
 *  objects         CMDObjectSet    Meta data for the PlacementSet
 *  *bbox           float           Bounding box for the area to check
 *                                  in the order, minx, maxx, miny, maxy
 *
 * Output parameters:
 *  *found          int             Number of objects marked 
 *  *inf_bbox       float           Bounding box for the of the area 
 *                                  potentially influenced by the updates
 *                                  in the order, minx, maxx, miny, maxy
 *  *iret    int    Return code
 *                       0 = Function successful
 *                      -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC           07/06   Created
 ****************************************************************************/
{
    int                     index, ier;
    float                   tmp_bbox[4], pl_bbox[4], ref_bbox[4], max_dist;
    CMDObject               obj;
    PlaceInfoContainer      *placeinfo;
    PlacementSetContainer   *p;

/*---------------------------------------------------------------------*/

    if (!placements || !objects || !bbox || !found || !inf_bbox) {
        *iret = -1;
        return;
    }

    *found = 0;
    *iret = 0;
    inf_bbox[0] = bbox[0];
    inf_bbox[1] = bbox[1];
    inf_bbox[2] = bbox[2];
    inf_bbox[3] = bbox[3];

    /*
     * First, for all the objects that are placed, see if their bounding box
     * intersects the area in question.  If so, mark them as not placed (so
     * it gets updated) and include their area in the resulting total extent 
     */
    p = (PlacementSetContainer*)placements;
    for (index = 0; index < p->used; index++) {
        placeinfo = p->places[index];
        if (placeinfo->was_placed) {
            cmd_osgetob(objects, placeinfo->id, &obj, &ier);
            cmd_obgetbb(obj, &pl_bbox[0], &pl_bbox[1], 
                    &pl_bbox[2], &pl_bbox[3], &ier);
            pl_bbox[0] += placeinfo->offset.delta_x;
            pl_bbox[1] += placeinfo->offset.delta_x;
            pl_bbox[2] += placeinfo->offset.delta_y;
            pl_bbox[3] += placeinfo->offset.delta_y;
            /* 
             * The 'from' part of the arrow is in the center of the object, 
             * just worry about the 'to' part
             */
            pl_bbox[0] = G_MIN(pl_bbox[0], placeinfo->arrow_x[1]);
            pl_bbox[1] = G_MAX(pl_bbox[1], placeinfo->arrow_x[1]);
            pl_bbox[2] = G_MIN(pl_bbox[2], placeinfo->arrow_y[1]);
            pl_bbox[3] = G_MAX(pl_bbox[3], placeinfo->arrow_y[1]);

            /*
             * Just a bbox intersection, nothing too 'heavy'
             */
            if (((   bbox[0] <= pl_bbox[0] && pl_bbox[0] <=    bbox[1]) || 
                 (   bbox[0] <= pl_bbox[1] && pl_bbox[1] <=    bbox[1]) ||
                 (pl_bbox[0] <=    bbox[0] &&    bbox[0] <= pl_bbox[1]) || 
                 (pl_bbox[0] <=    bbox[1] &&    bbox[1] <= pl_bbox[1])) &&
                ((   bbox[2] <= pl_bbox[2] && pl_bbox[2] <=    bbox[3]) || 
                 (   bbox[2] <= pl_bbox[3] && pl_bbox[3] <=    bbox[3]) ||
                 (pl_bbox[2] <=    bbox[2] &&    bbox[2] <= pl_bbox[3]) || 
                 (pl_bbox[2] <=    bbox[3] &&    bbox[3] <= pl_bbox[3]))) {
                placeinfo->was_placed = 0;
                cap_mergebbox(inf_bbox, pl_bbox, inf_bbox);
                *found += 1;
            }
        }
    }

    /*
     * Now, starting from the area for the 'direct' impacts from original
     * area, if any of 'potential' positions (from the max extent of the 
     * object) intersect this area, then include that 'max extent' in case
     * the object moves within that area to a new location during placement
     */
    tmp_bbox[0] = inf_bbox[0];
    tmp_bbox[1] = inf_bbox[1];
    tmp_bbox[2] = inf_bbox[2];
    tmp_bbox[3] = inf_bbox[3];
    for (index = 0; index < p->used; index++) {
        placeinfo = p->places[index];
        if (!placeinfo->was_placed) {
            cmd_osgetob(objects, placeinfo->id, &obj, &ier);
            cmd_obgetbb(obj, &pl_bbox[0], &pl_bbox[1], 
                    &pl_bbox[2], &pl_bbox[3], &ier);

            cap_psgetplmaxdist(placements, placeinfo, pl_bbox, 
                    &max_dist, &ier);

            cmd_osgetob(objects, placeinfo->reference, &obj, &ier);
            cmd_obgetbb(obj, &ref_bbox[0], &ref_bbox[1], 
                    &ref_bbox[2], &ref_bbox[3], &ier);

            ref_bbox[0] -= max_dist;
            ref_bbox[1] += max_dist;
            ref_bbox[2] -= max_dist;
            ref_bbox[3] += max_dist;

            /*
             * Again, just a bbox intersection, nothing too 'heavy'
             */
            if (((tmp_bbox[0] <= ref_bbox[0] && ref_bbox[0] <= tmp_bbox[1]) || 
                 (tmp_bbox[0] <= ref_bbox[1] && ref_bbox[1] <= tmp_bbox[1]) ||
                 (ref_bbox[0] <= tmp_bbox[0] && tmp_bbox[0] <= ref_bbox[1]) || 
                 (ref_bbox[0] <= tmp_bbox[1] && tmp_bbox[1] <= ref_bbox[1])) &&
                ((tmp_bbox[2] <= ref_bbox[2] && ref_bbox[2] <= tmp_bbox[3]) || 
                 (tmp_bbox[2] <= ref_bbox[3] && ref_bbox[3] <= tmp_bbox[3]) ||
                 (ref_bbox[2] <= tmp_bbox[2] && tmp_bbox[2] <= ref_bbox[3]) || 
                 (ref_bbox[2] <= tmp_bbox[3] && tmp_bbox[3] <= ref_bbox[3]))) {
                cap_mergebbox(inf_bbox, ref_bbox, inf_bbox);
                *found += 1;
            }
        }
    }

    return;
}
