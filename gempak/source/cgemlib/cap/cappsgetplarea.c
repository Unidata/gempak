#include "capcmn.h"

void cap_psgetplarea(PlacementSet placements, CMDObjectSet objects, 
    Handle id, int to_delete, float inf_bbox[4], int *iret)
/*****************************************************************************
 * cap_psgetplarea
 *
 * Computes the bounding box for the area effected by the handle specified.
 *
 * Input parameters:
 *  placements      PlacementSet    Handle to PlacementSet
 *  objects         CMDObjectSet    Meta data for the PlacementSet
 *  id              Handle          Handle to object in question
 *  to_delete       int             1 = to be deleted, 0 = otherwise
 *
 * Output parameters:
 *  *inf_bbox       float           Bounding box for the of the area 
 *                                  impacted by this object
 *                                  in the order, minx, maxx, miny, maxy
 *
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid pointer in arguments
 *                     -4 = Object not found
 **
 * Log:
 * S.Danz/AWC           01/07   Created
 ****************************************************************************/
{
    int                     ier, plloc, index, is_ref;
    float                   tmp_bbox[4], bbox[4], max_dist;
    CMDObject                obj;
    PlaceInfoContainer      *placeinfo;
    PlacementSetContainer   *p;
/*---------------------------------------------------------------------*/

    if (!placements || !objects || !inf_bbox) {
        *iret = -1;
        return;
    }

    p = (PlacementSetContainer*)placements;

    /* First find the object in the meta data structures */
    cmd_osgetob(objects, id, &obj, &ier);

    if (obj) {
        inf_bbox[0] =  99999.0F;
        inf_bbox[1] = -99999.0F;
        inf_bbox[2] =  99999.0F;
        inf_bbox[3] = -99999.0F;

        cmd_obgetbb(obj, &bbox[0], &bbox[1], &bbox[2], &bbox[3], &ier);

        plloc = cap_psfindpl(placements, id, 1);
        if (plloc != -1) {
            /*
             */
            placeinfo = p->places[plloc];
            if (placeinfo->was_placed) {
                /* 
                 * If we are placed, then its 'easy' in that its just
                 * our CMD location plus the offset... also include 
                 * the arrow endpoint since placing the object 'creates'
                 * the arrow
                 */
                bbox[0] += placeinfo->offset.delta_x;
                bbox[1] += placeinfo->offset.delta_x;
                bbox[2] += placeinfo->offset.delta_y;
                bbox[3] += placeinfo->offset.delta_y;
                cap_mergebbox(inf_bbox, bbox, inf_bbox);
                if (placeinfo->arrow_x[0] < placeinfo->arrow_x[1]) {
                    bbox[0] = placeinfo->arrow_x[0];
                    bbox[1] = placeinfo->arrow_x[1];
                } else {
                    bbox[0] = placeinfo->arrow_x[1];
                    bbox[1] = placeinfo->arrow_x[0];
                }
                if (placeinfo->arrow_y[0] < placeinfo->arrow_y[1]) {
                    bbox[2] = placeinfo->arrow_y[0];
                    bbox[3] = placeinfo->arrow_y[1];
                } else {
                    bbox[2] = placeinfo->arrow_y[1];
                    bbox[3] = placeinfo->arrow_y[0];
                }
                cap_mergebbox(inf_bbox, bbox, inf_bbox);
            } else {
                /*
                 * If we are not placed and are deleting, then the area 
                 * effected is just the CMD area. Otherwise, its that 
                 * location plus anywhere around our reference that we
                 * could end up at.
                 */
                cap_mergebbox(inf_bbox, bbox, inf_bbox);
                if (!to_delete) {
                    /* 
                     * We need the bbox of the reference
                     */
                    cmd_osgetob(objects, placeinfo->reference, &obj, &ier);
                    cmd_obgetbb(obj, &tmp_bbox[0], &tmp_bbox[1], &tmp_bbox[2],
                            &tmp_bbox[3], &ier);

                    cap_psgetplmaxdist(placements, placeinfo, bbox, 
                            &max_dist, &ier);
                    tmp_bbox[0] -= max_dist;
                    tmp_bbox[1] += max_dist;
                    tmp_bbox[2] -= max_dist;
                    tmp_bbox[3] += max_dist;
                    cap_mergebbox(inf_bbox, tmp_bbox, inf_bbox);
                }
            }
        } else {
            /*
             * Not a placed object.  Start with the bbox from CMD for the
             * orginal object
             */
            cap_mergebbox(inf_bbox, bbox, inf_bbox);

            /* 
             * See if it is a reference for a placed object
             */
            is_ref = 0;
            for (index = 0; index < p->used && !is_ref; index++) {
                placeinfo = p->places[index];
                if (id == placeinfo->reference) {
                    is_ref = 1;
                }
            }

            if (is_ref) {
                /*
                 * Get the CMD info for the object from the placement set
                 */
                cmd_osgetob(objects, placeinfo->id, &obj, &ier);
                cmd_obgetbb(obj, &tmp_bbox[0], &tmp_bbox[1], &tmp_bbox[2], 
                        &tmp_bbox[3], &ier);

                if (placeinfo->was_placed) {
                    if (to_delete) {
                        /*
                         * If we are being deleted, and the object we are a
                         * ref for is placed, then its going to 'bounce back'
                         * to its original location, so bring that in before
                         * adjusting for placement.
                         */
                        cap_mergebbox(inf_bbox, tmp_bbox, inf_bbox);
                    }

                    /* 
                     * Now include the bbox of the placed object, either
                     * to get it and/or its arrow included in the result
                     */
                    tmp_bbox[0] += placeinfo->offset.delta_x;
                    tmp_bbox[1] += placeinfo->offset.delta_x;
                    tmp_bbox[2] += placeinfo->offset.delta_y;
                    tmp_bbox[3] += placeinfo->offset.delta_y;

                    cap_mergebbox(inf_bbox, tmp_bbox, inf_bbox);
                } else {
                    /*
                     * If what we are a ref for isn't placed, then we only
                     * need to worry about the non-delete case since deleting
                     * the ref for something not placed isn't going to have
                     * 'extra' effects on the object from placement.  If the ref
                     * is not being delted, then the 'potential' area includes
                     * the entire space around the ref where the placed object
                     * *could* end up *as well as* the *original* location of 
                     * the placement object 
                     */
                    if (!to_delete) {
                        cap_mergebbox(inf_bbox, tmp_bbox, inf_bbox);

                        cap_psgetplmaxdist(placements, placeinfo, tmp_bbox, 
                                &max_dist, &ier);
                        bbox[0] -= max_dist;
                        bbox[1] += max_dist;
                        bbox[2] -= max_dist;
                        bbox[3] += max_dist;
                        cap_mergebbox(inf_bbox, bbox, inf_bbox);
                    }
                }
            }
        }
    } else {
        inf_bbox[0] = 0.0;
        inf_bbox[1] = 0.0;
        inf_bbox[2] = 0.0;
        inf_bbox[3] = 0.0;
        *iret = -4;
    }
}
