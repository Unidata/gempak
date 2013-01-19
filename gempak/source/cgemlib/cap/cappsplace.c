#include "capcmn.h"

/****************************************************************************
 * Function prototypes for private functions
 ****************************************************************************/
static void sort_placementset(CMDObjectSet, PlacementSetContainer*,
    PlaceInfoContainer***, int*);
static void compute_potential_locations(CMDObjectSet, PlacementSetContainer*,
    PlaceInfoContainer*, int, int, int*, int*, CAPOffset**, CAPOffset**, int*);
static int  compare_clutter(const void*, const void*);
static void adjust_by_offset(float, float, int,
    const float[], const float[], float, float, float[],
    float[], float[], float*, float*, float[]);
static int count_intersections(float*, float*, int, 
    const float*, const float*, int, int);

void
cap_psplace(PlacementSet placements, CMDObjectSet objects, int *iret)
/*****************************************************************************
 * cap_psplace
 *
 * For each object in the PlacementSet, attempt to place that object around
 * its reference without obstructing any other object, either with itself or
 * the line that would connect the object back to its reference.
 *
 * Input parameters:
 *  placements  PlacementSet    Set of placements to compute
 *  objects     CMDObjectSet    Complete set of objects to place against
 *
 * Output parameters:
 *  *iret       int     Return code
 *                      1 = At least one object in the set was not placed
 *                      0 = Function successful
 *                     -1 = Invalid pointer in arguments
 *                     -2 = The plot area was not previously set
 *                     -3 = Could not allocate memory
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    CAPOffset               *offsets, *ref_locations, off_device_offset;
    CAPOffset               off_device_ref, wo_arrow_offset;
    CAPOffset               inside_cross_offset;
    CMDObject               check_obj, obj_ref, obj_to_place;
    Handle                  check_obj_id;
    PlaceInfoContainer      **sorted_index, *check_obj_info, *placeinfo;
    Placement               check_obj_place;
    PlacementSetContainer   *p;
    const float             *check_x, *check_y, *orig_x, *orig_y, *ref_x;
    const float             *ref_y;
    float                   *check_tmp_x, *check_tmp_y, *tmp_x, *tmp_y;
    float                   check_bb[4], check_cntr_x, check_cntr_y, line_x[2];
    float                   line_y[2], orig_bb[4], orig_cntr_x, orig_cntr_y;
    float                   ref_bb[4], tmp_bb[4], tmp_cntr_x, tmp_cntr_y;
    float                   wo_arrow_cntr_x, wo_arrow_cntr_y;
    float                   wo_arrow_ref_x, wo_arrow_ref_y;
    float                   inside_cross_cntr_x, inside_cross_cntr_y;
    float                   inside_cross_ref_x, inside_cross_ref_y;
    int                     all_inside, attempt, centers, check_ispoly;
    int                     check_points, check_tmp_pts, count, index, winner;
    int                     intersected, intersects, line_inside, line_pts;
    int                     location, obj_inside, ok_to_place, point_to_center;
    int                     points, pts, ref_inside, ref_points, result;
    int                     tmp_pts, visible, ok_wo_arrow, loc_wo_arrow;
    int                     ref_intersects, ok_inside_cross, loc_inside_cross;
/*---------------------------------------------------------------------*/

    p = (PlacementSetContainer*)placements;
    placeinfo = NULL;
    sorted_index = NULL;
    result = 0;
    tmp_pts = 0;
    tmp_x = NULL;
    tmp_y = NULL;
    check_x = NULL;
    check_y = NULL;
    check_tmp_pts = 0;
    check_tmp_x = NULL;
    check_tmp_y = NULL;

    *iret = 0;

    if (!p || !objects) {
        *iret = -1;
        return;
    }

    if (!p->plot_area_valid) {
        *iret = -2;
        return;
    }

    if (!p->used) {
        return;
    }

    /*
     * Obtain an ordered index for the placement set based on the 'cluttering'
     * around each reference object.  The sorted_index is 'owned' by the
     * sort routine, so don't mess with freeing it up here.
     */
    sort_placementset(objects, p, &sorted_index, iret);
    if (*iret != 0) {
        return;
    }

    for (index = 0; index < p->used; index++) {
        placeinfo = sorted_index[index];

        /* 
         * If its already been placed, then just go to the next.  This is
         * for the case where we call placement on the same set multiple
         * times.  For those that are intended to be placed again, the
         * caller would need to clear the flag on their own, or clear all
         * of them with the cap_psclrplaced()
         */
        if (placeinfo->was_placed) {
            continue;
        }

        /*
         * get the object handles for what is being placed and its reference
         * and use that to get the vertices and bounding box for each
         */
        cmd_osgetob(objects, placeinfo->reference, &obj_ref, iret);
        if (!obj_ref) {
            /* If there is no reference, we can't place it */
            placeinfo->was_placed = 0;
            result = 1;
            continue;
        }

        cmd_obgetpoints(obj_ref, &ref_x, &ref_y, &ref_points, iret);
        cmd_obgetbb(obj_ref, &ref_bb[0], &ref_bb[1], &ref_bb[2], &ref_bb[3],
                iret);

        cmd_osgetob(objects, placeinfo->id, &obj_to_place, iret);
        cmd_obgetpoints(obj_to_place, &orig_x, &orig_y, &points, iret);
        cmd_obgetbb(obj_to_place, &orig_bb[0], &orig_bb[1], &orig_bb[2],
                &orig_bb[3], iret);
        cmd_obgetcntr(obj_to_place, &orig_cntr_x, &orig_cntr_y, iret);

        /*
         * build space for a temporary set of points to be used as we move the
         * object around the reference
         */
        if (tmp_pts == 0) {
            G_MALLOC(tmp_x, float, points,
                    "creating temporary vertex x-coordinates for object");
            G_MALLOC(tmp_y, float, points,
                    "creating temporary vertex y-coordinates for object");
            tmp_pts = points;
        } else if (points > tmp_pts) {
            G_REALLOC(tmp_x, float, points,
                    "creating temporary vertex x-coordinates for object");
            G_REALLOC(tmp_y, float, points,
                    "creating temporary vertex y-coordinates for object");
            tmp_pts = points;
        }

        if (!tmp_x || !tmp_y) {
            *iret = -3;
            return;
        }

        /*
         * If the reference isn't visisble, then we can just skip any 'real'
         * placement and just make sure the object is outside the display
         */
        cmd_obisvisible(obj_ref, &visible, iret);
        if (!visible) {
            ok_to_place = 1;
            cmd_obgetcntrd(obj_ref, &(off_device_ref.delta_x), 
                    &(off_device_ref.delta_y), iret);
            off_device_offset.delta_x = off_device_ref.delta_x - orig_cntr_x;
            off_device_offset.delta_y = off_device_ref.delta_y - orig_cntr_y;
            tmp_cntr_x = off_device_ref.delta_x;
            tmp_cntr_y = off_device_ref.delta_y;
            offsets = &off_device_offset;
            ref_locations = &off_device_ref;
            winner = 0;
            centers = 1;
        } else {
            /*
             * Until we have been successful or tried as many times as we can
             * keep adjusting the distance and try again
             */
            point_to_center = placeinfo->point_to_center;
            attempt = 0;
            ok_to_place = 0;
        }

        /*
         * Clear the location where placement works except that the
         * label is inside while the reference isn't
         */
        loc_inside_cross = 0;

        /*
         * Clear the location where placement works except that the
         * arrow crossed something
         */
        loc_wo_arrow = 0;

        while (!ok_to_place && attempt < placeinfo->max_attempts) {
            /*
             * get the list of locations for the object around the ref
             */
            compute_potential_locations(objects, p, placeinfo, 
                    point_to_center, attempt, &count, &centers,
                    &offsets, &ref_locations, iret
                );
            if (*iret != 0) {
                return;
            } 

            /*
             * Loop over the potential postions trying each until we find one
             * that works. The position is defined as an offset from the
             * 'original' location of the object being placed.
             */
            for (location = 0; !ok_to_place && (location < count); location++) {
                /*
                 * create temporary definition of the obj_to_place by adding
                 * offset to each point in its description
                 */
                adjust_by_offset(offsets[location].delta_x,
                        offsets[location].delta_y, points, orig_x, orig_y,
                        orig_cntr_x, orig_cntr_y, orig_bb, tmp_x, tmp_y,
                        &tmp_cntr_x, &tmp_cntr_y, tmp_bb);
                /*
                 * Start by saying everything is fine, so we can stop as soon as
                 * we find a spot that doesn't work.  Loop through all the
                 * defined objects in the forecast to see if any impact with the
                 * new location.
                 */
                ok_to_place = 1;
                ok_wo_arrow = 1;
                ok_inside_cross = 1;

                cmd_ositerinit(objects, iret);
                for (cmd_ositernext(objects, &check_obj, iret);
                        (ok_to_place || ok_wo_arrow) && (check_obj != NULL);
                        cmd_ositernext(objects, &check_obj, iret)
                    ) {
                    /* 
                     * Get the handle for the object we are checking against 
                     */
                    cmd_obgetid(check_obj, &check_obj_id, iret);
                    all_inside = 0;

                    /* 
                     * Don't check against ourselves 
                     */
                    if (check_obj_id == placeinfo->id) {
                        continue;
                    }

                    /* 
                     * Don't check the ref if this location is in the center 
                     * we know its ok or it wouldn't be in the list
                     */
                    if (check_obj_id == placeinfo->reference && 
                        location < centers) {
                        continue;
                    }

                    cap_psgetpl(placements, check_obj_id, &check_obj_place,
                            iret
                        );

                    /* 
                     * For objects not in the placement set, don't check 
                     * against them if they aren't visible.  Placed objects
                     * will be visible (or they wouldn't have been placed)
                     * and if they are not placed they will be moved later
                     */
                    if (!check_obj_place) {
                        cmd_obisvisible(check_obj, &visible, iret);
                        if (!visible) {
                            continue;
                        }
                    }
                    check_obj_info  = (PlaceInfoContainer*)check_obj_place;
                    cmd_obispoly(check_obj, &check_ispoly, iret);

                    /*
                     * If the check_obj isn't in the placement set, or it is but
                     * is placed we can check to see if it is going to intersect
                     * Otherwise its going to be moved later and we will deal
                     * with it when we are placing it
                     */
                    if (check_obj_info == NULL || check_obj_info->was_placed) {
                        /*
                         * Again start by saying its ok so we can stop as soon
                         * as we find a place where they intersect
                         */
                        intersected = 0;

                        /*
                         * Acquire the vertices and bounding box for the object
                         * being checked
                         */
                        cmd_obgetpoints(check_obj, &check_x, &check_y,
                                &check_points, iret
                            );
                        cmd_obgetbb(check_obj, &check_bb[0], &check_bb[1],
                                &check_bb[2], &check_bb[3], iret
                            );
                        cmd_obgetcntr(check_obj, &check_cntr_x, &check_cntr_y,
                                iret);

                        /*
                         * Adjust the object to its placed location if it was
                         * placed
                         */
                        if (check_obj_info) {
                            /*
                             * build space for a temporary set of points to used
                             * as we move the object around the reference
                             */
                            if (check_tmp_pts == 0) {
                                G_MALLOC(check_tmp_x, float, check_points,
                                        "creating tmp x-coord for check obj");
                                G_MALLOC(check_tmp_y, float, check_points,
                                        "creating tmp y-coord for check obj");
                                check_tmp_pts = check_points;
                            } else if (check_points > check_tmp_pts) {
                                G_REALLOC(check_tmp_x, float, check_points,
                                        "creating tmp x-coord for check obj");
                                G_REALLOC(check_tmp_y, float, check_points,
                                        "creating tmp y-coord for check obj");
                                check_tmp_pts = check_points;
                            }

                            if (!check_tmp_x || !check_tmp_y) {
                                *iret = -3;
                                return;
                            }

                            adjust_by_offset(check_obj_info->offset.delta_x,
                                    check_obj_info->offset.delta_y,
                                    check_points, check_x, check_y,
                                    check_cntr_x, check_cntr_y, check_bb,
                                    check_tmp_x, check_tmp_y, &check_cntr_x,
                                    &check_cntr_y, check_bb
                                );

                            check_x = check_tmp_x;
                            check_y = check_tmp_y;
                        }

                        /*
                         * See if the location intersects the object to check
                         * If they do there is more checking
                         */
                        cgr_objint(points, tmp_x, tmp_y, tmp_bb,
                                check_points, check_x, check_y, check_bb,
                                &intersects, iret
                            );
                        if (intersects) {
                            intersected = 1;
                            /* If we are not looking at the reference object
                             * and this is a polygon, then we have to see if
                             * maybe both the object and its reference are
                             * inside the object being checked.  If they are
                             * then we are 'ok'.
                             */
                            if (check_obj_id != placeinfo->reference && 
                                check_ispoly) {
                                cgr_objinpoly(points, tmp_x, tmp_y, tmp_bb,
                                        check_points, check_x, check_y,
                                        check_bb, &obj_inside, iret
                                    );
                                if (obj_inside) {
                                    cgr_objinpoly(ref_points, ref_x, ref_y,
                                            ref_bb, check_points, check_x,
                                            check_y, check_bb, &ref_inside,
                                            iret
                                        );
                                    /*
                                     * Ok, they are both inside, remove the
                                     * flag saying this was a 'bad' location
                                     * We don't have to check the line between
                                     * the obj & its ref since we do that later
                                     */
                                    if (ref_inside) {
                                        intersected = 0;
                                        all_inside = 1;
                                    } else {
                                        /* If the ref crosses the object being 
                                         * checked, but the label is inside, 
                                         * then this might be the best we can
                                         * do so hang on to this.
                                         */
                                        cgr_objint(ref_points, ref_x, ref_y, 
                                                ref_bb, check_points, check_x,
                                                check_y, check_bb, 
                                                &ref_intersects, iret
                                            );
                                        if (!ref_intersects) {
                                            ok_inside_cross = 0;
                                        }
                                    }
                                } else {
                                    /* If we were ok inside an object before
                                     * we aren't now as we actually cross over
                                     * something else
                                     */
                                    ok_inside_cross = 0;
                                }
                            }
                        }

                        /*
                         * If the object to check was placed, and it isn't in
                         * in the center (since there will be no line then)
                         * check against the line between it and its reference
                         * to make sure we aren't getting in the way
                         */
                        if (!intersected && check_obj_info &&
                            check_obj_info->was_placed && 
                            !check_obj_info->in_center ) {
                            pts = 2;
                            cgr_objint(points, tmp_x, tmp_y, tmp_bb, pts,
                                    check_obj_info->arrow_x,
                                    check_obj_info->arrow_y, NULL, &intersects,
                                    iret
                                );
                            if (intersects) {
                                intersected = 1;
                            }
                        }

                        /*
                         * If the object itself has intersected, or its a
                         * location that is inside the object, then this 
                         * location isn't one that we want to consider as
                         * ok without the arrow
                         */
                        if (intersected || location < centers) {
                            ok_wo_arrow = 0;
                        }

                        /*
                         * If we are still ok, and we aren't placing in the
                         * center, then check that the potential
                         * line between the object and its ref does not
                         * hit the object being checked
                         */
                        if (!intersected && location >= centers) {
                            /*
                             * Create a line from the location around the ref
                             * and the center of the object at the new location
                             */
                            line_pts = 2;
                            line_x[0] = ref_locations[location].delta_x;
                            line_y[0] = ref_locations[location].delta_y;
                            line_x[1] = tmp_cntr_x;
                            line_y[1] = tmp_cntr_y;

                            /*
                             * Only bother if the object being checked
                             * isn't the reference, of course the line
                             * from the object to the ref touches the
                             * ref, that's it purpose!
                             */
                            if (check_obj_id != placeinfo->reference) {
                                if (all_inside) {
                                    /*
                                     * If what is being placed and its
                                     * ref were both inside the object
                                     * then we just need to know if the
                                     * line is completely inside the
                                     * object as well, really only is an
                                     * issue when the enclosing object is
                                     * concave somewhere along the way
                                     */
                                    cgr_objinpoly(line_pts, line_x, line_y,
                                            NULL, check_points, check_x,
                                            check_y, check_bb, &line_inside,
                                            iret
                                        );
                                    if (line_inside) {
                                        intersected = 0;
                                    }
                                } else {
                                    cgr_objint(line_pts, line_x, line_y, NULL,
                                            check_points, check_x, check_y,
                                            check_bb, &intersects, iret
                                        );
                                    if (intersects) {
                                        intersected = 1;
                                        /*
                                         * If the ref is completely inside the
                                         * check_obj, and the check_obj is a 
                                         * polygon, then its ok since there is
                                         * no other choice but to cross the line
                                         */
                                        if (check_ispoly) {
                                            cgr_objinpoly(ref_points, ref_x, 
                                                    ref_y, ref_bb, check_points,
                                                    check_x, check_y, check_bb,
                                                    &ref_inside, iret
                                                );
                                            if (ref_inside) {
                                                intersected = 0;
                                            }
                                        }
                                    }
                                }
                            }

                            /*
                             * If we are still ok, and the object being checked
                             * was placed, then check the line for the checked 
                             * doesn't hit the line for what is being placed
                             */
                            if (!intersected && check_obj_info &&
                                check_obj_info->was_placed) {
                                pts = 2;
                                cgr_objint(line_pts, line_x, line_y, NULL, pts,
                                        check_obj_info->arrow_x,
                                        check_obj_info->arrow_y, NULL,
                                        &intersects, iret
                                    );
                                if (intersects) {
                                    intersected = 1;
                                }
                            }
                        }

                        /*
                         * If we went through all that and something
                         * intersected, this is not a valid location
                         */
                        if (intersected) {
                            ok_to_place = 0;
                        }
                    }
                }

                /*
                 * If we end with a spot that is ok without the arrow
                 * keep track of this location in case we can't find
                 * anything better
                 */
                if (ok_wo_arrow && !loc_wo_arrow) {
                    loc_wo_arrow = 1;
                    wo_arrow_cntr_x = tmp_cntr_x;
                    wo_arrow_cntr_y = tmp_cntr_y;
                    wo_arrow_ref_x  = ref_locations[location].delta_x;
                    wo_arrow_ref_y  = ref_locations[location].delta_y;
                    wo_arrow_offset = offsets[location];
                }

                /*
                 * If we end with a spot that is inside a polygon
                 * that crosses the reference, keep it in case we
                 * can't find anything better
                 */
                if (ok_inside_cross && !loc_inside_cross) {
                    loc_inside_cross = 1;
                    inside_cross_cntr_x = tmp_cntr_x;
                    inside_cross_cntr_y = tmp_cntr_y;
                    inside_cross_ref_x  = ref_locations[location].delta_x;
                    inside_cross_ref_y  = ref_locations[location].delta_y;
                    inside_cross_offset = offsets[location];
                }

                /*
                 * Save the working location since the for() messes
                 * with the value
                 */
                if (ok_to_place) {
                    winner = location;
                }
            }

            /*
             * Well, one more attempt to place has been completed
             */
            attempt++;

            /*
             * If we didn't find a spot on any attempt, and were trying to 
             * point to the centroid, try again but this time point to the 
             * edge since that will move the arrow
             */
            if (!ok_to_place && attempt == placeinfo->max_attempts 
                && point_to_center) {
                point_to_center = 0;
                attempt = 0;
            }
        }

        if (ok_to_place) {
            /*
             * We have a winner!  Save the information in the placement
             * record.
             */
            placeinfo->was_placed = 1;
            placeinfo->in_center  = (winner < centers) ? 1 : 0;
            placeinfo->arrow_x[0] = tmp_cntr_x;
            placeinfo->arrow_x[1] = ref_locations[winner].delta_x;
            placeinfo->arrow_y[0] = tmp_cntr_y;
            placeinfo->arrow_y[1] = ref_locations[winner].delta_y;
            placeinfo->offset = offsets[winner];
        } else {
            /*
             * If we didn't find anything, but had a place where it would have
             * worked if not for the arrow line crossing something, give up and
             * use that location and call it ok.  The logic is, if we have space
             * for the object and the only problem is that the arrow crosses 
             * something else, moving farther out isn't going to change what
             * the arrow intersects so just use what was found.
             */
            if (loc_wo_arrow) {
                placeinfo->was_placed = 1;
                placeinfo->in_center  = 0;
                placeinfo->arrow_x[0] = wo_arrow_cntr_x;
                placeinfo->arrow_x[1] = wo_arrow_ref_x;
                placeinfo->arrow_y[0] = wo_arrow_cntr_y;
                placeinfo->arrow_y[1] = wo_arrow_ref_y;
                placeinfo->offset = wo_arrow_offset;
            } else {
                /*
                 * If we didn't find anything, but had a place where it would have
                 * worked to put inside a polygon that crosses over the reference,
                 * use that location and call it ok.  The logic is, if we have space
                 * for the object and the only problem is that the reference clips
                 * into the polygon that the label is completely inside of, then
                 * we really won't intersect a line so just use it.
                 */
                if (loc_inside_cross) {
                    placeinfo->was_placed = 1;
                    placeinfo->in_center  = 0;
                    placeinfo->arrow_x[0] = inside_cross_cntr_x;
                    placeinfo->arrow_x[1] = inside_cross_ref_x;
                    placeinfo->arrow_y[0] = inside_cross_cntr_y;
                    placeinfo->arrow_y[1] = inside_cross_ref_y;
                    placeinfo->offset = inside_cross_offset;
                } else {
                    placeinfo->was_placed = 0;
                    result = 1;
                }
            }
        }
    }

    G_FREE(tmp_x, float);
    G_FREE(tmp_y, float);
    G_FREE(check_tmp_x, float);
    G_FREE(check_tmp_y, float);

    *iret = result;
    return;
}


static void
sort_placementset(CMDObjectSet objects, PlacementSetContainer *placements, 
        PlaceInfoContainer ***sorted_index, int *iret)
/*****************************************************************************
 * sort_placementset
 *
 * Function to take the set of objects to place and return a sorted array that
 * is based on the 'clutter' around the reference for that object.  The most
 * cluttered will be listed first so that those with less room to maneuver will
 * be placed first.  The memory is kept between calls, so the caller should
 * NOT worry about freeing it.
 *
 * Input parameters:
 *  objects     CMDObjectSet            Set of objects to place against
 *  placements  PlacementSetContainer   Set of placements to compute
 *
 * Output parameters:
 *  ***sorted_index  PlaceInfoContainer Array of pointers to
 *                                      PlaceInfoContainers sorted by
 *                                      the amount of clutter around the
 *  *iret           int                 Return code
 *                                          0 = Function successful
 *                                         -3 = Could not allocate memory
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    static PlaceInfoContainer **index_list = (PlaceInfoContainer**)NULL;
    static int index_length = 0;

    PlaceInfoContainer  *placeinfo;
    CMDObject           obj, obj_ref, check_obj;
    Handle              obj_id;
    PlaceInfoContainer  *obj_info;
    Placement           obj_place;
    float               ref_bb[4], obj_bb[4], tmp_bb[4], tmp_x[5], tmp_y[5];
    float               half_width, half_height, diag;
    int                 visible, intersects, inside[5], index, check_points;
    int                 ii, edges;
    const float         *check_x, *check_y;
    float               check_bb[4];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (index_list) {
        if (placements->used > index_length) {
            G_REALLOC(index_list, PlaceInfoContainer*, placements->used,
                    "extending sorted placement index");
            index_length = placements->used;
        }
    } else {
        G_MALLOC(index_list, PlaceInfoContainer*, placements->used,
                "creating sorted placement index");
        index_length = placements->used;
    }

    if (!index_list) {
        *iret = -4;
        return;
    }

    /*
     * Compute the 'clutter' index for each placement entry and
     * put a pointer reference in the index list so they
     * can be sorted later.
     */
    for (index = 0; index < placements->used; index++) {
        placeinfo = placements->places[index];
        placeinfo->clutter = 0;
        index_list[index] = placeinfo;
        /*
         * get the object handles for what is being placed and its reference
         * and use that to get the bounding box for each
         */
        cmd_osgetob(objects, placeinfo->id, &obj, iret);
        cmd_osgetob(objects, placeinfo->reference, &obj_ref, iret);
        cmd_obgetbb(obj_ref, &ref_bb[0], &ref_bb[1], &ref_bb[2], &ref_bb[3],
                iret);
        cmd_obgetbb(obj, &obj_bb[0], &obj_bb[1], &obj_bb[2], &obj_bb[3], iret);

        /*
         * 'expand' the bounding box around the reference to include the
         * 1/2 diagonal from the object to place, and using an initial dist of
         * 0 from the object to the reference.  Use the box to define the 
         * 'area of influence' and see who impacts us there
         */
        diag = (float)sqrt((double)((obj_bb[1]-obj_bb[0])*(obj_bb[1]-obj_bb[0])
                + (obj_bb[3]-obj_bb[2])*(obj_bb[3]-obj_bb[2])
            )) / 2.0;

        half_width  = (obj_bb[1] - obj_bb[0]) / 2.0;
        half_height = (obj_bb[3] - obj_bb[2]) / 2.0;

        tmp_bb[0] = ref_bb[0] - (diag + half_width);
        tmp_bb[1] = ref_bb[1] + (diag + half_width);
        tmp_bb[2] = ref_bb[2] - (diag + half_height);
        tmp_bb[3] = ref_bb[3] + (diag + half_height);

        tmp_x[0] = tmp_bb[0]; tmp_y[0] = tmp_bb[2];
        tmp_x[1] = tmp_bb[1]; tmp_y[1] = tmp_bb[2];
        tmp_x[2] = tmp_bb[1]; tmp_y[2] = tmp_bb[3];
        tmp_x[3] = tmp_bb[0]; tmp_y[3] = tmp_bb[3];
        tmp_x[4] = tmp_bb[0]; tmp_y[4] = tmp_bb[2];

        cmd_ositerinit(objects, iret);
        for (cmd_ositernext(objects, &check_obj, iret); check_obj != NULL;
            cmd_ositernext(objects, &check_obj, iret)) {
            cmd_obisvisible(check_obj, &visible, iret);
            if (visible) {
                cmd_obgetid(check_obj, &obj_id, iret);
                cap_psgetpl(placements, obj_id, &obj_place, iret);
                obj_info = (PlaceInfoContainer*)obj_place;
                if (!obj_place || obj_info->was_placed) {
                    cmd_obgetpoints(check_obj, &check_x, &check_y, 
                            &check_points, iret);
                    cmd_obgetbb(check_obj, &check_bb[0], &check_bb[1], 
                            &check_bb[2], &check_bb[3], iret
                    );

                    cgr_objint(5, tmp_x, tmp_y, tmp_bb, check_points, 
                            check_x, check_y, check_bb, &intersects, iret
                    );
                    if (intersects) {
                        placeinfo->clutter++;
                    }
                }
            }
        }

        /*
         * If the box is not completely inside the plot area, then we are 
         * 'too close' to the edge so add one for the clutter there as well
         */
        cgr_inpolywn(5, tmp_x, tmp_y, 5, placements->plot_area_x,
                placements->plot_area_y, 1, inside, iret
            );
        /* 
         * Only check points 0-3 since the definition for the area repeats
         * the first point at the end
         */
        edges = 0;
        for (ii = 0; ii < 4; ii++) {
            if (!inside[ii]) {
                edges++;
            }
        }
        edges = ceil((double)edges/2.0);
        placeinfo->clutter += edges;
    }

    /*
     * Now that we have a grading scale... sort the elements in the
     * index_list in reverse order by the clutter value
     */
    qsort(index_list, placements->used, sizeof(PlaceInfoContainer*),
            compare_clutter);

    *sorted_index = index_list;
}


static void compute_potential_locations(CMDObjectSet objects, 
        PlacementSetContainer *placements, PlaceInfoContainer *place, 
        int point_to_center, int attempt, int *count, int *centers, 
        CAPOffset **offsets, CAPOffset **ref_locations, int *iret)
/*****************************************************************************
 * compute_potential_locations
 *
 * Function to take the set of objects to place and the information about an
 * object being placed, and create a prioritized set of potential locations 
 * around its reference.  The distance away from the reference is driven from 
 * the info in the placement info container, as well as what 'pass' around the
 * reference we are on.  Each pass around the reference will move the object
 * to be placed farther out until the max number of passes is attempted.
 * The memory allocated for the offset and location results will be held by
 * this function for reuse, so the caller should *NOT* attempt to free it.
 *
 * Input parameters:
 *  CMDObjectSet          objects          Set of objects to place against
 *  PlacementSetContainer *placements      Set of PlaceInfoContainers
 *  PlaceInfoContainer    *place           Placement to compute locations for
 *  int                   point_to_center  Point to centroid or edge
 *  int                   attempt          Pass around the reference are we on
 *
 * Output parameters:
 *  *count          int         How many locations were calculated
 *  *centers        int         How many at the start of the set are inside
 *                              the reference (i.e. don't need an arrow)
 *  **offsets       CAPOffset   Array containing the offsets that
 *                              were computed
 *  **ref_location  CAPOffset   Array with the matching location
 *                              around the reference that each
 *                              offset coresponds to
 *  *iret           int         Return code
 *                              0 = Function successful
 *                             -3 = Could not allocate memory
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    static CAPOffset    *offset_list = (CAPOffset*)NULL;
    static CAPOffset    *ref_list = (CAPOffset*)NULL;
    static int          list_len = 0;

    CMDObject   obj, ref;
    const float *obj_x, *obj_y, *ref_x, *ref_y;
    float       *tmp_x, *tmp_y, diag, dist, obj_bb[4], obj_cntr_x, obj_cntr_y;
    float       ref_bb[4], ref_cntr_x, ref_cntr_y, step, step_incr, step_x;
    float       step_y, steps, tmp_bb[4], tmp_cntr_x, tmp_cntr_y, x, x_line[3];
    float       x_off, y, y_line[3], y_off;
    int         center_count, estimate, excl_end, excl_start, finish, incr;
    int         inside, locations, obj_points, pass, prev, next, curr;
    int         ref_cntrd_inside, ref_concave, ref_ispoly, ref_points;
    int         try_inside;
/*---------------------------------------------------------------------*/

    obj_x = NULL;
    obj_y = NULL;
    ref_x = NULL;
    ref_y = NULL;
    tmp_x = NULL;
    tmp_y = NULL;
    center_count = 0;
    locations = 0;

    *iret = 0;

    /*
     * Add one to attempt so it is one-based (not starting from zero)
     * this makes the math that uses it later simpler
     */
    attempt++;

    /*
     * If we aren't supposed to be placed now, return no locations
     */
    if (place->mode == DELAYED || place->mode == MODE_UNDEF ||
        (place->mode == ONESHOT && attempt != 1)) {
        *count = 0;
        *offsets = (CAPOffset*)NULL;
        *ref_locations = (CAPOffset*)NULL;
        return;
    }

    /*
     * Get the info about the object and its reference, we will be needing
     * the info to do the computations.
     */
    cmd_osgetob(objects, place->id, &obj, iret);
    cmd_osgetob(objects, place->reference, &ref, iret);
    cmd_obgetpoints(obj, &obj_x, &obj_y, &obj_points, iret);
    cmd_obgetpoints(ref, &ref_x, &ref_y, &ref_points, iret);
    cmd_obgetbb(ref, &ref_bb[0], &ref_bb[1], &ref_bb[2], &ref_bb[3], iret);
    cmd_obgetbb(obj, &obj_bb[0], &obj_bb[1], &obj_bb[2], &obj_bb[3], iret);
    cmd_obgetcntrd(ref, &ref_cntr_x, &ref_cntr_y, iret);
    cmd_obgetcntr(obj, &obj_cntr_x, &obj_cntr_y, iret);
    cmd_obispoly(ref, &ref_ispoly, iret);

    /*
     * For later, check if the reference is concave and hang on to the info
     */
    cgr_concave(ref_points, ref_x, ref_y, &ref_concave, iret);

    /*
     * If the ref is concave and is a polygon, and we are planning to use
     * the centroid, check that the centroid is actually inside
     * the polygon.  If not, then later we can't use it since its not 
     * inside the reference
     */
    if (ref_concave && ref_ispoly && (place->allow_center || point_to_center)) {
        cgr_inpolywn(1, &ref_cntr_x, &ref_cntr_y,
                ref_points, ref_x, ref_y,
                0, &ref_cntrd_inside, iret
            );
    } else {
        ref_cntrd_inside = 1;
    }

    /*
     * Even if it is inside the ref, make sure its also inside the plot area
     * if its not, we can't use it
     */
    if (ref_cntrd_inside) {
        cgr_inpolywn(1, &ref_cntr_x, &ref_cntr_y, 5, placements->plot_area_x,
                placements->plot_area_y, 0, &ref_cntrd_inside, iret
            );
    }
    
    /*
     * Use the user defined step increment if it exists and is valid
     */
    if (placements->step_incr > 0) {
        step_incr = placements->step_incr;
    } else {
        step_incr = placements->default_incr;
    }

    /* 
     * Use a default distance if the client did not specify,
     * include 1/2 the diagonal of the bounding box so that the 
     * visible line outside the object will be approx. the distance 
     * requested
     */
    diag = (float)sqrt((double)((obj_bb[1]-obj_bb[0])*(obj_bb[1]-obj_bb[0])+
            (obj_bb[3]-obj_bb[2])*(obj_bb[3]-obj_bb[2]))
        ) / 2.0;

    if (placements->distance > 0) {
        dist = place->dist_offset * placements->distance + 
                placements->distance * place->dist_incr * (attempt - 1);
    } else {
        dist = place->dist_offset * placements->default_dist +
                placements->default_dist * place->dist_incr * (attempt - 1);
    }
    dist += diag;

    /*
     * Temporary areas for testing potential locations for the object
     * being placed
     */
    G_MALLOC(tmp_x, float, obj_points,
            "creating temporary vertex x-coordinates for object");
    G_MALLOC(tmp_y, float, obj_points,
            "creating temporary vertex y-coordinates for object");
    if (!tmp_x || !tmp_y) {
        *iret = -3;
        return;
    }

    /*
     * Start with space for 5x the number of verticies around the reference
     * for locations, yes, I'm guessing
     */
    estimate = ref_points * 5;
    if (estimate > list_len) {
        if (list_len > 0) {
            G_REALLOC(offset_list, CAPOffset, estimate,
                    "extending location set");
            G_REALLOC(ref_list, CAPOffset, estimate,
                    "extending ref location set");
        } else {
            G_MALLOC(offset_list, CAPOffset, estimate,
                    "creating location set");
            G_MALLOC(ref_list, CAPOffset, estimate,
                    "creating ref location set");
        }

        list_len = estimate;
    }
    if (!offset_list || !ref_list) { 
        *iret = -3;
        return;
    }

    /*
     * On the first pass, if allowed, lets try the centroid of the ref
     * if its a polygon.  Putting it on the center of a line would cover
     * the line
     */
    if (place->allow_center && attempt == 1 && ref_cntrd_inside && ref_ispoly) {
        x_off = ref_cntr_x - obj_cntr_x;
        y_off = ref_cntr_y - obj_cntr_y;

        adjust_by_offset(x_off, y_off,
                obj_points, obj_x, obj_y, obj_cntr_x, obj_cntr_y, obj_bb,
                tmp_x, tmp_y, &tmp_cntr_x, &tmp_cntr_y, tmp_bb
            );

        /*
         * See if we are still inside the drawing area, no point in looking
         * if we aren't
         */
        cgr_inpolywn(obj_points, tmp_x, tmp_y, 5, placements->plot_area_x,
                placements->plot_area_y, 0, &inside, iret
            );

        if (inside) {
            /*
             * Check if the object completely fits in the reference, if that is
             * ok, then we will add it to the list and let the caller determine
             * if we are running into anyone else (since they are already doing
             * that)
             */
            cgr_objinpoly(obj_points, tmp_x, tmp_y, tmp_bb, ref_points, ref_x,
                    ref_y, ref_bb, &inside, iret
                );

            if (inside) {
                /*
                 * Include the center in the set, and set the return 
                 * to 1 to tell the caller that the first location is
                 * the center so they can optimize their intersection
                 * tests.
                 */
                center_count = 1;
                offset_list[locations].delta_x = x_off;
                offset_list[locations].delta_y = y_off;
                ref_list[locations].delta_x = ref_cntr_x;
                ref_list[locations].delta_y = ref_cntr_y;
                locations++;
            }
        }
    }

    /*
     * On the first pass, if the ref is a polygon and using the 
     * center is ok, first attempt to place the object inside the
     * ref along the edge.  In this case we don't want to use 
     * the complete distance computed earlier, only the diagonal
     * of the bounding box for the object.
     */
    if (attempt == 1 && ref_ispoly && place->allow_center) {
        try_inside = 1;
    } else {
        try_inside = 0;
    }

    /*
     * Do this at least once, twice if we are to try using an 
     * offset to the *inside* of the ref
     */
    for (;try_inside >= 0; try_inside--) {
        /*
         * Take two passes over the vertices in the reference to create
         * the set of potential locations.  First include the verticies
         * to give them a higher priority in the set.  Then create locations
         * between verticies by stepping along each line segment.
         */
        for (pass = 0; pass < 2; pass++) {
            incr = 1;
            finish = ref_points - 1;
            /*
             *For a line, stop two from the end since we can't bisect
             * the last vertex, except on the second pass when we are
             * stepping along the segment
             */
            if (!ref_ispoly && pass == 0 && finish != 1) {
                finish--;
            }

            /* Test the first point only for a polygon */
            if (ref_ispoly || pass == 1) {
                curr = 0;
            } else {
                curr = 1;
            }
            while (curr != finish) {
                /*
                 * For lines we need to skip the first vertex if we are
                 * computing a position outward from the vertex since the
                 * first point doesn't create an angle with a preceeding
                 * line
                 */
                if (ref_ispoly || pass == 1 ||
                    (curr >= 0 && curr < ref_points-1)) {
                    /*
                     * For a polygon, since the first point is repeated we
                     * need to use two back from the last for the first test
                     */
                    if (ref_ispoly && curr == 0) {
                        prev = ref_points-2;
                    } else {
                        prev = curr - incr;
                    }
                    next = curr + incr;

                    if (pass == 1) {
                        /*
                         * Compute the number of steps along this segment
                         */
                        steps = (float)ceil(sqrt((double)
                                ((ref_x[curr]-ref_x[next])*
                                (ref_x[curr]-ref_x[next])+
                                (ref_y[curr]-ref_y[next])*
                                (ref_y[curr]-ref_y[next]))) /
                                step_incr
                            );
                        /*
                         * Always split each segement at least once
                         */
                        if (steps < 2) {
                            steps = 2;
                        }
                    } else {
                        steps = 2;
                    }

                    for (step = 1; step < steps; step++) {
                        /* Create two line segments from the points */
                        if (pass == 0) {
                            x_line[0] = ref_x[prev]; y_line[0] = ref_y[prev];
                            x_line[1] = ref_x[curr]; y_line[1] = ref_y[curr];
                            x_line[2] = ref_x[next]; y_line[2] = ref_y[next];
                        } else {
                            /*
                             * We use the curr and next for the segment, then
                             * compute the step along it.
                             */
                            step_x = ref_x[curr] +
                                    step/steps * (ref_x[next]-ref_x[curr]);
                            step_y = ref_y[curr] +
                                    step/steps * (ref_y[next]-ref_y[curr]);

                            x_line[0] = ref_x[curr]; y_line[0] = ref_y[curr];
                            x_line[1] = step_x;      y_line[1] = step_y;
                            x_line[2] = ref_x[next]; y_line[2] = ref_y[next];
                        }

                        /*
                         * See if the point along the polygon is inside the
                         * drawing area, if not, no sense using it as we would
                         * point to a part of the reference that is not visible
                         */
                        cgr_inpolywn(1, &(x_line[1]), &(y_line[1]),
                                5, placements->plot_area_x,
                                placements->plot_area_y,
                                0, &inside, iret
                            );

                        if (!inside) {
                            continue;
                        }

                        /*
                         * Calculate the location out from the center point
                         * by bisceting the angle created by the two lines
                         * If we are trying to place on the inside, flip the
                         * first and last points so that we point to the 
                         * inside of the reference, and use just the 1/2 diag
                         * of the object's bounding box as the offset
                         */
                        if (try_inside) {
                            x = x_line[0]; x_line[0] = x_line[2]; x_line[2] = x;
                            y = y_line[0]; y_line[0] = y_line[2]; y_line[2] = y;
                            cgr_bisectpt(x_line, y_line, diag, &x, &y, iret);
                        } else {
                            cgr_bisectpt(x_line, y_line, dist, &x, &y, iret);

                            /*
                             * Now that we now where we are going to put the
                             * object, switch to the center of the ref if we 
                             * are going to be checking the line to there
                             */
                            if (point_to_center && ref_cntrd_inside) {
                                x_line[1] = ref_cntr_x;
                                y_line[1] = ref_cntr_y;
                            }

                            /*
                             * If the ref was concave, then we have to worry
                             * if the line from the ref to the potential point
                             * crosses the ref more than once.  If so, its
                             * not an option.  A simple intersection test won't
                             * work since the start point of the line is
                             * guaranteed to touch the ref, that's why its
                             * there.  If we are pointing to the edge of the
                             * ref we need to run an intersect test against
                             * each edge of the ref *except* the one we are
                             * currently on.  If we are pointing to the
                             * centroid then we have to test all the edges and
                             * count the number of times we intersect.
                             */
                            if (ref_concave) {
                                x_line[0] = x;
                                y_line[0] = y;
                                if (point_to_center && ref_cntrd_inside) {
                                    excl_start = -1;
                                } else {
                                    excl_start = curr;
                                }

                                if (pass == 0 && 
                                    !(point_to_center && ref_cntrd_inside)) {
                                    if (curr == 0 && ref_ispoly) {
                                        excl_end = ref_points -1;
                                    } else {
                                        excl_end = curr;
                                    }
                                } else {
                                    excl_end = -1;
                                }

                                if (count_intersections(x_line, y_line, 
                                    ref_points, ref_x, ref_y, excl_start, 
                                    excl_end) > 1) {
                                    continue;
                                }
                            }
                        }

                        /*
                         * Compute the offset for this location, based on
                         * the original position of the object
                         */
                        x_off = x - obj_cntr_x;
                        y_off = y - obj_cntr_y;

                        /*
                         * Adjust the object to the new location
                         */
                        adjust_by_offset(x_off, y_off,
                                obj_points, obj_x, obj_y, obj_cntr_x,
                                obj_cntr_y, obj_bb, tmp_x, tmp_y,
                                &tmp_cntr_x, &tmp_cntr_y, tmp_bb
                            );

                        if (try_inside) {
                            /*
                             * When going to the center there was no line
                             * to check but we need to check if the object 
                             * completely fits in the reference, if it 
                             * doesn't we can go to the next location
                             */
                            cgr_objinpoly(obj_points, tmp_x, tmp_y, tmp_bb, 
                                    ref_points, ref_x, ref_y, ref_bb, &inside,
                                    iret
                                );

                            if (!inside) {
                                continue;
                            }
                        }

                        /*
                         * See if we are still inside the drawing area, if
                         * so add the location to the set to be sent back
                         */
                        cgr_inpolywn(obj_points, tmp_x, tmp_y,
                                5, placements->plot_area_x,
                                placements->plot_area_y,
                                0, &inside, iret
                            );
                        if (inside) {
                            /* add location to set */
                            if (locations == list_len) {
                                estimate = list_len * 2;
                                G_REALLOC(offset_list, CAPOffset, estimate,
                                        "extending location set");
                                G_REALLOC(ref_list, CAPOffset, estimate,
                                        "extending ref location set");
                                if (!offset_list || !ref_list) {
                                    *iret = -3;
                                    return;
                                }
                                list_len = estimate;
                            }

                            offset_list[locations].delta_x = x_off;
                            offset_list[locations].delta_y = y_off;
                            ref_list[locations].delta_x = x_line[1];
                            ref_list[locations].delta_y = y_line[1];

                            if (try_inside) {
                                center_count++;
                            }

                            locations++;
                        }
                    }
                }

                curr += incr;
                if ((curr == finish) && !ref_ispoly &&
                    place->both_sides_of_line && incr > 0) {
                    /*
                     * If we are allowed to place the object on either side
                     * of the ref when its a line, iterate back down the
                     * other side by decrementing from here to the start
                     */
                     incr = -1;
                     finish = 0;
                }
            }
        }
    }

    G_FREE(tmp_x, float);
    G_FREE(tmp_y, float);

    *count = locations;
    *centers = center_count;
    *offsets = offset_list;
    *ref_locations = ref_list;
}


static int compare_clutter(const void *a, const void *b)
/*****************************************************************************
 * compare_clutter
 *
 * Function to allow qsort to compare the clutter values of two placement
 * objects.  The comparison is done in the 'reverse' of a typical function
 * so the values come out in reverse order (largest to smallest). To ensure
 * things stay in the same relative position, if the clutter values are the
 * same the records are sorted by id value
 *
 * Input parameters:
 *  **a      PlaceInfoContainer  First placement to compare
 *  **b      PlaceInfoContainer  Second placement to compare
 *
 * Output parameters:
 *  None
 *
 * Return values
 *  int     -1 = b < a
 *           0 = b = a
 *           1 = b > a
 *
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    const PlaceInfoContainer  *ap = *((PlaceInfoContainer**)a);
    const PlaceInfoContainer  *bp = *((PlaceInfoContainer**)b);
/*---------------------------------------------------------------------*/

    if (!ap) {
        if (!bp) {
            return 0;
        } else {
            return 1;
        }
    } else {
        if (!bp) {
            return -1;
        }

        if (ap->clutter == bp->clutter) {
            return (bp->id > ap->id) ? 1 : -1;
        } else {
            return (bp->clutter > ap->clutter) ? 1 : -1;
        }
    }
}


static void
adjust_by_offset(float x_off, float y_off, int points, const float orig_x[],
    const float orig_y[], float orig_cntr_x, float orig_cntr_y, float orig_bb[],
    float adj_x[], float adj_y[], float *adj_cntr_x, float *adj_cntr_y,
    float adj_bb[])
/*****************************************************************************
 * adjust_by_offset
 *
 * Given the offset and definition of the object, adjust its location by the
 * offset and put the results in the fields given.
 * This function does *NOT* allocate any space, but expects the caller to
 * deal with all those issues.
 *
 * Input parameters:
 *  x_off        float      X offset
 *  y_off        float      Y offset
 *  points       int        Number of vertices in the array
 *  orig_x       float[]    X coordinates for verticies
 *  orig_y       float[]    Y coordinates for verticies
 *  orig_cntr_x  float      X coordinate of the center for the object
 *  orig_cntr_x  float      Y coordinate of the center for the object
 *  orig_bb      float[]    Bounding box for the object
 *
 * Output parameters:
 *  adj_x       float[]     Adjusted X coordinates for the verticies
 *  adj_y       float[]     Adjusted Y coordinates for the verticies
 *  adj_cntr_x  *float      X coordinate for the adjusted center
 *  adj_cntr_x  *float      Y coordinate for the adjusted center
 *  adj_bb      float[]     Adjusted bounding box definition
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int point;
/*---------------------------------------------------------------------*/

    for (point = 0; point < points; point++) {
        adj_x[point] = orig_x[point] + x_off;
        adj_y[point] = orig_y[point] + y_off;
    }
    adj_bb[0] = orig_bb[0] + x_off;
    adj_bb[1] = orig_bb[1] + x_off;
    adj_bb[2] = orig_bb[2] + y_off;
    adj_bb[3] = orig_bb[3] + y_off;
    *adj_cntr_x = orig_cntr_x + x_off;
    *adj_cntr_y = orig_cntr_y + y_off;
}


static int
count_intersections(float *line_x, float *line_y, int points, 
    const float *obj_x, const float *obj_y, int startpt, int endpt)
/*****************************************************************************
 * count_intersections
 *
 * Given a line segment and a set of verticies defining an object, see if the
 * count the number of times the line segment intersects any segment defined
 * in the list *EXCEPT* the one that use the given start or end points.  
 * (Start and end are both included when checking at a vertex in the object 
 * so that both lines are excluded). 
 *
 * Input parameters:
 *  line_x       float[2]   X verticies of the line
 *  line_y       float[2]   Y verticies of the line
 *  points       int        Number of points in the set to check
 *  obj_x        float[]    X coordinates for object verticies
 *  obj_y        float[]    Y coordinates for object verticies
 *  startpt      int        Index of start for the segment to exclude
 *  endpt        int        Index of end for the segment to exclude
 *
 * Output parameters:
 *  None
 *
 * Return values
 *  int         Count of the number of intersections
 *
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    float   int_x, int_y, seg_x[2], seg_y[2];
    int     check_all, intersects, iret, point, total;
/*---------------------------------------------------------------------*/

    check_all =  (startpt == endpt && startpt == -1) ? 1 : 0;
    intersects = 0;
    total = 0;

    /* 
     * If not checking all lines, then assume that we will be intersecting the
     * excluded line 
     */
    if (!check_all) {
        total++;
    }

    for (point = 0; !intersects && point < points-1; point++) {
        if ((startpt == -1 || point != startpt) && (endpt == -1 || 
            endpt != point+1)) {
            seg_x[0] = obj_x[point];
            seg_y[0] = obj_y[point];
            seg_x[1] = obj_x[point+1];
            seg_y[1] = obj_y[point+1];
            cgr_csegint(line_x, line_y, seg_x, seg_y, &int_x, &int_y,
                    &intersects, &iret);
            if (intersects) {
                total++;
                if (check_all) {
                    /*
                     * If the line intersects at the end vertex and we are 
                     * checking all the lines, ignore this one since we will 
                     * count it again at the start of the next line.
                     */
                    if (fabs(int_x - seg_x[1]) < 0.00001 && 
                        fabs(int_x - seg_y[1]) < 0.00001) {
                        total--;
                    }
                    intersects = 0;
                }
            }
        }
    }

    return total;
}
