#include "geminc.h"
#include "gemprm.h"

void cgr_objinpoly(const int objpoints, const float *obj_x, const float *obj_y, 
                   const float *obj_bb, const int polypoints, 
                   const float *poly_x, const float *poly_y, const float *poly_bb,
                   int *inside, int *iret)
/*******************************************************************************
 * cgr_objinpoly
 *
 * This function determines if a given object is completely contained within
 * the polygon given.  The polygon is *required* to be 'CLOSED',i.e., the first
 * vertex MUST BE repeated.  If the first object is a polygon, to have all 
 * segments checked it will need to be closed by the caller (i.e. the first
 * point MUST BE repeated).
 *
 * To determine if an object is *completely* inside a polygon:
 *      1) the bounding boxes are checked, and if that passes then 
 *      2) the points of the objects are verified to be within the polygon
 *         if that passes then,
 *      3) each edge of the object is checked for intersection against each
 *         edge of the polygon.  If any interesect, then the test fails.
 * 
 * Input parameters:
 *  objpoints   int     number of coordinates for the object
 *  *obj_x      float   x coordinates of the object
 *  *obj_y      float   y coordinates of the object
 *  *obj_bb     float   minx,maxx,miny,maxy values for bounding box of
 *                      object, NOTE: if NULL it will be computed
 *  polypoints  int     number of coordinates for the polygon
 *  *poly_x     float   x coordinates of the polygon
 *  *poly_y     float   y coordinates of the polygon
 *  *poly_bb    float   minx,maxx,miny,maxy values for bounding box of
 *                      polygon, NOTE: if NULL it will be computed
 *
 * Output parameters:
 *  *inside     int     Result: 1 completely inside, 0 not inside
 *  *iret       int     Return code
 *                       0 = Function successful
 *                      -5 = Invalid pointer in arguments
 *                      -7 = Not enough points in either object to compute result
 *                      -8 = Second object is not a polygon
 **
 * Log:
 *  S.W.Danz/AWC    02/06   Created
 *  S.W.Danz/AWC    03/07   Fix test for endpoint touching polygon
 ******************************************************************************/
{
    float   obj_min_x, obj_max_x, obj_min_y, obj_max_y, poly_min_x, poly_max_x;
    float   poly_min_y, poly_max_y;
    float   x1[2], x2[2], y1[2], y2[2], xint, yint;
    int     obj_index, poly_index, segment_int;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (!obj_x || !obj_y || !poly_x || !poly_y || !inside) {
        *iret = -5;
        return;
    }

    if (!objpoints || !polypoints) {
        *iret = -7;
        return;
    }

    if (( !G_DIFFT(poly_x[0], poly_x[polypoints-1], GDIFFD)) ||
        ( !G_DIFFT(poly_y[0], poly_y[polypoints-1], GDIFFD))) {
        *iret = -8;
        return;
    }

    *inside = 1;

    /*
     * Compute the bounding box min/max for both objects if not sent in by the
     * caller
     */
    if (obj_bb) {
        obj_min_x = obj_bb[0];
        obj_max_x = obj_bb[1];
        obj_min_y = obj_bb[2];
        obj_max_y = obj_bb[3];
    } else {
        obj_min_x = obj_x[0];
        obj_max_x = obj_x[0];
        obj_min_y = obj_y[0];
        obj_max_y = obj_y[0];
        for (obj_index = 1; obj_index < objpoints; obj_index++) {
            if (obj_x[obj_index] > obj_max_x)
                obj_max_x = obj_x[obj_index];
            if (obj_x[obj_index] < obj_min_x)
                obj_min_x = obj_x[obj_index];

            if (obj_y[obj_index] > obj_max_y)
                obj_max_y = obj_y[obj_index];
            if (obj_y[obj_index] < obj_min_y)
                obj_min_y = obj_y[obj_index];
        }
    }

    if (poly_bb) {
        poly_min_x = poly_bb[0];
        poly_max_x = poly_bb[1];
        poly_min_y = poly_bb[2];
        poly_max_y = poly_bb[3];
    } else {
        poly_min_x = poly_x[0];
        poly_max_x = poly_x[0];
        poly_min_y = poly_y[0];
        poly_max_y = poly_y[0];
        for (poly_index = 1; poly_index < polypoints; poly_index++) {
            if (poly_x[poly_index] > poly_max_x)
                poly_max_x = poly_x[poly_index];
            if (poly_x[poly_index] < poly_min_x)
                poly_min_x = poly_x[poly_index];

            if (poly_y[poly_index] > poly_max_y)
                poly_max_y = poly_y[poly_index];
            if (poly_y[poly_index] < poly_min_y)
                poly_min_y = poly_y[poly_index];
        }
    }

    /*
     * if the object's bounding box isn't completely within the polygon's,
     * it can't be inside since its going to clip or be completely outside
     */
    if (obj_min_x < poly_min_x || poly_max_x < obj_max_x)
        *inside = 0;

    if (obj_min_y < poly_min_y || poly_max_y < obj_max_y)
        *inside = 0;

    /*
     * Now, if we are inside the bounding box, make sure all the points are 
     * inside the polygon.  Its possible that the bounding boxes overlap, 
     * but the objects don't.
     */
    cgr_inpolywn(objpoints, obj_x, obj_y, polypoints, poly_x, poly_y, 0, inside, iret);

    /* 
     * Now see if see any edge of the object clips the polygon if we are still
     * a possibility.  Its needed for things like concave edges of the polygon
     */
    for (obj_index = 1; obj_index < objpoints && *inside; obj_index++) {
        x1[0] = obj_x[obj_index - 1];
        y1[0] = obj_y[obj_index - 1];

        x1[1] = obj_x[obj_index];
        y1[1] = obj_y[obj_index];
        for (poly_index = 1; poly_index < polypoints && *inside; poly_index++) {
            x2[0] = poly_x[poly_index - 1];
            y2[0] = poly_y[poly_index - 1];

            x2[1] = poly_x[poly_index];
            y2[1] = poly_y[poly_index];
            cgr_csegint(x1, y1, x2, y2, &xint, &yint, &segment_int, iret);

            /*
             * Segments intersect, so we can't be completely inside unless
             * one end or the other is *exactly* on the edge of the polygon
             * so check the xint and yint and if they are == to an endpoint
             * then consider it 'ok'.
             */
            if (segment_int) { 
                if (!( (G_DIFFT(xint, x1[0], GDIFFD) && G_DIFFT(yint, y1[0], GDIFFD) ) ||
                       (G_DIFFT(xint, x1[1], GDIFFD) && G_DIFFT(yint, y1[1], GDIFFD) ))) {
                    *inside = 0;
                }
            }
        }
    }
}
