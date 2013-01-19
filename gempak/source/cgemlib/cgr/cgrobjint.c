#include "geminc.h"
#include "gemprm.h"

void cgr_objint(const int obj1_pts, const float *obj1_x, const float *obj1_y, 
                const float *obj1_bb, const int obj2_pts, const float *obj2_x,
                const float *obj2_y, const float *obj2_bb, int *intersects, 
                int *iret)
/*****************************************************************************
 * cgr_objint
 * This function takes in two arbitrary sets of points (lines or polygons)
 * determines if they intersect each other.  If either set of points 
 * represents a polygon, the polygon should be CLOSED (the first and last 
 * points should be identical) and should be defined in a CCW direction.
 * For the purposes of this function, inclusion equals intersection, so if
 * either object is a polygon that completely surrounds the other, this 
 * will also be considered intersecting.
 *
 * Input parameters:
 *  obj1_pts    int     number of coordinates for the first object
 *  *obj1_x     float   x coordinates of the first object
 *  *obj1_y     float   y coordinates of the first object
 *  *obj1_bb    float   minx,maxx,miny,maxy values for bounding box of
 *                      first object, NOTE: if NULL it will be computed
 *  obj2_pts    int     number of coordinates for the second object
 *  *obj2_x     float   x coordinates of the second object
 *  *obj2_y     float   y coordinates of the second object
 *  *obj2_bb    float   minx,maxx,miny,maxy values for bounding box of
 *                      second object, NOTE: if NULL it will be computed
 *
 * Output parameters:
 *  *intersects int     boolean indicator if the two objects intersect
 *  *iret       int     Return code
 *                       0 = Function successful
 *                      -5 = Invalid pointer in arguments
 *                      -7 = Not enough points in either object to compute result
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    float   x1[2], y1[2], x2[2], y2[2], xint, yint, obj1_min_x;
    float   obj1_max_x, obj1_min_y, obj1_max_y, obj2_min_x, obj2_max_x;
    float   obj2_min_y, obj2_max_y;
    int     obj1_index, obj2_index, obj1_poly, obj2_poly;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (!obj1_x || !obj1_y || !obj2_x || !obj2_y || !intersects) {
        *iret = -5;
        return;
    }

    if (!obj1_pts || !obj2_pts) {
        *iret = -7;
        return;
    }
    obj1_poly = 0;
    obj2_poly = 0;

    /*
     * Compute the bounding box min/max for both objects if not sent in by the
     * caller
     */
    if (obj1_bb) {
        obj1_min_x = obj1_bb[0];
        obj1_max_x = obj1_bb[1];
        obj1_min_y = obj1_bb[2];
        obj1_max_y = obj1_bb[3];
    } else {
        obj1_min_x = obj1_x[0];
        obj1_max_x = obj1_x[0];
        obj1_min_y = obj1_y[0];
        obj1_max_y = obj1_y[0];
        for (obj1_index = 1; obj1_index < obj1_pts; obj1_index++) {
            if (obj1_x[obj1_index] > obj1_max_x)
                obj1_max_x = obj1_x[obj1_index];
            if (obj1_x[obj1_index] < obj1_min_x)
                obj1_min_x = obj1_x[obj1_index];

            if (obj1_y[obj1_index] > obj1_max_y)
                obj1_max_y = obj1_y[obj1_index];
            if (obj1_y[obj1_index] < obj1_min_y)
                obj1_min_y = obj1_y[obj1_index];
        }
    }

    if (obj2_bb) {
        obj2_min_x = obj2_bb[0];
        obj2_max_x = obj2_bb[1];
        obj2_min_y = obj2_bb[2];
        obj2_max_y = obj2_bb[3];
    } else {
        obj2_min_x = obj2_x[0];
        obj2_max_x = obj2_x[0];
        obj2_min_y = obj2_y[0];
        obj2_max_y = obj2_y[0];
        for (obj2_index = 1; obj2_index < obj2_pts; obj2_index++) {
            if (obj2_x[obj2_index] > obj2_max_x)
                obj2_max_x = obj2_x[obj2_index];
            if (obj2_x[obj2_index] < obj2_min_x)
                obj2_min_x = obj2_x[obj2_index];

            if (obj2_y[obj2_index] > obj2_max_y)
                obj2_max_y = obj2_y[obj2_index];
            if (obj2_y[obj2_index] < obj2_min_y)
                obj2_min_y = obj2_y[obj2_index];
        }
    }

    /*
     * if the object's bounding box don't intersect, then there is no way
     * the objects can
     */
    if (obj1_max_x < obj2_min_x || obj1_min_x > obj2_max_x) {
        *intersects = 0;
        return;
    }

    if (obj1_max_y < obj2_min_y || obj1_min_y > obj2_max_y) {
        *intersects = 0;
        return;
    }

    /*
     * See which (if either) object is a polygon
     */
    if (obj1_pts > 1 && (G_DIFFT(obj1_x[0], obj1_x[obj1_pts-1], GDIFFD) ) &&
			(G_DIFFT(obj1_y[0], obj1_y[obj1_pts-1], GDIFFD) )) {
        obj1_poly = 1;
    }

    if (obj2_pts > 1 && (G_DIFFT(obj2_x[0], obj2_x[obj2_pts-1], GDIFFD) ) &&
			(G_DIFFT(obj2_y[0], obj2_y[obj2_pts-1], GDIFFD) )) {
        obj2_poly = 1;
    }

    /*
     * No matter if they are polygons or not, we will need to check if any line
     * segments between the two objects itersect. Start by saying they don't
     * intersect and as soon as they do, stop
     */
    *intersects = 0;
    for (obj1_index = 1; obj1_index < obj1_pts && !*intersects; obj1_index++) {
        x1[0] = obj1_x[obj1_index - 1];
        y1[0] = obj1_y[obj1_index - 1];

        x1[1] = obj1_x[obj1_index];
        y1[1] = obj1_y[obj1_index];
        for (obj2_index = 1; obj2_index < obj2_pts && !*intersects; obj2_index++) {
            x2[0] = obj2_x[obj2_index - 1];
            y2[0] = obj2_y[obj2_index - 1];

            x2[1] = obj2_x[obj2_index];
            y2[1] = obj2_y[obj2_index];

            cgr_csegint(x1, y1, x2, y2, &xint, &yint, intersects, iret);
        }
    }

    /*
     * If we made it through all that and they don't intersect, then the last
     * check only applies if one of the objects is a polygon.  We will consider
     * an object *completely* inside a polygon as intersecting as well
     */

    if (obj1_poly && !*intersects) {
        cgr_inpolywn(obj2_pts, obj2_x, obj2_y, obj1_pts, obj1_x, obj1_y, 
                  0, intersects, iret);
    }

    if (obj2_poly && !*intersects) {
        cgr_inpolywn(obj1_pts, obj1_x, obj1_y, obj2_pts, obj2_x, obj2_y, 
                  0, intersects, iret);
    }

    return;
}
