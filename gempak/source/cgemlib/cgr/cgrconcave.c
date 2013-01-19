#include "geminc.h"
#include "gemprm.h"

void cgr_concave(int points, const float *x, const float *y, 
        int *concave, int *iret)
/*****************************************************************************
 * cgr_concave
 *
 * With the set of points defined, determine if any angle described in the 
 * set is concave.  The points are expected to be listed in a CCW direction
 * and the 'right' side of the line is considered the outside. Note that 
 * all coordinates are assumed to be in the same cartesian coordinate system,
 * not earth coordinates. Also, a polygon should be CLOSED (the first and last 
 * points should be identical).
 *
 * The method used to determine if an angle ABC is convex/concave comes from
 * Comp.Graphics.Algorithms FAQ, Section 2.07 which describes the method for
 * determining the orientaition as computing the signed area of a polygon, 
 * and if it is negative, then the polygon is concave. If we just deal with 
 * 3 points in sequence at a time, can can connect A and C to make a triangle,
 * and compute its signed area.  If the value is negative, ABC is concave.
 *
 * Input parameters:
 *  points      int     number of coordinates for the object
 *  *x          float   x coordinates of the object
 *  *y          float   y coordinates of the object
 *
 * Output parameters:
 *  *concave    int     Result: 1 concave, 0 not concave
 *  *iret       int     Return code
 *                          0 = Function successful
 *                         -5 = Invalid pointer in arguments
 *                         -7 = Too few points to determine if concave.
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int prev, next, curr, finish, ispoly;
    float area;
/*---------------------------------------------------------------------*/

    if (!x || !y || !concave) {
        *iret = -5;
        return;
    }

    *concave = 0;
    if (points < 3) {
        *iret = -7;
        return;
    }

    /* 
     * Check if a polygon, stop one earlier for a line since we don't 
     * wrap around for that
     */
    if (((points > 2) && ( G_DIFFT(x[0], x[points-1], GDIFFD) )
		      && ( G_DIFFT(y[0], y[points-1], GDIFFD) ))) {
        finish = points;
        ispoly = 1;
    } else {
        finish = points-1;
        ispoly = 0;
    }

    *iret = 0;
    for (curr = 1; !*concave && curr < finish; curr++) {
        prev = curr - 1;
        /*
         * For a polygon, since the first point is repeated we
         * need to use two back from the last for the first test
         */
        if (ispoly && curr == finish-1) {
            next = 1;
        } else {
            next = curr + 1;
        }

        /* 
         * Test if the angle ABC is convex/concave.  If concave, we need to
         * to 'flip' the bisecting line over since it will be pointing the
         * wrong way.  From the Comp.Graphics.Algorithms FAQ, Section 2.07
         * the orientaition can be determined by computing the signed area
         * of a polygon, and if it is negative it is concave. Since we
         * are only dealing with 3 points on a line, can can connect A and
         * C to make a triangle, and compute its signed area.  If the value
         * is negative, ABC is concave.
         */
        area =  x[curr]*y[next] -
                x[next]*y[curr] +
                x[prev]*y[curr] -
                x[curr]*y[prev] +
                x[next]*y[prev] -
                x[prev]*y[next];

        if (area < 0) {
            *concave = 1;
        }
    }

    return;
}
