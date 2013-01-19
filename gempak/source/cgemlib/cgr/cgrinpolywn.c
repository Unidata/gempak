#include "geminc.h"
#include "gemprm.h"

/*****************************************************************************
 * Function prototypes for private static utility functions
 ****************************************************************************/
static float
left_of_line(float line_p1_x, float line_p1_y, float line_p2_x, float line_p2_y, 
             float point_to_check_x, float point_to_check_y);


void cgr_inpolywn(const int points, const float *pt_x, const float *pt_y, 
                  const int polypoints, const float *poly_x, const float *poly_y, 
                  const int check_all, int *inside, int *iret)
/*****************************************************************************
 * cgr_inpolywn
 *
 * This function accepts a set of points and the vertices for a polygon and 
 * determines whether if the points are inside or outside the polygon.  A
 * point that is on an edge or vertex is considered to be *inside*.
 * 
 * The polygon should be CLOSED (the first and last points should be identical)
 * and should be defined in a CCW direction.  All coordinates are assumed to be
 * in the same cartesian coordinate system, not earth coordinates.
 *
 * Based the wn_PnPoly() winding number function by Dan Sunday, unpublished 
 * but available at:
 * http://softsurfer.com/Archive/algorithm_0103/algorithm_0103.htm
 *
 * Computes the winding number for a point w.r.t. the polygon supplied
 * If the resulting winding number is 0, then the point is outside the polygon
 *
 * This is an improved (at a loss in clarity) implementation of the general 
 * winding number algorithm described in "Computer Graphics (2nd Edition in C)"
 * by James Foley, Andries van Dam, Steven Feiner & John Hughes.  This
 * version improves upon the original in that the descriminator for the
 * left/right of line test is able to distinguish if the point lies *on*
 * the line, which the general version is not able to do.  It also replaces the
 * divide in the general algorithm with a multiply, which on some systems is 
 * faster.
 *
 * The original algorithm only checks > or < 0.0 from the result of left_of_line
 * since they consider *on* the polygon _not_ to be inside.  For our use, 
 * on == inside so the result comparisons are for >= or <= 0.0
 *
 * Input parameters:
 *  points      int     number of points to be checked
 *  *pt_x       float   x coordinates for each point
 *  *pt_y       float   y coordinates for each point
 *  polypoints  int     number of verticies in the polygon
 *  *poly_x     float   x coordinates for each vertex
 *  *poly_y     float   y coordinates for each vertex
 *  check_all   int     flag to indicate if all points should be checked, or
 *                      if the algorithm should stop on the first point 
 *                      outside the polygon.  
 *                          0 = stop at first
 *                          1 = check all
 *
 * Output parameters:
 *  *inside     int     Indicator if the point(s) were inside the polygon
 *                          0 = outside
 *                          1 = inside
 *                      If check_all is 0, then inside is expected to be 
 *                      a pointer to a single integer.
 *                      If check_all is 1, then inside is expected to be 
 *                      a pointer to an array the same length as the number
 *                      of points so that the state of each point can be
 *                      returned
 *                      
 *  *iret       int     Return code
 *                          0 = No errrors
 *                         -5 = Invalid pointer in arguments
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int winding_number, curr, next, point;
/*---------------------------------------------------------------------*/

    *iret = 0;
    *inside = 1;

    if (!pt_x || !pt_y || !poly_x || !poly_y || !inside) {
        *iret = -5;
        return;
    }

    /*
     * for all the points given
     */
    for (point=0; point < points && *inside; point++) {
        /*
         * loop through all edges of the polygon
         */
        winding_number = 0;
        for (curr=0, next=1; next < polypoints; next++, curr++) {   /* edge from V[curr] to V[next] */
            if (poly_y[curr] <= pt_y[point]) {          /* start y <= P.y */
                if (poly_y[next] > pt_y[point])         /* an upward crossing */
                    if (left_of_line( poly_x[curr], poly_y[curr], 
                                      poly_x[next], poly_y[next], 
                                      pt_x[point], pt_y[point]) >= 0.0)  /* P left of OR ON an edge */
                        ++winding_number;               /* have a valid up intersect */
            } else {                                    /* start y > P.y (no test needed) */
                if (poly_y[next] <= pt_y[point])        /* a dowinding_number crossing */
                    if (left_of_line( poly_x[curr], poly_y[curr], 
                                      poly_x[next], poly_y[next], 
                                      pt_x[point], pt_y[point]) <= 0.0)  /* P right of OR ON an edge */
                        --winding_number;               /* have a valid down intersect */
            }
        }

        if (winding_number == 0) {
            *inside = 0;
        }

        /*
         * If we are checking everthing, then increment to the next result 
         * location if there is one and initialize it
         */
        if (check_all && (point < points-1)) {
            inside++;
            *inside = 1;
        }
    }
}

/*****************************************************************************
 * left_of_line
 *
 * Utility function used by the winding number algorithm for determinining if a
 * point is inside/outside a polygon. Reference implementation is at: 
 * http://www.geometryalgorithms.com/Archive/algorithm_0103/algorithm_0103.htm#isLeft()
 *
 * This routine is used to determine if the given point is to the left, right 
 * or on the given line segment.
 *
 * Returns:
 *          > 0 : point is to the left of the line
 *          = 0 : point is on the line
 *          < 0 : point is to the right of the line
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
static float
left_of_line(float line_p1_x, float line_p1_y, float line_p2_x, float line_p2_y, 
             float point_to_check_x, float point_to_check_y)
{
    return ( 
            (line_p2_x - line_p1_x) * 
            (point_to_check_y - line_p1_y) - 
            (point_to_check_x - line_p1_x) * 
            (line_p2_y - line_p1_y) 
        );
}
