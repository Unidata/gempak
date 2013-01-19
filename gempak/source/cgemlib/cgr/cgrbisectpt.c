#include "geminc.h"
#include "gemprm.h"

void cgr_bisectpt(float *x_line, float *y_line, float distance, float *x, 
                  float *y, int *iret)
/*****************************************************************************
 * cgr_bisectpt
 *
 * With the line defined by the three points given, ABC, compute a point that
 * is the given distance away from B, projected down the bisected angle 
 * between AB and BC.  The point will be placed on the 'right' side of
 * of the line, as if the points were defined in a CCW direction around a 
 * polygon.  Note that all coordinates are assumed to be in the same cartesian
 * coordinate system, not earth coordinates.
 *
 * Input parameters:
 *  x_line[3]   float   x coordinates of the line
 *  y_line[3]   float   y coordinates of the line
 *  distance    float   distance away from the 2nd point to compute
 *
 * Output parameters:
 *  *x          float   x coordinate of the computed point
 *  *y          float   y coordinate of the computed point
 *  *iret       int     Return code
 *                          0 = Function successful
 *                         -5 = Invalid pointer in arguments
 *                         -6 = Multiple consecutive points at the same location
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    float   vector_x[3], vector_y[3], length[3];
    int     concave;
/*---------------------------------------------------------------------*/

    *iret = 0;
    if (!x_line  || !y_line || !x || !y) {
        *iret = -5;
        return;
    }

    *x = RMISSD;
    *y = RMISSD;

    /* Normalize the vectors from A->B and from B->C */
    vector_x[0] = x_line[0] - x_line[1];
    vector_y[0] = y_line[0] - y_line[1];
    vector_x[1] = x_line[2] - x_line[1];
    vector_y[1] = y_line[2] - y_line[1];

    length[0] = (float)sqrt((double)(vector_x[0]*vector_x[0] + 
		vector_y[0]*vector_y[0]));
    length[1] = (float)sqrt((double)(vector_x[1]*vector_x[1] + 
		vector_y[1]*vector_y[1]));

    /* 
     * If the length of either segment is 0, we don't have two lines so 
     * return with an error
     */
    if (G_DIFFT(length[0], 0.0, GDIFFD) || G_DIFFT(length[1], 0.0, GDIFFD) ) {
        *iret = -6;
        return;
    }

    vector_x[0] = vector_x[0] / length[0];
    vector_y[0] = vector_y[0] / length[0];

    vector_x[1] = vector_x[1] / length[1];
    vector_y[1] = vector_y[1] / length[1];

    /* Now sum the vectors to get the bisecting vector between them */
    vector_x[2] = vector_x[0] + vector_x[1];
    vector_y[2] = vector_y[0] + vector_y[1];

    cgr_concave(3, x_line, y_line, &concave, iret);
    /* 
     * Test if the angle ABC is convex/concave.  If concave, we need to
     * to 'flip' the bisecting line over since it will be pointing the
     * wrong way.
     */

    if (concave) {
        vector_x[2] *= -1.0;
        vector_y[2] *= -1.0;
    }

    length[2] = (float)sqrt((double)(vector_x[2]*vector_x[2] + 
		vector_y[2]*vector_y[2]));

    /* 
     * If the length of the bisecting vector is 0, then ABC line along
     * a line and we need to compute the perpendicular to the line outward
     * from point B.  Thanks to floating point, best we can do is an est.
     * compare to zero
     */

    if (fabs((double)(length[2])) > 0.00001) {
        /* 
         * Since the result is at a point w.r.t. 0,0 from the normalized 
         * vectors, adjust it now back to the original coordinate system
         * as an offset from point B
         */
        *x = x_line[1] - (distance/length[2]) * vector_x[2];
        *y = y_line[1] - (distance/length[2]) * vector_y[2];
    } else {
        /*
         * Compute the negative of the reciprocal of the slope of line between A->C
         * so we have the slope for the perpendicular to the line, unless the result
         * is vertical (which means the original line is horizontal), then we have 
         * different work to do
         */
        if ( !G_DIFFT(y_line[0], y_line[2], GDIFFD) ) {
            float slope = ((x_line[2] - x_line[0]) / (y_line[2] - y_line[0])) * -1.0;
            float deltax = (float)sqrt((double)((distance * distance) / 
		(1 + (slope * slope))));
            float deltay = slope * deltax;

            /*
             * From the slope and the distance we have the magnitude of how far
             * we need to be away from the line, now we need to know which
             * side of the line we need to be on
             */
            if (slope > 0) {
                if (x_line[0] < x_line[2]) {
                    *x = x_line[1] - deltax;
                } else {
                    *x = x_line[1] + deltax;
                }
            } else {
                if (x_line[0] < x_line[2]) {
                    *x = x_line[1] + deltax;
                } else if (x_line[0] > x_line[2]) {
                    *x = x_line[1] - deltax;
                } else {
                    if (y_line[0] < y_line[2]) {
                        *x = x_line[1] + deltax;
                    } else {
                        *x = x_line[1] - deltax;
                    }
                }
            }

            if (y_line[0] < y_line[2]) {
                *y = y_line[1] + deltay;
            } else {
                *y = y_line[1] - deltay;
            }
        } else {
            /* Its a horizontal line, just move up/down from point B */
            if (x_line[0] < x_line[2]) {
                *y = y_line[1] - distance;
            } else {
                *y = y_line[1] + distance;
            }
            *x = x_line[1];
        }
    }
}
