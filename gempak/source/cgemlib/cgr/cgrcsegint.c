#include "geminc.h"
#include "gemprm.h"

void cgr_csegint (float *xin1, float *yin1, float *xin2, float *yin2, 
                  float *xint, float *yint, int *intrsct, int *iret)
/************************************************************************
 * cgr_csegint                                                          *
 *                                                                      *
 * This function implements the core features of the algorithm to accept*
 * two line segments and determines if they intersect one another.      *
 * Note that all points are assumed to be in the same cartesian         *
 * coordinate system, not earth coordinates.                            *
 * Note that if two line segments are extended as lines, they will      *
 * always intersect (unless they are parallel, ie. their slopes are     *
 * equal).  This intersecting point is returned regardless of whether   *
 * it falls on the segments themselves.  If the segments are parallel,  *
 * the intersecting point is (RMISSD,RMISSD).                           *
 *                                                                      *
 * cgr_csegint ( xin1, yin1, xin2, yin2, xint, yint, intrsct, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *xin1   float   X-coordinate of endpoints for segment #1        *
 *      *yin1   float   Y-coordinate of endpoints for segment #1        *
 *      *xin2   float   X-coordinate of endpoints for segment #2        *
 *      *yin2   float   Y-coordinate of endpoints for segment #2        *
 *                                                                      *
 * Output parameters:                                                   *
 *      *xint   float   X-coordinate of intersecting point              *
 *      *yint   float   Y-coordinate of intersecting point              *
 *      *intrsct int    Result:                                         *
 *                          0-FALSE (the segments do not intersect),    *
 *                          1-TRUE (the segments intersect)             *
 *      *iret    int    Return code                                     *
 *                          0 = Function was successful                 *
 *                         -5 = Invalid pointer in arguments            *
 **                                                                     *
 * Log:                                                                 *
 * S.Danz/AWC            2/06   Created
 ***********************************************************************/
{
    float   m1, b1, m2, b2;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (!xin1 || !xin2 || !yin1 || !yin2 || !xint || !yint || !intrsct) {
        *iret = -5;
        return;
    }

    *intrsct = 0;
    *xint = RMISSD;
    *yint = RMISSD;

    /*
     *  Check for vertical first segment and compute (x,y) intersect.
     */

    if ( G_DIFFT(xin1[0], xin1[1], GDIFFD) ) {

        *xint = xin1[0];
        if ( G_DIFFT(xin2[0], xin2[1], GDIFFD) )  return;
        m2 = (yin2[1]-yin2[0]) / (xin2[1]-xin2[0]);
        b2 = yin2[0] - m2 * xin2[0];
        *yint = m2 * *xint + b2;

    }

    /*
     *  Check for vertical second segment and compute (x,y) intersect.
     */

    else if ( G_DIFFT(xin2[0], xin2[1], GDIFFD) ) {

        *xint = xin2[0];
        if ( G_DIFFT(xin1[0], xin1[1], GDIFFD) )  return;
        m1 = (yin1[1]-yin1[0]) / (xin1[1]-xin1[0]);
        b1 = yin1[0] - m1 * xin1[0];
        *yint = m1 * *xint + b1;

    }

    /*
     *  Finally compute (x,y) intersect for all other cases.
     */

    else {

        m1 = (yin1[1]-yin1[0]) / (xin1[1]-xin1[0]);
        b1 = yin1[0] - m1 * xin1[0];

        m2 = (yin2[1]-yin2[0]) / (xin2[1]-xin2[0]);
        b2 = yin2[0] - m2 * xin2[0];

        if ( G_DIFFT(m1, m2, GDIFFD) )  {
            /*
             * The lines are parallel, but they might be along the same line
             * in which case they do intersect.  If the y intercepts are the
             * same then just use the endpoint of line 1 as the intersection.
             */
            if ( G_DIFFT(b1, b2, GDIFFD) ) {
                *xint = xin1[1];
                *yint = yin1[1];
            } else {
                *xint = RMISSD;
                *yint = RMISSD;
            }
        }
        else  {
            if ( G_DIFFT(m1, 0.0F, GDIFFD) )  {
                *xint = ( b2 - yin1[0] ) / ( - m2 );
                *yint = yin1[0];
            }
            else if ( G_DIFFT(m2, 0.0F, GDIFFD) )  {
                *xint = ( yin2[0] - b1 ) / ( m1 );
                *yint = yin2[0];
            }
            else  {
                *xint = ( b2 - b1 ) / ( m1 - m2 );
                *yint = m1 * *xint + b1;
            }
        }

    }

    /*
     *  Check if intersecting point is within each segment's bounds.
     */

    if ( ERMISS(*xint) || ERMISS(*yint) )  return;

    if ( *xint < G_MIN(xin1[0],xin1[1]) || *xint > G_MAX(xin1[0],xin1[1]) )  return;
    if ( *xint < G_MIN(xin2[0],xin2[1]) || *xint > G_MAX(xin2[0],xin2[1]) )  return;
    if ( *yint < G_MIN(yin1[0],yin1[1]) || *yint > G_MAX(yin1[0],yin1[1]) )  return;
    if ( *yint < G_MIN(yin2[0],yin2[1]) || *yint > G_MAX(yin2[0],yin2[1]) )  return;

    *intrsct = 1;

    return;

}
