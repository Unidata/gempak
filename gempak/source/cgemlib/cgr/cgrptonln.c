#include "geminc.h"
#include "gemprm.h"

void cgr_ptonln ( float px, float py, float qx, float qy, float tx,
                  float ty, int *iside, int *iret )
/************************************************************************
 * cgr_ptonln                                                           *
 *                                                                      *
 * This function, given a line through P:(px,py) Q: (qx,qy) and using 	*
 * test	point T: (tx,ty),  determines if the test point is on the line  *
 * and if it is on the line, which side of the line it lies.  This	* 
 * computation is from "A fast 2D Point-on-Line Test", A. Glassner,	*
 * Graphics Gems, Academic Press, Inc, 1998, pp 49-50.			* 
 *                                                                      *
 * cgr_ptonln ( px, py, qx, qy, tx, ty, iside, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      px              float          X coordinate of start of line	* 
 *      py              float          Y coordinate of start of line	* 
 *      qx              float          X coordinate of end of line	* 
 *      qy              float          Y coordinate of end of line	* 
 *      tx              float          X coordinate of test point	* 
 *      ty              float          Y coordinate of test point	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iside       	int 		Side of line			*
 *                                         0 = test pt not on the line	*
 *                                         1 = test pt left of pt p 	*
 *                                         2 = test pt inbetween p & q  *
 *                                         3 = test pt right of pt q	*
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC	 4/02	Created					*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;

   /*
    * The test point is not on the line through  <--P--Q-->
    */

    if ( G_ABS ( ( qy-py) * (tx-px) - (ty-py) * (qx-px) ) >=
         ( G_MAX ( G_ABS (qx-px), G_ABS (qy-py) ) ) ) {
	*iside = 0;
        return;
    }

   /*
    * The test point is on the open ray ending at P: <--P
    */

    if ( ( (qx<px) && (px<tx) ) || ( (qy<py) && (py<ty) ) ) {
	*iside = 1;
        return;
    }

   /*
    * The test point is on the open ray ending at P: <--P
    */

    if ( ( (tx<px) && (px<qx) ) || ( (ty<py) && (py<qy) ) ) {
	*iside = 1;
        return;
    }

   /*
    * The test point is on the open ray beginning at Q: Q-->
    */

    if ( ( (px<qx) && (qx<tx) ) || ( (py<qy) && (qy<ty) ) ) {
	*iside = 3;
        return;
    }

   /*
    * The test point is on the open ray beginning at Q: Q-->
    */

    if ( ( (tx<qx) && (qx<px) ) || ( (ty<qy) && (qy<py) ) ) {
	*iside = 3;
        return;
    }

   /*
    * The test point is on the closed interior along:  P--Q 
    */

    *iside = 2;
    return;

}
