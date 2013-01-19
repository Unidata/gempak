#include "cmdcmn.h"

void cmd_obgetcntrd(CMDObject object, float *x, float *y, int *iret)
/*****************************************************************************
 * cmd_obgetcntrd 
 *
 * Retrieve the centroid for the given object
 *
 * Input parameters:
 *  object      CMDObject   what we would like the bounding box for
 *
 * Output parameters:
 *  *x          float   x coordinate of the centroid
 *  *x          float   y coordinate of the centroid
 *  *iret       int     Return code
 *                          0 = Function successful
 *                         -1 = Invalid pointer in arguments
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    CMDObjectContainer  *o;
/*---------------------------------------------------------------------*/

    if (!object || !x || !y) {
        *iret = -1;
        return;
    }

    *iret = 0;
    o = (CMDObjectContainer *)object;
    *x = o->centroid_x;
    *y = o->centroid_y;
}
