#include "cmdcmn.h"

void cmd_obgetcntr(CMDObject object, float *x, float *y, int *iret)
/*****************************************************************************
 * cmd_obgetcntr 
 *
 * Retrieve the center of the bounding box for the given object
 *
 * Input parameters:
 *  object      CMDObject   what we would like the bounding box for
 *
 * Output parameters:
 *  *x          float   x coordinate of the center of the bounding box
 *  *x          float   y coordinate of the center of the bounding box
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
    *x = o->center_x;
    *y = o->center_y;
}
