#include "cmdcmn.h"

void cmd_obgetbb(CMDObject object, float *minx, float *maxx, float *miny,
                 float *maxy, int *iret)
/*****************************************************************************
 * cmd_obgetbb 
 *
 * Retrieve the bounding box information for the given object
 *
 * Input parameters:
 *  object      CMDObject   what we would like the bounding box for
 *
 * Output parameters:
 *  *minx       float   x coordinate of the left of the bounding box
 *  *maxx       float   x coordinate of the right of the bounding box
 *  *miny       float   y coordinate of the bottom of the bounding box
 *  *maxy       float   y coordinate of the top of the bounding box
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

    if (!object || !minx || !miny || !maxx || !maxy) {
        *iret = -1;
        return;
    }

    *iret = 0;

    o = (CMDObjectContainer *)object;
    *minx = o->extent.min_x;
    *miny = o->extent.min_y;
    *maxx = o->extent.max_x;
    *maxy = o->extent.max_y;
}
