#include "cmdcmn.h"

void cmd_obispoly(CMDObject object, int *poly, int *iret)
/*****************************************************************************
 * cmd_obispoly
 *
 * Return an boolean if the given object is a polygon or not.  Right now this
 * is determined by checking if the first and last points in the object are
 * identical and if there are more than 2 points.
 *
 * Input parameters:
 *  object      CMDObject   The object to check
 *
 * Output parameters:
 *  *poly       int     1 = object is a polygon, 0 = object is NOT a polygon
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

    if (!object || !poly) {
        *iret = -1;
        return;
    }

    *iret = 0;
    o = (CMDObjectContainer *)object;

    *poly = o->ispoly;
}
