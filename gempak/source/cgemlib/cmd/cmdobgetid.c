#include "cmdcmn.h"

void cmd_obgetid(CMDObject object, Handle *id, int *iret)
/*****************************************************************************
 * cmd_obgetid 
 *
 * Retrieve the handle id for the given object
 *
 * Input parameters:
 *  object      CMDObject   what we would like the bounding box for
 *
 * Output parameters:
 *  *id         Handle  The opaque handle assigned to this object
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

    if (!object || !id) {
        *iret = -1;
        return;
    }

    *iret = 0;
    o = (CMDObjectContainer *)object;
    *id = o->id;
}
