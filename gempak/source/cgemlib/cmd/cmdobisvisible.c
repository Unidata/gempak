#include "cmdcmn.h"

void cmd_obisvisible(CMDObject object, int *visible, int *iret)
/*****************************************************************************
 * cmd_obisvisible
 *
 * Return an boolean if the given object is visible in the display area or not.
 *
 * Input parameters:
 *  object      CMDObject   The object to check
 *
 * Output parameters:
 *  *visible    int     1 = object is visisble, 0 = object is NOT visible
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

    if (!object || !visible) {
        *iret = -1;
        return;
    }

    *iret = 0;
    o = (CMDObjectContainer *)object;

    *visible = o->isvisible;
}
