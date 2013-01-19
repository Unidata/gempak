#include "cmdcmn.h"

void cmd_osdel(CMDObjectSet objects, int *iret)
/*****************************************************************************
 * cmd_osdel
 *
 * Deletes the given CMDObjectSet and any associated structures
 *
 * Input parameters:
 *  objects CMDObjectSet    CMDObjectSet to delete
 *
 * Output parameters:
 *  *iret       int     Return code
 *                          0 = Function successful
 *                         -1 = Invalid pointer in arguments
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int                     object;
    CMDObjectSetContainer   *o;
/*---------------------------------------------------------------------*/

    o = (CMDObjectSetContainer*)objects;
    if (o) {
        *iret = 0;
        for (object = 0; object < o->total; object++) {
            cmd_obclear(o->objects[object]);
            G_FREE(o->objects[object], CMDObjectContainer);
        }

        G_FREE(o->objects, CMDObjectContainer*);

        G_FREE(o, CMDObjectSetContainer);
    } else {
        *iret = -1;
    }
}
