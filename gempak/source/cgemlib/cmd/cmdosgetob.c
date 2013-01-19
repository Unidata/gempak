#include "cmdcmn.h"

void cmd_osgetob(CMDObjectSet objects, Handle id, CMDObject *object, 
                 int *iret)
/*****************************************************************************
 * cmd_osgetob
 *
 * Retrieves the CMDObject for the object referenced by the Handle from the
 * CMDObjectSet
 *
 * Input parameters:
 *  objects      CMDObjectSet    handle to CMDObjectSet to be extended
 *  id              Handle          Handle to the object to be located
 *
 * Output parameters:
 *  *object      CMDObject       handle to CMDObject for the object
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid pointer in arguments
 *                     -5 = Could not find id in set
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int                     place;
    CMDObjectSetContainer   *o;
/*---------------------------------------------------------------------*/

    if (!objects || !object) {
        *iret = -1;
        return;
    }

    o = (CMDObjectSetContainer*)objects;
    place = cmd_osfindob(o, id, 1);
    if (place != -1) {
        *object = (CMDObject)o->objects[place];
        *iret = 0;
    } else {
        *object = (CMDObject *)NULL;
        *iret = -5;
    }

    return;
}
