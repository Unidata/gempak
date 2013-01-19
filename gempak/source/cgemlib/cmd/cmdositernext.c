#include "cmdcmn.h"

void cmd_ositernext(CMDObjectSet objects, CMDObject *object, int *iret)
/*****************************************************************************
 * cmd_ositernext
 *
 * Returns the CMDObject at the current position for the iterator and moves
 * to the next position.  If iterator is past the end, then NULL is returned.
 *
 * Input parameters:
 *  objects     CMDObjectSet    handle to CMDObjectSet to iterate 
 *
 * Output parameters:
 *  *iret       int    Return code
 *                          0 = Function successful
 *                         -1 = Invalid pointer in arguments
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    CMDObjectSetContainer  *o;
/*---------------------------------------------------------------------*/

    if (!objects || !object) {
        *iret = -1;
        return;
    }

    *iret = 0;
    o = (CMDObjectSetContainer *)objects;

    if (o->iter < o->used) {
        *object = (CMDObject*)o->objects[o->iter];
        o->iter++;
    } else {
        *object = (CMDObject*)NULL;
    }

    return;
}
