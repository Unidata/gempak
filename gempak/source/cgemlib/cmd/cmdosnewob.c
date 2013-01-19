#include "cmdcmn.h"

CMDObjectContainer* cmd_osnewob(CMDObjectSetContainer *objects, int *iret)
/*****************************************************************************
 * cmd_osnewob
 *
 * Returns a CMDObjectContainer.  If there is an unused container in the 
 * set, that is returned, otherwise a new one is allocated from memory.
 *
 * Input parameters:
 *  *objects CMDObjectSetContainer   Set to add an new CMDObjectContainer to
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid pointer to store result
 *                     -2 = Could not allocate memory
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int	one=1;
    CMDObjectContainer  *newobject=NULL;
/*---------------------------------------------------------------------*/

    if (!objects) {
        *iret = -1;
        return newobject;
    }

    *iret = 0;
    newobject = (CMDObjectContainer*)NULL;
    if (objects->objects && 
        objects->used < objects->total && 
        objects->objects[objects->used] != NULL) {
        newobject = objects->objects[objects->used];
        cmd_obclear(newobject);
    } else {
        G_MALLOC(newobject, CMDObjectContainer, one, "creating new object container");
        if (newobject) {
            memset(newobject, 0, sizeof(CMDObjectContainer));
        } else {
            *iret = -2;
        }
    }

    return newobject;
}
