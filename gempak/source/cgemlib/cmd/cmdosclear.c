#include "cmdcmn.h"

void cmd_osclear(CMDObjectSet objects, int *iret)
/*****************************************************************************
 * cmd_osclear
 *
 * Clears the CMDObjectSet without freeing the memory (expecting that it will 
 * be reused shortly).
 *
 * Input parameters:
 *  objects         CMDObjectSet    handle to CMDObjectSet to be cleared
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid pointer in arguments
 **
 * Log:
 * S.Danz/AWC           07/06   Created
 ****************************************************************************/
{
    int                     index;
    CMDObjectSetContainer   *o;
/*---------------------------------------------------------------------*/

    if (!objects) {
        *iret = -1;
        return;
    }

    *iret = 0;
    o = (CMDObjectSetContainer *)objects;
    for (index = 0; index < o->used; index++) {
        /*
         * Clear the object for later use
         */
        cmd_obclear(o->objects[index]);
    }
    o->used = 0;

    return;
}
