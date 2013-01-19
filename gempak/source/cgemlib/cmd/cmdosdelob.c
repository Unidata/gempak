#include "cmdcmn.h"

void cmd_osdelob(CMDObjectSet objects, Handle id, int *iret)
/*****************************************************************************
 * cmd_osdelob
 *
 * Removes the CMDObject information for the given id from the CMDObjectSet
 *
 * Input parameters:
 *  objects         CMDObjectSet    handle to CMDObjectSet to be changed
 *  id              Handle          Handle to the object to be deleted
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid pointer in arguments
 *                     -5 = Could not find id in set
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 * S.Danz/AWC            8/06   Fix problem with iterator when deleting
 ****************************************************************************/
{
    int                         location;
    CMDObjectContainer          *to_delete;
    CMDObjectSetContainer       *o;
/*---------------------------------------------------------------------*/

    if (!objects) {
        *iret = -1;
        return;
    }

    o = (CMDObjectSetContainer *)objects;
    location = cmd_osfindob(o, id, 1);
    if (location >= 0) {
        /*
         * Keep the object to move
         */
        to_delete = o->objects[location];

        if (location != (o->used-1)) {

            /*
             * Shift everything down
             */
            memmove(&(o->objects[location]), 
                    &(o->objects[location+1]), 
                    sizeof(CMDObjectContainer*)*(o->used - location - 1)
                );

            /*
             * Keep the free ones at the end of the array
             */
            o->objects[o->used-1] = to_delete;
        }

        /*
         * Clear the object for later use
         */
        cmd_obclear(to_delete);

        /*
         * If the iterator is in use, adjust it so we don't
         * skip one.
         */
        if (o->iter > 0 && location <= o->iter) {
            o->iter--;
        }

        o->used -= 1;
        *iret = 0;
    } else {
        *iret = -5;
    }

    return;
}
