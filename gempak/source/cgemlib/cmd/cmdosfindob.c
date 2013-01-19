#include "cmdcmn.h"

int cmd_osfindob(CMDObjectSetContainer *objects, Handle id, int matchonly)
/*****************************************************************************
 * cmd_osfindob
 * 
 * Searches the CMDObjectSet for the given id.  If matchonly is 1, 
 * then the return value will be -1 if the given id doesn't exist in the set.
 * Otherwise, the position that the id should be located at is returned.
 *
 * Input parameters:
 *  *objects  CMDObjectSet  CMDObjectSet to search for the id
 *
 * Output parameters:
 *  None
 *
 * Return value:
 *  int     Location for the given id, -1 if matchonly and id not in the set
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int left, right, mid, result, found;
/*---------------------------------------------------------------------*/

    left = 0;
    right = objects->used - 1;
    found = 0;
    result = -1;

    while (left <= right && !found) {
        mid = (left + right) / 2;
        if (objects->objects[mid]->id < id) {
            left = mid + 1;
        } else if (objects->objects[mid]->id > id) {
            right = mid - 1;
        } else {
            found = 1;
            result = mid;
        }
    }

    if (!found && !matchonly) {
        result = left;
    }

    return result;
}
