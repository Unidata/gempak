#include "capcmn.h"

void cap_psdelpl(PlacementSet placements, Handle id, int *iret)
/*****************************************************************************
 * cap_psdelpl
 *
 * Removes the placement information for the given id from the PlacementSet
 *
 * Input parameters:
 *  placements      PlacementSet    handle to PlacementSet to be changed
 *  id              Handle          Handle to the object to be deleted
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid object
 *                     -4 = Could not find id in set
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 * S.Danz/AWC            8/06   Fix problem with iterator when deleting
 ****************************************************************************/
{
    int                     location;
    PlacementSetContainer   *p = (PlacementSetContainer*)placements;
/*---------------------------------------------------------------------*/

    if (p) {
        location = cap_psfindpl(p, id, 1);
        if (location >= 0) {
            /*
             * Keep the placement to move
             */
            PlaceInfoContainer *to_delete = p->places[location];

            if (location != (p->used-1)) {
                /*
                 * Shift everything down
                 */
                memmove(&(p->places[location]), 
                        &(p->places[location+1]), 
                        sizeof(PlaceInfoContainer*)*(p->used - location - 1)
                    );

                /*
                 * Keep the free ones at the end of the array
                 */
                p->places[p->used-1] = to_delete;
            }

            /*
             * Clear the placement for later use
             */
            cap_plclear(to_delete);

            /*
             * If the iterator is in use, adjust it so we don't
             * skip one.
             */
            if (p->iter > 0 && location <= p->iter) {
                p->iter--;
            }

            p->used -= 1;
            *iret = 0;
        } else {
            *iret = -4;
        }
    } else {
        *iret = -1;
    }

    return;
}
