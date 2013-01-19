#include "capcmn.h"

void cap_psclrplaced(PlacementSet placements, int *iret)
/*****************************************************************************
 * cap_psclrplaced
 *
 * Clears the 'placed' indicator for all the elements in the PlacementSet
 *
 * Input parameters:
 *  placements      PlacementSet    handle to PlacementSet to be extended
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int                     index;
    PlacementSetContainer   *p = (PlacementSetContainer*)placements;
/*---------------------------------------------------------------------*/

    if (!placements) {
        *iret = -1;
        return;
    }

    for (index = 0; index < p->used; index++) {
        p->places[index]->was_placed = 0;
    }


    return;
}
