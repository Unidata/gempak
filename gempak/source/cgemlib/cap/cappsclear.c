#include "capcmn.h"

void cap_psclear(PlacementSet placements, int *iret)
/*****************************************************************************
 * cap_psclear
 *
 * Clears the PlacementSet information without freeing the memory (expecting 
 * that it will be reused shortly).
 *
 * Input parameters:
 *  placements      PlacementSet    handle to PlacementSet to be cleared
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
    PlacementSetContainer   *p;
/*---------------------------------------------------------------------*/

    if (!placements) {
        *iret = -1;
        return;
    }

    *iret = 0;
    p = (PlacementSetContainer*)placements;
    for (index = 0; index < p->used; index++) {
        /*
         * Clear the placement for later use
         */
        cap_plclear(p->places[index]);
    }
    p->used = 0;

    return;
}
