#include "capcmn.h"

void cap_psiterinit(PlacementSet placements, int *iret)
/*****************************************************************************
 * cap_psiterinit
 *
 * Initializes the PlacementSet iterator so that the cap_psiternext can be
 * used to iterate over the Placements contained in the set.
 *
 * Input parameters:
 *  placements      PlacementSet    handle to PlacementSet 
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
    PlacementSetContainer  *p = (PlacementSetContainer*)placements;
/*---------------------------------------------------------------------*/

    if (p) {
        p->iter = 0;
        *iret = 0;
    } else {
        *iret = -1;
    }

    return;
}
