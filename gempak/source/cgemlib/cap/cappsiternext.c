#include "capcmn.h"

void cap_psiternext(PlacementSet placements, Placement *placement, int *iret)
/*****************************************************************************
 * cap_psiternext
 *
 * Returns the Placement at the current position for the iterator and moves
 * to the next position.  If iterator is past the end, then NULL is returned.
 *
 * Input parameters:
 *  placements  PlacementSet    handle to PlacementSet 
 *
 * Output parameters:
 *  placement   Placement       handle to the next Placement in the set
 *  *iret       int             Return code
 *                                  3 = Iterator is past end of set
 *                                  0 = Function successful
 *                                 -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlacementSetContainer  *p = (PlacementSetContainer*)placements;
/*---------------------------------------------------------------------*/

    if (!p || !placement) {
        *iret = -1;
        return;
    }

    *iret = 0;
    if (p->iter < p->used) {
        *placement = (Placement*)p->places[p->iter];
        p->iter++;
    } else {
        *placement = (Placement *)NULL;
        *iret = 3;
    }

    return;
}
