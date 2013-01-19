#include "capcmn.h"

void cap_psgetpl(PlacementSet placements, Handle id, Placement *placement, 
                 int *iret)
/*****************************************************************************
 * cap_psgetpl
 *
 * Retreives the Placement for the object referenced by the Handle from the
 * PlacementSet
 *
 * Input parameters:
 *  placements      PlacementSet    handle to PlacementSet to be extended
 *  id              Handle          Handle to the object to be placed
 *
 * Output parameters:
 *  *placement      Placement       handle to Placement for the object
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid object
 *                     -4 = Could not find object in set
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int                     place;
    PlacementSetContainer   *p = (PlacementSetContainer*)placements;
/*---------------------------------------------------------------------*/

    if (!placements || !placement) {
        *iret = -1;
        return;
    }

    place = cap_psfindpl(p, id, 1);
    if (place != -1) {
        *placement = (Placement)p->places[place];
        *iret = 0;
    } else {
        *placement = (Placement *)NULL;
        *iret = -4;
    }

    return;
}
