#include "capcmn.h"

void cap_psdel(PlacementSet placements, int *iret)
/*****************************************************************************
 * cap_psdel
 *
 * Deletes the given PlacementSet and any associated structures
 *
 * Input parameters:
 *  placements  PlacementSet    Placement set to delete
 *
 * Output parameters:
 *  *iret       int     Return code
 *                          0 = Function successful
 *                         -1 = Invalid pointer in arguments
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int                     place;
    PlacementSetContainer   *p = (PlacementSetContainer*)placements;
/*---------------------------------------------------------------------*/

    if (p) {
        *iret = 0;
        for (place = 0; place < p->total; place++) {
            G_FREE(p->places[place], PlaceInfoContainer);
        }

        G_FREE(p->places, PlaceInfoContainer*);

        G_FREE(p, PlacementSetContainer);
    } else {
        *iret = -1;
    }
}
