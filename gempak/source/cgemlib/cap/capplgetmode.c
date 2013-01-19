#include "capcmn.h"

void cap_plgetmode(Placement place, PlacementMode *mode, int *iret)
/*****************************************************************************
 * cap_plgetmode
 *
 * Retrieves the placement mode for the Placment object specified.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve offset from
 *
 * Output parameters:
 *  *mode    PlacementMode  Mode to use for this placement definition
 *  *iret    int    Return code
 *                      0 = Results are valid
 *                     -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (!p) {
        *mode = MODE_UNDEF;
        *iret = -1;
    } else {
        *mode = p->mode;
        *iret = 0;
    }

    return;
}
