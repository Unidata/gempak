#include "capcmn.h"

void cap_plgetref(Placement place, Handle *ref, int *iret)
/*****************************************************************************
 * cap_plgetref
 *
 * Retrieves the reference for the Placement object specified.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve line from
 *
 * Output parameters:
 *  *ref     Handle Handle to the reference defined for this placement
 *  *iret    int    Return code
 *                      0 = Reference is valid
 *                     -1 = Invaild object pointer
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (!p) {
        *ref = (Handle)NULL;
        *iret = -1;
    } else {
        *ref = p->reference;
        *iret = 0;
    }

    return;
}
