#include "capcmn.h"

void cap_plsetref(Placement place, Handle ref, int *iret) 
/*****************************************************************************
 * cap_plsetref
 *
 * Sets the reference for the object specified by this Placement.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to set
 *  ref     Handle      handle to the reference for this object
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid object
 *                     -5 = An object cannot use itself as a reference
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (p && (p->id != ref)) {
        *iret = 0;
        p->reference = ref;
    } else {
        if (p) {
            *iret = -5;
        } else {
            *iret = -1;
        }
    }

    return;
}

