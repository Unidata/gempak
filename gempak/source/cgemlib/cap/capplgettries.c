#include "capcmn.h"

void cap_plgettries(Placement place, int *tries, int *iret)
/*****************************************************************************
 * cap_plgettries
 *
 * Retreives the value for the maximum number of attempts to make around the 
 * reference when placing the object.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve offset from
 *
 * Output parameters:
 *  *tries   int    Maximum number of attempts
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (!p) {
        *tries = 0;
        *iret = -1;
    } else {
        *tries = p->max_attempts;
        *iret = 0;
    }

    return;
}
