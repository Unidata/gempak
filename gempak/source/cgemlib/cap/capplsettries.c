#include "capcmn.h"

void cap_plsettries(Placement place, int tries, int *iret)
/*****************************************************************************
 * cap_plsettries
 *
 * Sets the value for the maximum number of attempts to make around the 
 * reference when placing the object. On each attempt, the distance away from
 * the reference is increased to attempt to find an open location for the 
 * object.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to set
 *  tries   int         Maximum number of attempts
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      0 = Value set
 *                     -1 = Invalid object
 *                     -9 = Value outside acceptable range
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (p && tries > 0) {
        *iret = 0;
        p->max_attempts = tries;
    } else {
        if (p) {
            *iret = -9;
        } else {
            *iret = -1;
        }
    }

    return;
}

