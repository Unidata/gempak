#include "capcmn.h"

void cap_plclrplace(Placement place, int *iret) 
/*****************************************************************************
 * cap_plclrplace
 *
 * Clears the placed indication for the object specified by this Placement.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to clear
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      0 = Value set
 *                     -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (p) {
        *iret = 0;
        p->was_placed = 0;
    } else {
        *iret = -1;
    }

    return;
}

