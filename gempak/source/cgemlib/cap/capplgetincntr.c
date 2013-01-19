#include "capcmn.h"

void cap_plgetincntr(Placement place, int *in_center, int *iret)
/*****************************************************************************
 * cap_plgetincntr
 *
 * Retrieves an indicator to report if the placed object was put in the center
 * of the reference (and needs no arrow).
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve information from
 *
 * Output parameters:
 *  *in_center int  Flag specifing if object was placed in the center
 *                      0 = object was NOT placed in the center
 *                      1 = object WAS placed in the center
 *  *iret      int  Return code
 *                      4 = Object was not placed
 *                      0 = Function successful
 *                     -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (!p || !(p->was_placed)) {
        *in_center = 0;
        if (p) {
            *iret =  4;
        } else {
            *iret = -1;
        }
    } else {
        *in_center = p->in_center;
        *iret = 0;
    }

    return;
}
