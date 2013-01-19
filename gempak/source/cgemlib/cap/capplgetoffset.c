#include "capcmn.h"

void cap_plgetoffset(Placement place, float *x, float *y, int *iret)
/*****************************************************************************
 * cap_plgetoffset
 *
 * Retrieves the offset information for the Placment object specified.  If the
 * object was not placed, the offsets will be returned as RMISSD.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve offset from
 *
 * Output parameters:
 *  *x       float  x offset of the placed object
 *  *y       float  y offset of the placed object
 *  *iret    int    Return code
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
        *x = RMISSD;
        *y = RMISSD;
        if (p) {
            *iret =  4;
        } else {
            *iret = -1;
        }
    } else {
        *x = p->offset.delta_x;
        *y = p->offset.delta_y;
        *iret = 0;
    }

    return;
}
