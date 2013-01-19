#include "capcmn.h"

void cap_plgetline(Placement place, float *x, float *y, int *iret)
/*****************************************************************************
 * cap_plgetline
 *
 * Retrieves the line information for the Placement object specified.  If the
 * object was not placed, the endpoints will be returned as RMISSD.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve line from
 *
 * Output parameters:
 *  x[2]     float  x coordinates of the line, starting from object to ref
 *  y[2]     float  y coordinates of the line, starting from object to ref
 *  *iret    int    Return code
 *                      4 = Object was not placed
 *                      0 = Line is valid
 *                     -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (!p || !(p->was_placed)) {
        x[0] = RMISSD;
        x[1] = RMISSD;
        y[0] = RMISSD;
        y[1] = RMISSD;
        if (p) {
            *iret =  4;
        } else {
            *iret = -1;
        }
    } else {
        x[0] = p->arrow_x[0];
        x[1] = p->arrow_x[1];
        y[0] = p->arrow_y[0];
        y[1] = p->arrow_y[1];
        *iret = 0;
    }

    return;
}
