#include "capcmn.h"

void cap_plgetsides(Placement place, int *sides, int *iret)
/*****************************************************************************
 * cap_plgetsides
 *
 * Retrieves the value of indicator which specifies if the object is allowed
 * to be placed on both side of the reference when the reference is a line.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve offset from
 *
 * Output parameters:
 *  *sides   int    Flag specifing the state
 *                      0 = do not allow placement on both sides
 *                      1 = allow placement on both sides
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
        *sides = 0;
        *iret = -1;
    } else {
        *sides = p->both_sides_of_line;
        *iret = 0;
    }

    return;
}
