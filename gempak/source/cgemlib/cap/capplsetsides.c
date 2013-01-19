#include "capcmn.h"

void cap_plsetsides(Placement place, int sides, int *iret) 
/*****************************************************************************
 * cap_plsetsides
 *
 * Sets the indicator to allow the object to be placed on both side of the 
 * reference when the reference is a line.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to set
 *  sides   int         Flag specifing the state
 *                          0 = do not allow placement on both sides
 *                          1 = allow placement on both sides
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
        p->both_sides_of_line = (sides == 0) ? 0 : 1;
    } else {
        *iret = -1;
    }

    return;
}

