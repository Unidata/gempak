#include "capcmn.h"

void cap_plsetcenter(Placement place, int center, int *iret)
/*****************************************************************************
 * cap_plsetcenter
 *
 * Sets the value of the indicator allowing the object to be placed in the 
 * center of the reference
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to set 
 *  center  int         Flag specifing if center is allowed
 *                          0 = do not allow placement in the center
 *                          1 = allow placement in the center
 *
 * Output parameters:
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

    if (p) {
        *iret = 0;
        p->allow_center = (center == 0) ? 0 : 1;
    } else {
        *iret = -1;
    }

    return;
}

