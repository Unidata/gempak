#include "capcmn.h"

void cap_plgetcenter(Placement place, int *center, int *iret)
/*****************************************************************************
 * cap_plgetcenter
 *
 * Retrieves the value of the 'place in center allowed' indicator for the 
 * Placment object specified. 
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve indicator from
 *
 * Output parameters:
 *  *center  int    Flag specifing if center is allowed
 *                      0 = do not allow placement in the center
 *                      1 = allow placement in the center
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
        *center = 0;
        *iret = -1;
    } else {
        *center = p->allow_center;
        *iret = 0;
    }

    return;
}
