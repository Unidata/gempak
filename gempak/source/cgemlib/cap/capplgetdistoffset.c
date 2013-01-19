#include "capcmn.h"

void cap_plgetdistoffset(Placement place, float *dist, int *iret)
/*****************************************************************************
 * cap_plgetdistoffset
 *
 * Retrieves the distance offset value for the Placment object 
 * specified.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve offset from
 *
 * Output parameters:
 *  *dist    float  Distance offset factor 
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
        *dist = RMISSD;
        *iret = -1;
    } else {
        *dist = p->dist_offset;
        *iret = 0;
    }

    return;
}
