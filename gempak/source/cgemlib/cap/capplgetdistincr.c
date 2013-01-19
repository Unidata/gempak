#include "capcmn.h"

void cap_plgetdistincr(Placement place, float *dist, int *iret)
/*****************************************************************************
 * cap_plgetdistincr
 *
 * Retrieves the distance increment value for the Placment object 
 * specified.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve offset from
 *
 * Output parameters:
 *  *dist    float  Distance increment factor 
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
        *dist = p->dist_incr;
        *iret = 0;
    }

    return;
}
