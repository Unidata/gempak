#include "capcmn.h"

void cap_plsetdist(Placement place, float dist, int *iret);

void cap_plsetdist(Placement place, float dist, int *iret)
/*****************************************************************************
 * cap_plsetdist
 *
 * Sets the value for the distance increment factor between the placed object 
 * and its reference.  Note that the value is a *percentage* factor of the
 * overall default increment set for the placement set and must be > 0.
 * This distance is increased on each iteration around the reference when
 * cap_psplace() fails to place the object at any point around the reference.
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to set
 *  dist    float       Initial distance between the object and the reference
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
        p->dist_incr = dist;
    } else {
        *iret = -1;
    }

    return;
}

