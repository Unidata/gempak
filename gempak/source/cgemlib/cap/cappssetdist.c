#include "capcmn.h"

void cap_pssetdist(PlacementSet placements, float dist, int *iret)
/*****************************************************************************
 * cap_pssetdist
 *
 * Assigns the initial distance to be used when positioning an object outside
 * its reference.
 * Note, if a value isn't assigned, and a value isn't assigned to the 
 * placement object, then 10% of the smallest dimension of the area will be 
 * used.
 * Note that the value should be in terms of the same cartesian coordinate 
 * system as the vertexes for the objects, not earth coordinates.
 *
 * Input parameters:
 *  placements      PlacementSet    handle to PlacementSet 
 *  dist            float           dist value
 *
 * Output parameters:
 *  *iret           int         Return code
 *                                  0 = Function successful
 *                                 -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlacementSetContainer   *p = (PlacementSetContainer*)placements;
/*---------------------------------------------------------------------*/

    if (p) {
        p->distance = dist;
        *iret = 0;
    } else {
        *iret = -1;
    }

    return;
}
