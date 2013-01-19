#include "capcmn.h"

void cap_pssetincr(PlacementSet placements, float increment, int *iret)
/*****************************************************************************
 * cap_pssetincr
 *
 * Assigns the 'step increment' to be used when dividing up the line segments 
 * around a reference when looking for places to position an object outside
 * the reference.
 * Note, if a value isn't assigned, then 5% of the smallest dimension of the
 * area will be used.
 * Note that the value should be in terms of the same cartesian coordinate 
 * system as the vertexes for the objects, not earth coordinates.
 *
 * Input parameters:
 *  placements      PlacementSet    handle to PlacementSet 
 *  increment       float           increment value
 *
 * Output parameters:
 *  *iret       int             Return code
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
        p->step_incr = increment;
        *iret = 0;
    } else {
        *iret = -1;
    }

    return;
}
