#include "capcmn.h"

void cap_plsetmode(Placement place, PlacementMode mode, int *iret) 
/*****************************************************************************
 * cap_plsetmode
 *
 * Sets the value for the 'mode' for the placement for the object.  The three
 * types are, IMMEDIATE, ONE_SHOT, DELAYED.  Immediate takes as many passes
 * as allowed to attempt to place the object.  One shot only attempts one
 * iteration for the placement of the object.  Delayed just allows the caller
 * to specify that the placement of this object should be skipped.
 *
 * Input parameters:
 *  place   Placement       handle to Placement object to set
 *  mode    PlacementMode   Mode to use for this placement definition
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
        p->mode = mode;
    } else {
        *iret = -1;
    }

    return;
}

