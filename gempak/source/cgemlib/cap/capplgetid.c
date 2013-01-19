#include "capcmn.h"

void cap_plgetid(Placement place, Handle *id, int *iret)
/*****************************************************************************
 * cap_plgetid
 *
 * Retrieves the handle of the object corresponding to this placement 
 * definition. 
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve offset from
 *
 * Output parameters:
 *  *id      Handle Id for the object corresponding to this placement def.
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = CMDObject wasn't placed so the offset is invalid
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (!p) {
        *id = (Handle)NULL;
        *iret = -1;
    } else {
        *id = p->id;
        *iret = 0;
    }

    return;
}
