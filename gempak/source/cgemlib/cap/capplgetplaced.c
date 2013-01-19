#include "capcmn.h"

void cap_plgetplaced(Placement place, int *placed, int *iret)
/*****************************************************************************
 * cap_plgetplaced
 *
 * Retrieves the indicator reporting if the object referenced by the Placement
 * has been placed. 
 *
 * Input parameters:
 *  place   Placement   handle to Placement object to retrieve placement state
 *
 * Output parameters:
 *  *placed  int    1 if placed, 0 if not placed
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invaild object pointer
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (!p || !placed) {
        *placed = IMISSD;
        *iret = -1;
    } else {
        *iret = 0;
        *placed = (p->was_placed == 0) ? 0 : 1;
    }

    return;
}

