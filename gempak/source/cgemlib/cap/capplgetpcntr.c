#include "capcmn.h"

void cap_plgetpcntr(Placement place, int *check_center, int *iret) 
/*****************************************************************************
 * cap_plgetpcntr
 *
 * Retrieves the indicator to check the line from the object to
 * the center of the reference vs the edge of the reference.
 *
 * Input parameters:
 *  place           Placement   handle to Placement object to set
 *
 * Output parameters:
 *  *check_center   int    Flag specifing the line to check:
 *                           0 = Check line from object to edge of ref
 *                           1 = Check line from object to center of ref
 *  *iret           int    Return code
 *                           0 = Value set
 *                          -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlaceInfoContainer  *p = (PlaceInfoContainer*)place;
/*---------------------------------------------------------------------*/

    if (p && check_center) {
        *iret = 0;
        *check_center = p->point_to_center;
    } else {
        *iret = -1;
    }

    return;
}

