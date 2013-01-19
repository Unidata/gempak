#include "capcmn.h"

void cap_plsetpcntr(Placement place, int check_center, int *iret) 
/*****************************************************************************
 * cap_plsetpcntr
 *
 * Sets the indicator to have the object check the line from the object to the
 * center of the reference vs the edge of the reference.
 *
 * Input parameters:
 *  place           Placement   handle to Placement object to set
 *  check_center    int         Flag specifing the line to check:
 *                                  0 = Check line from object to edge of ref
 *                                  1 = Check line from object to center of ref
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
        p->point_to_center = (check_center == 0) ? 0 : 1;
    } else {
        *iret = -1;
    }

    return;
}

