#include "capcmn.h"

void cap_psnew(PlacementSet *placements, int *iret)
/*****************************************************************************
 * cap_psnew
 *
 * Creates an empty PlacementSet and any associated structures
 *
 * Input parameters:
 *
 * Output parameters:
 *  *placements  PlacementSet    Placement set to create
 *  *iret       int     Return code
 *                      0 = Function successful
 *                     -1 = Invalid pointer in argument list
 *                     -3 = Could not allocate memory
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int	one=1;
    PlacementSetContainer   *p;
/*---------------------------------------------------------------------*/

    if (placements) {
        G_MALLOC(p, PlacementSetContainer, one, "creating new placement set");
        *placements = (PlacementSet*)p;
        if (p) {
            *iret = 0;
            memset(p, 0, sizeof(PlacementSetContainer));
        } else {
            *iret = -3;
        }
    } else {
        *iret = -1;
    }
}
