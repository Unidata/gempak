#include "cmdcmn.h"

void cmd_osnew(CMDObjectSet *objects, int *iret)
/*****************************************************************************
 * cmd_osnew
 *
 * Creates an empty CMDObjectSet and any associated structures
 *
 * Input parameters:
 *
 * Output parameters:
 *  *placements  PlacementSet    Placement set to create
 *  *iret       int     Return code
 *                      0 = Function successful
 *                     -1 = Invalid pointer to store result
 *                     -2 = Could not allocate memory
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int	one=1;
    CMDObjectSetContainer   *o;
/*---------------------------------------------------------------------*/

    if (objects) {
        G_MALLOC(o, CMDObjectSetContainer, one, "creating new object set");
        *objects = (CMDObjectSet*)o;
        if (o) {
            *iret = 0;
            memset(o, 0, sizeof(CMDObjectSetContainer));
        } else {
            *iret = -2;
        }
    } else {
        *iret = -1;
    }
}
