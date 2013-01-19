#include "capcmn.h"

PlaceInfoContainer* cap_psnewpl(PlacementSetContainer *placements, int *iret)
/*****************************************************************************
 * cap_psnewpl
 *
 * Returns a PlaceInfoContainer.  If there is an unused container in the 
 * set, that is returned, otherwise a new one is allocated from memory.
 *
 * Input parameters:
 *  *placements PlacementSetContainer   PlacmentInfoContainer to add to
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid pointer in argument list
 *                     -3 = Could not allocate memory
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int	one=1;
    PlaceInfoContainer  *newplace = (PlaceInfoContainer*)NULL;
/*---------------------------------------------------------------------*/

    if (!placements) {
        *iret = -1;
        return newplace;
    }

    *iret = 0;
    if (placements->places && 
        placements->used < placements->total && 
        placements->places[placements->used] != NULL) {
        newplace = placements->places[placements->used];
        cap_plclear(newplace);
    } else {
        G_MALLOC(newplace, PlaceInfoContainer, one,
                "creating new placement container");

        if (newplace) {
            cap_plclear(newplace);
        } else {
            *iret = -3;
        }
    }

    return newplace;
}
