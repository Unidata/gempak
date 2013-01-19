#include "capcmn.h"

void cap_plclear(PlaceInfoContainer *placement)
/*****************************************************************************
 * cap_plclear
 * 
 * Clear the contents of the given PlaceInfoContainer to prevent old values
 * from causing problems if the container is reused.
 *
 * Input parameters:
 *  *placement  PlaceInfoContainer  Container to clear
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    if (placement) {
        memset(placement, 0, sizeof(PlaceInfoContainer));
        placement->id = -1;
        placement->reference = -1;
        placement->allow_center = 1;
        placement->both_sides_of_line = 0;
        placement->max_attempts = 4;
        placement->dist_incr = -1.0;
        placement->dist_offset = -1.0;
        placement->point_to_center = 0;
        placement->mode = IMMEDIATE;
        placement->clutter = 0;

        placement->was_placed = 0;
        placement->in_center = 0;
        placement->offset.delta_x = 0.0;
        placement->offset.delta_y = 0.0;
        placement->arrow_x[0]= 0.0;
        placement->arrow_x[1]= 0.0;
        placement->arrow_y[0]= 0.0;
        placement->arrow_y[1]= 0.0;
    }
}
