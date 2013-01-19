#include "capcmn.h"

void cap_pssetarea(PlacementSet placements, float plot_area_min_x, 
                   float plot_area_max_x, float plot_area_min_y, 
                   float plot_area_max_y, int *iret)
/*****************************************************************************
 * cap_pssetarea
 *
 * Assigns the 'plot region' to be used to bound all the objects being placed.
 * Note that the coordinates of the region are assumed to be in the same 
 * cartesian coordinate system as the vertexes for the objects, not earth 
 * coordinates.
 *
 * Input parameters:
 *  placements      PlacementSet    handle to PlacementSet 
 *  plot_area_min_x float           left side of the plot area
 *  plot_area_max_x float           right side of the plot area
 *  plot_area_min_y float           bottom of the plot area
 *  plot_area_max_y float           top of the plot area
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                  0 = Function successful
 *                                 -1 = Invalid object
 *                                 -7 = Invalid plot area defined
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    float                   x_dist;
    float                   y_dist;
    PlacementSetContainer   *p = (PlacementSetContainer*)placements;
/*---------------------------------------------------------------------*/

    if (p) {
        p->plot_area_valid = 1;

        p->plot_area_x[0] = plot_area_min_x;
        p->plot_area_x[1] = plot_area_max_x;
        p->plot_area_x[2] = plot_area_max_x;
        p->plot_area_x[3] = plot_area_min_x;
        p->plot_area_x[4] = p->plot_area_x[0];

        p->plot_area_y[0] = plot_area_min_y;
        p->plot_area_y[1] = plot_area_min_y;
        p->plot_area_y[2] = plot_area_max_y;
        p->plot_area_y[3] = plot_area_max_y;
        p->plot_area_y[4] = p->plot_area_y[0];

        p->plot_bb[0] = plot_area_min_x;
        p->plot_bb[1] = plot_area_max_x;
        p->plot_bb[2] = plot_area_min_y;
        p->plot_bb[3] = plot_area_max_y;

        x_dist = 1 + plot_area_max_x - plot_area_min_x;
        y_dist = 1 + plot_area_max_y - plot_area_min_y;

        if (x_dist <= 0.0 || y_dist <= 0.0) {
            p->default_dist = 0.10;
            p->default_incr = 0.05;
            *iret = -7;
        } else {
            if (x_dist > y_dist) {
                p->default_dist = 0.03 * y_dist;
                p->default_incr = 0.02 * y_dist;
            } else {
                p->default_dist = 0.03 * x_dist;
                p->default_incr = 0.02 * x_dist;
            }
            *iret = 0;
        }
    } else {
        *iret = -1;
    }

    return;
}
