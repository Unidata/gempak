#include "capcmn.h"

void cap_psgetarea(PlacementSet placements, float *plot_area_min_x, 
                   float *plot_area_max_x, float *plot_area_min_y, 
                   float *plot_area_max_y, int *iret)
/*****************************************************************************
 * cap_psgetarea
 *
 * Retreives the 'plot region' to be used to bound all the objects being 
 * placed.
 *
 * Input parameters:
 *  placements      PlacementSet    handle to PlacementSet 
 *
 * Output parameters:
 *  plot_area_min_x float       left side of the plot area
 *  plot_area_max_x float       right side of the plot area
 *  plot_area_min_y float       bottom of the plot area
 *  plot_area_max_y float       top of the plot area
 *  *iret           int         Return code
 *                                  0 = Function successful
 *                                 -1 = Invalid object
 *                                 -2 = The plot area was not previously set
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    PlacementSetContainer  *p = (PlacementSetContainer*)placements;
/*---------------------------------------------------------------------*/

    if (!p || !plot_area_min_x || !plot_area_max_x || 
        !plot_area_min_y || !plot_area_max_y) {
        *iret = -1;
        return;
    }

    if (!p->plot_area_valid) {
        *plot_area_min_x = RMISSD;
        *plot_area_max_x = RMISSD;
        *plot_area_min_y = RMISSD;
        *plot_area_max_y = RMISSD;
        *iret = -2;
        return;
    }

    *iret = 0;

    *plot_area_min_x = p->plot_area_x[0];
    *plot_area_max_x = p->plot_area_x[1];
    *plot_area_min_y = p->plot_area_y[0];
    *plot_area_max_y = p->plot_area_y[2];

    return;
}
