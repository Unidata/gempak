#include "cmdcmn.h"

void cmd_ossetarea(CMDObjectSet objects, float plot_area_min_x, 
                   float plot_area_max_x, float plot_area_min_y, 
                   float plot_area_max_y, int *iret)
/*****************************************************************************
 * cmd_pssetarea
 *
 * Assigns the 'plot region' to be used to bound all the objects in the set
 * for the 'visibility' test.
 * Note that the coordinates of the region are assumed to be in the same 
 * cartesian coordinate system as the vertexes for the objects, not earth 
 * coordinates.
 *
 * Input parameters:
 *  objects         CMDObjectSet    handle to CMDObjectSet 
 *  plot_area_min_x float           left side of the plot area
 *  plot_area_max_x float           right side of the plot area
 *  plot_area_min_y float           bottom of the plot area
 *  plot_area_max_y float           top of the plot area
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                  0 = Values assigned
 *                                 -1 = Invalid object
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int                     object;
    float                   bb[4];
    CMDObjectSetContainer   *o;
/*---------------------------------------------------------------------*/

    o = (CMDObjectSetContainer*)objects;
    if (o) {
        o->plot_area_valid = 1;

        o->plot_area_x[0] = plot_area_min_x;
        o->plot_area_x[1] = plot_area_max_x;
        o->plot_area_x[2] = plot_area_max_x;
        o->plot_area_x[3] = plot_area_min_x;
        o->plot_area_x[4] = o->plot_area_x[0];

        o->plot_area_y[0] = plot_area_min_y;
        o->plot_area_y[1] = plot_area_min_y;
        o->plot_area_y[2] = plot_area_max_y;
        o->plot_area_y[3] = plot_area_max_y;
        o->plot_area_y[4] = o->plot_area_y[0];

        o->plot_bb[0] = plot_area_min_x;
        o->plot_bb[1] = plot_area_max_x;
        o->plot_bb[2] = plot_area_min_y;
        o->plot_bb[3] = plot_area_max_y;

        if (o->used > 0) {
            for (object = 0; object < o->used; object++) {
                bb[0] = o->objects[object]->extent.min_x;
                bb[1] = o->objects[object]->extent.max_x;
                bb[2] = o->objects[object]->extent.min_y;
                bb[3] = o->objects[object]->extent.max_y;
                cgr_objint(o->objects[object]->points, 
                        o->objects[object]->vertex_x, 
                        o->objects[object]->vertex_y, bb, 
                        5, o->plot_area_x, o->plot_area_y, o->plot_bb,
                        &(o->objects[object]->isvisible), iret
                    );
            }
        }
    } else {
        *iret = -1;
    }

    return;
}
