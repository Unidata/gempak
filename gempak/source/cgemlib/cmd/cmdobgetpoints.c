#include "cmdcmn.h"

void cmd_obgetpoints(CMDObject object, const float **xpts, const float **ypts, 
                     int *points, int *iret)
/*****************************************************************************
 * cmd_obgetpoints 
 *
 * Retrieve the coordinate information for the given object.  
 *
 * NOTE: Right now we are just passing back a ptr to the internal data, BUT, 
 * as a CONST so that the caller can't mess up the data.  Maybe in the future 
 * there could be an option to return the results in space allocated for the 
 * caller to free when they were done (esp. if the internal format were to 
 * change).
 *
 * Input parameters:
 *  object      CMDObject   what we would like the bounding box for
 *
 * Output parameters:
 *  **xpts      float   x coordinates for the objects verticies
 *  **ypts      float   y coordinates for the objects verticies
 *  *points     int     number of points returned
 *  *iret       int     Return code
 *                          0 = Function successful
 *                         -1 = Invalid pointer in arguments
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    CMDObjectContainer  *o;
/*---------------------------------------------------------------------*/

    if (!object || !xpts || !ypts || !points) {
        *iret = -1;
        return;
    }

    *iret = 0;
    o = (CMDObjectContainer *)object;
    *xpts = o->vertex_x;
    *ypts = o->vertex_y;
    *points = o->points;
}
