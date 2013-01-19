#include "cmdcmn.h"
#include "pgprm.h"

void cmd_osaddob(CMDObjectSet objects, Handle id, float *xpts, float *ypts,
                 int points, int *iret)
/*****************************************************************************
 * cmd_osaddob
 *
 * Adds the given information to the CMDObjectSet as a new CMDObject
 *
 * If the object is a polygon it is *required* to be 'CLOSED',i.e., the first
 * vertex MUST BE repeated at the end.
 *
 * The arrays of points will be copied by this routine, so the user is 
 * expected to managing the memory associated with them.
 *
 * NOTE: There are no 'outputs' from this function other than the storage
 *       associated with the CMDObjectSet handle will be extended to include
 *       another object.
 *
 * Input parameters:
 *  objects         CMDObjectSet    handle to CMDObjectSet to be extended
 *  id              Handle          handle for the object to be added
 *  *xpts           float           x coord. for the verticies of the object
 *  *ypts           float           y coord. for the verticies of the object
 *  points          int             the number of verticies in the object
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      0 = Function successful
 *                     -1 = Invalid pointer in arguments
 *                     -2 = Could not allocate memory
 *                     -3 = CMDObject with this id already exists
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    float                       area, dist, length, midpoint, bb[4];
    int                         point, check_pts, location;
    CMDObjectContainer          *newobject;
    CMDObjectSetContainer       *o;
/*---------------------------------------------------------------------*/

    if (!objects || !xpts || !ypts) {
        *iret = -1;
        return;
    }

    o = (CMDObjectSetContainer *)objects;

    /*
     * Figure out where we should put this one
     */
    location = cmd_osfindob(o, id, 0);

    /*
     * If nothing is here, or whatever is here isn't this id (so we
     * will be moving things around), put this one in
     */
    if (!o->objects || location == o->total || !o->objects[location] || 
        o->objects[location]->id != id ) {
        /*
         * If there is no more room, extend the set
         */
        if ((o->total - o->used) < 1) {
            if (o->total) {
                o->total *= 2;
                o->objects = (CMDObjectContainer**)realloc(o->objects, sizeof(CMDObjectContainer*) * o->total);
                G_REALLOC(o->objects, CMDObjectContainer*, o->total, "extending CMDObjectSet storage");
                memset(&(o->objects[o->used]), 0, sizeof(CMDObjectContainer*)*o->used);
            } else {
                o->total = MAX_EDITABLE_ELEMS;
                G_MALLOC(o->objects, CMDObjectContainer*, o->total, "creating CMDObjectSet storage");
                if (!o->objects) {
                    *iret = -2;
                    return;
                }
                memset(o->objects, 0, sizeof(CMDObjectContainer*)*o->total);
            }
        }

        /*
         * Before we go moving things around, get a free object if its there
         */
        newobject = cmd_osnewob(o, iret);

        if (newobject) {
            if (location != o->used) {
                memmove(&(o->objects[location+1]), 
                        &(o->objects[location]), 
                        sizeof(CMDObjectContainer*)*(o->used - location)
                    );
            }
            newobject->id = id;
            newobject->points = points;
            G_MALLOC(newobject->vertex_x, float, points, "Creating CMDObject x coordinates");
            G_MALLOC(newobject->vertex_y, float, points, "Creating CMDObject x coordinates");

            newobject->ispoly = ((points > 2) && 
                    ( G_DIFFT(xpts[0], xpts[points-1], GDIFFD) ) && 
                    ( G_DIFFT(ypts[0], ypts[points-1], GDIFFD) )) ? 1 : 0;

            /* 
             * First, if its a polygon, guarantee the points are in CCW order
             */
            if (newobject->ispoly) {
                cgr_ordrccw(points-1, xpts, ypts, iret);
                xpts[points-1] = xpts[0];
                ypts[points-1] = ypts[0];
            }
            memcpy(newobject->vertex_x, xpts, sizeof(float)*points);
            memcpy(newobject->vertex_y, ypts, sizeof(float)*points);

            newobject->extent.min_x = xpts[0];
            newobject->extent.max_x = xpts[0];
            newobject->extent.min_y = ypts[0];
            newobject->extent.max_y = ypts[0];
            for (point=1; point < points; point++) {
                if (xpts[point] < newobject->extent.min_x)
                    newobject->extent.min_x = xpts[point];
                if (xpts[point] > newobject->extent.max_x)
                    newobject->extent.max_x = xpts[point];
                if (ypts[point] < newobject->extent.min_y)
                    newobject->extent.min_y = ypts[point];
                if (ypts[point] > newobject->extent.max_y)
                    newobject->extent.max_y = ypts[point];
            }

            /*
             * Store the center now that we know the points
             */
            newobject->center_x = (newobject->extent.max_x + 
                    newobject->extent.min_x) / 2;
            newobject->center_y = (newobject->extent.max_y + 
                    newobject->extent.min_y) / 2;

            /*
             * If the display area has been set, determine if we
             * are visible.  To be visible means that the object
             * needs to intersect the polygon of the drawing area
             */
            if (o->plot_area_valid) {
                bb[0] = newobject->extent.min_x;
                bb[1] = newobject->extent.max_x;
                bb[2] = newobject->extent.min_y;
                bb[3] = newobject->extent.max_y;
                cgr_objint(points, xpts, ypts, bb,
                        5, o->plot_area_x, o->plot_area_y, o->plot_bb,
                        &(newobject->isvisible), iret
                );
            } else {
                /* 
                 * If we don't know any better, then we have to say
                 * its visible or else nothing will get placed
                 */
                newobject->isvisible = 1;
            }

            /*
             * Compute the centroid and hang onto it for later use
             * For lines, just compute the midpoint for the line
             */
            if (newobject->ispoly) {
                /*
                 * WARNING:  The cgr_centroid *does not* want to see
                 * the polygon with the repeated point, so send it
                 * in one less.
                 */
                check_pts = points - 1;
                cgr_centroid (xpts, ypts, &check_pts, &newobject->centroid_x,
                        &newobject->centroid_y, &area, iret);
            } else {
                dist = 0;
                for (point=0; point < points-1; point++) {
                    dist += (float)sqrt((double)((xpts[point]-xpts[point+1])*
                                (xpts[point]-xpts[point+1])+
                                (ypts[point]-ypts[point+1])*
                                (ypts[point]-ypts[point+1])));
                }
                midpoint = dist/2.0;
                dist = 0;
                point = 0;
                for (point=0; (dist < midpoint) && point < points-1; point++) {
                    dist += (float)sqrt((double)((xpts[point]-xpts[point+1])*
                                (xpts[point]-xpts[point+1])+
                                (ypts[point]-ypts[point+1])*
                                (ypts[point]-ypts[point+1])));
                }

                if (point > 0) {
                    point--;
                    length = (float)sqrt((double)((xpts[point]-xpts[point+1])*
                                (xpts[point]-xpts[point+1])+
                                (ypts[point]-ypts[point+1])*
                                (ypts[point]-ypts[point+1])));
                    dist = length - (dist - midpoint);
                    newobject->centroid_x = xpts[point] + 
                            (dist/length) * (xpts[point+1] - xpts[point]);
                    newobject->centroid_y = ypts[point] + 
                            (dist/length) * (ypts[point+1] - ypts[point]);
                } else {
                    newobject->centroid_x = xpts[0];
                    newobject->centroid_y = ypts[0];
                }
            }

            o->objects[location] = newobject;
            o->used++;
            *iret = 0;
        }
    } else {
        *iret = -3;
    }

    return;
}
