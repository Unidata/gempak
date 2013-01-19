#include "capcmn.h"
#include "pgprm.h"

void cap_psaddpl(PlacementSet placements, Handle id, Handle ref, 
                 int allow_center, int both_sides, int max_tries, 
                 float dist_incr, float dist_offset, PlacementMode pmode, 
                 int point_center, int *iret)
/*****************************************************************************
 * cap_psaddpl
 *
 * Adds the given information to the PlacementSet as a new Placement
 *
 * Input parameters:
 *  placements      PlacementSet    handle to PlacementSet to be extended
 *  id              Handle          Handle to the object to be placed
 *  ref             Handle          Handle to the reference for the object
 *  allow_center    int             Allow placement in the center of the ref
 *                                  (if reference is a polygon)
 *  both_sides      int             Allow placement on both sides of line
 *                                  (if reference is a line)
 *  max_tries       int             Number of iterations to attempt around ref
 *  dist_incr       float           Distance increment factor
 *  dist_offset     float           Factor for the initial offset from the ref
 *  pmode           PlacementMode   Mode of the placement
 *  point_center    int             Check line from object to center of ref
 *                                  or edge, 0=center, 1=edge
 *
 * Output parameters:
 *  *iret    int    Return code
 *                      2 = Handle specified is already in the placement set
 *                      0 = Function successful
 *                     -1 = Invalid object
 *                     -3 = Could not allocate memory
 *                     -8 = Invalid distance increment percentage
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    int                     location;
    PlaceInfoContainer      *newplace;
    PlacementSetContainer   *p = (PlacementSetContainer*)placements;
/*---------------------------------------------------------------------*/

    if (p) {
        /*
         * Figure out where we should put this one
         */
        location = cap_psfindpl(p, id, 0);

        /*
         * If nothing is here, or whatever is here isn't this id (so we
         * will be moving things around), put this one in
         */
        if (!p->places || location == p->total || !p->places[location] || 
            p->places[location]->id != id ) {
            /*
             * If there is no more room, extend the set
             */
            if ((p->total - p->used) < 1) {
                if (p->total) {
                    p->total *= 2;
                    p->places = (PlaceInfoContainer**)realloc(p->places, 
                            sizeof(PlaceInfoContainer*) * p->total);
                    G_REALLOC(p->places, PlaceInfoContainer*, p->total, 
                            "extending PlacementSet");
                    memset(&(p->places[p->used]), 0, 
                            sizeof(PlaceInfoContainer*)*p->used);
                } else {
                    p->total = MAX_EDITABLE_ELEMS/10;
                    G_MALLOC(p->places, PlaceInfoContainer*, p->total, 
                            "creating PlacementSet");
                    if (!p->places) {
                        *iret = -3;
                        return;
                    }
                    memset(p->places, 0, sizeof(PlaceInfoContainer*)*p->total);
                }
            }

            /*
             * Before we go moving things around, get a free object if its there
             */
            newplace = cap_psnewpl(p, iret);

            if (newplace) {
                if (location != p->used) {
                    memmove(&(p->places[location+1]), 
                            &(p->places[location]), 
                            sizeof(PlaceInfoContainer*)*(p->used - location)
                        );
                }
                newplace->id = id;
                newplace->reference = ref;
                newplace->allow_center = allow_center;
                newplace->both_sides_of_line = both_sides;
                newplace->max_attempts = max_tries;
                if (dist_incr > 0) {
                    newplace->dist_incr = dist_incr;
                } else {
                    newplace->dist_incr = 1.0;
                    *iret = -8;
                }
                if (dist_offset < 0) {
                    newplace->dist_offset = 0;
                } else {
                    newplace->dist_offset = dist_offset;
                }
                newplace->mode = pmode;
                newplace->point_to_center = point_center;

                p->places[location] = newplace;
                p->used++;
                *iret = 0;
            } 
        } else {
            *iret = 2;
        }
    } else {
        *iret = -1;
    }

    return;
}
