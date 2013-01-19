/************************************************************************
 * capcmn.h                                                             *
 * Contains structures and definitions for private functions and        *
 * datatypes in the cap library.                                        *
 **                                                                     *
 * S.Danz/AWC            3/06   Created                                 *
 ***********************************************************************/
#ifndef _capcmn_include
#define _capcmn_include
#include "geminc.h"
#include "gemprm.h"

#define cap_mergebbox(a,b,r) \
(r)[0] = G_MIN((a)[0],(b)[0]);\
(r)[1] = G_MAX((a)[1],(b)[1]);\
(r)[2] = G_MIN((a)[2],(b)[2]);\
(r)[3] = G_MAX((a)[3],(b)[3]);

/* 
 * Typedefs for the internal data managed by the CAP placement library 
 */

typedef struct {
    float   delta_x;
    float   delta_y;
} CAPOffset;

typedef struct {
    /* 
     * Information set by the client about the placement to be performed 
     */
    Handle          id;
    Handle          reference;
    int             allow_center;
    int             both_sides_of_line;
    int             max_attempts;
    int             point_to_center;
    float           dist_incr;
    float           dist_offset;
    PlacementMode   mode;
    int             clutter;

    /* 
     * Results computed by the library 
     */
    int             was_placed;
    int             in_center;
    CAPOffset       offset;
    float           arrow_x[2];
    float           arrow_y[2];
} PlaceInfoContainer;


/* 
 * The set of placements is currently just an ordered array, sorted by the 
 * Handle defined in the placement info to allow for fast lookups by
 * Handle.  The array is also the free list, with each free placement moved
 * to the end of the array.
 */
typedef struct {
    int                 total;
    int                 iter;
    int                 used;
    int                 plot_area_valid;
    float               plot_area_x[5];
    float               plot_area_y[5];
    float               plot_bb[4];
    float               distance;
    float               step_incr;
    float               default_dist;
    float               default_incr;
    PlaceInfoContainer  **places;
} PlacementSetContainer;


/*
 * Private functions to manage PlaceInfoContainers in the PlacementSetContainer
 */

PlaceInfoContainer* cap_psnewpl(
        PlacementSetContainer   *placements,
        int                     *iret
    );

void cap_plclear(
        PlaceInfoContainer      *place
    );


int cap_psfindpl(
        PlacementSetContainer   *placements,
        Handle                  id,
        int                     exact_match
    );

void cap_psgetplmaxdist(
        PlacementSet            placements,
        PlaceInfoContainer      *placeinfo, 
        float                   bbox[4],
        float                   *max_dist,
        int                     *iret
    );

#endif
