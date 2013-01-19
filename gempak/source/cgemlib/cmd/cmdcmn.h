/************************************************************************
 * cmdcmn.h                                                             *
 * Contains structures and definitions for private functions and        *
 * datatype in the cmd library.                                         *
 **                                                                     *
 * S.Danz/AWC            2/06   Created                                 *
 ***********************************************************************/

#ifndef _cmdcmn_include
#define _cmdcmn_include
#include "geminc.h"
#include "gemprm.h"


typedef struct {
    float  min_x;
    float  max_x;
    float  min_y;
    float  max_y;
} CMDBoundingBox;


typedef struct {
    Handle          id;
    float           *vertex_x;
    float           *vertex_y;
    float           center_x;
    float           center_y;
    float           centroid_x;
    float           centroid_y;
    int             points;
    int             ispoly;
    int             isvisible;
    CMDBoundingBox  extent;
} CMDObjectContainer;


/* 
 * The set of objects is currently just an ordered array, sorted by the 
 * Handle to allow for fast lookups by Handle.  The array is also the 
 * free list, with each free object moved to the end of the array.
 */
typedef struct {
    int                 total;
    int                 used;
    int                 iter;
    int                 plot_area_valid;
    float               plot_area_x[5];
    float               plot_area_y[5];
    float               plot_bb[4];
    CMDObjectContainer  **objects;
} CMDObjectSetContainer;

/*
 * Private functions to manage CMDObjectContainers in the CMDObjectSetContainer
 */
CMDObjectContainer *cmd_osnewob(
    CMDObjectSetContainer*,
    int*
);

void cmd_obclear(
    CMDObjectContainer*
);

int cmd_osfindob(
    CMDObjectSetContainer*, 
    Handle, 
    int
);
#endif
