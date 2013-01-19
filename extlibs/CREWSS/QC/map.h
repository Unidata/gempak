/************************************************************************
 * map.h                                                                *
 *                                                                      *
 * This header file defines the map structure.                          *
 **                                                                     *
 * Log:                                                                 *
 * P. Freeman/NCEP       2/99                                           *
 ***********************************************************************/


typedef struct {                  /* map boundries                     */
        float  lat[2];            /* lat of upper left and lower right */
        float  lon[2];            /* lon of upper left and lower right */
        float  x[2];              /* ndc of upper left and lower right */
        float  y[2];              /* ndc of upper left and lower right */
}mapbnd;

mapbnd   MapBnd;                  /* map boundaries                    */
