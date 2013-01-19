/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv72_windprofile(int,float *,float ***);

int aodtv72_getwindprofile(int vmax,float *vcrit,float ***v2d)
/* Routine to obtain wind profile information and array from Jim Kossin IR 
   wind profile routines
   Inputs : vmax  : input first guess wind speed (from model)
   Outputs: vcrit : array holding critical wind radii information
                    (35kt,50kt,65kt,100kt, and rmw wind)
            v2d   : 3D array holding lat, lon, and wind speed information grid
   Return : 0 : o.k.
*/
{
  int iok;

  iok=aodtv72_windprofile(vmax,vcrit,v2d);

  return iok;
}
