/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv72_gettemps(int);

int aodtv72_getwarmeyetemplocation(float *lat, float *lon)
/* Routine to search for warmest pixel temperature within search radius
   and return location to API.
   Inputs : none
   Outputs: warmest pixel location from automated storm positioning routine
   Return : 91 : good warmest pixel latitude/longitude
*/
{
  int iok;

  iok=aodtv72_gettemps(keyerA_v72);
  *lat=odtcurrent_v72->IR.warmlatitude;
  *lon=odtcurrent_v72->IR.warmlongitude;

  return 91;
}
