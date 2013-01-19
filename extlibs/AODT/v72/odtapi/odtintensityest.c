/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv72_calcintensity(void);

int aodtv72_intensity(void)
/* Routine to calculate intensity values and store values in AODT library.
   Inputs : none
   Outputs: none
   Return : 71 : storm is over land
             0 : o.k.
*/
{
  int iok;

  iok=aodtv72_calcintensity();

  return iok;
}
