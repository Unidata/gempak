/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv72_gettemps(int);

int aodtv72_seteyecloudtemp(void)
/* Routine to search for, idenfify, and set the eye and cloud temperature values
   for the AODT library.  Temperatuers are set within AODT library.
   Inputs : none
   Outputs: none
   Return : -51 : eye, CWcloud, or warmest temperature <-100C or >+40C
              0 : o.k.
*/
{
  int iok;

  iok=aodtv72_gettemps(keyerM_v72);

  return iok;
}
