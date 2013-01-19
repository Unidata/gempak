/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs.h"

/* AODT library function */
extern int aodtv72_initcurrent(int);

int aodtv72_initialize(void)
/* Subroutine to initialize history structure variables for current analysis
   Inputs : none
   Outputs: none
   Return : 0 : o.k.
*/
{
  int iok;
  iok=aodtv72_initcurrent(0);  /* initialize all variables */

  return iok;
}
