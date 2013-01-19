/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv72_calcscene(void);

int aodtv72_scenetype(void)
/* Routine to calculate scene type and store values in AODT library.
   Inputs : none
   Outputs: none
   Return : -41 : Error with Fourier Transform Analysis
            -51 : cloud temperature <-100C or >+40C
              0 : Good return
*/
{
  int iok;

  iok=aodtv72_calcscene();

  return iok;
}
