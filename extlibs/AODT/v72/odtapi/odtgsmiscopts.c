/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv72_getmiscoptions(int *land,int *search,int *eye)
/* return various AODT library variables from AODT library
   Inputs : none
   Outputs: various AODT library variables
   Return : 0 : o.k.
*/
{

  *land=oland_v72;          /* allow AODT operation over land */
  *search=osearch_v72;      /* search for maximum curved band position */
  *eye=rmwsizeman_v72;      /* eye size parameter */

  return 0;
}

int aodtv72_setmiscoptions(int land,int search,int eye)
/* set various AODT library variables within AODT library memory
   Inputs : various AODT library variables
   Outputs: none 
   Return : 0 : o.k.
*/
{

  oland_v72=land;           /* allow AODT operation over land */
  osearch_v72=search;       /* search for maximum curved band position */
  rmwsizeman_v72=eye;       /* eye size parameter */

  return 0;
}
