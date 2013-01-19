/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv72_getversion( char *version  )
/* returns string variable containing the current AODT Version number 
   Inputs : none
   Outputs: AODT version number string
   Return : 0 : o.k.
*/
{
  strcpy(version,"ADT-Version 7.2.1");
  version[strlen(version)]='\0';
  return 0;
}
