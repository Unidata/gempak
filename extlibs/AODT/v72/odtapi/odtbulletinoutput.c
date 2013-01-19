/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv72_textscreenoutput(char *);

int aodtv72_bulletinoutput(char *bulletin)
/* Subroutine to load final AODT intensity bulletin into a character string
   for output within API
   Inputs : none
   Outputs: character string containing intensity estimate bulletin 
   Return : 0 : o.k.
*/
{
  int iok;
  char *retstrng;

  retstrng=(char *)calloc((size_t)5000,(size_t)sizeof(char));
  iok=aodtv72_textscreenoutput(retstrng);
  strcpy(bulletin,retstrng);
  bulletin[strlen(retstrng)]='\0';
  free(retstrng);

  return 0;
}
