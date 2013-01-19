/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv72_textscreenoutput(char *);

int aodtv72_historybullfmt(struct odtdata *historyrec,char *listing)
/* Subroutine to load AODT history file record, listed in bulletin output format,
   into a character string for output within API.
   Inputs : pointer to history structure entry
   Outputs: intensity estimage in bulletin format (character string)
   Return : 0 : o.k.
*/
{
  int iok;
  char *retstrng;

  retstrng=(char *)calloc((size_t)2000,(size_t)sizeof(char));
  odtcurrent_v72=historyrec;
  iok=aodtv72_textscreenoutput(retstrng);
  strcpy(listing,retstrng);
  listing[strlen(retstrng)]='\0';

  free(retstrng);
  return 0;
}
