/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int aodtv72_historylistfmt(struct odtdata *historyrec,int itype,char *srcID,char *stormID,char *listing)
/* Subroutine to load AODT history file record listing into a character string
   for output within API.
   Passing in value of zero for historyrec will return listing column 
   header definitions.
   Inputs : pointer to history structure entry 
            listing format type (original or ATCF format)
   Outputs: intensity estimate listing for history structure entry
   Return : 0 : o.k.
*/
{
  int iok;
  char *retstrng;

  retstrng=(char *)calloc((size_t)50000,(size_t)sizeof(char));
  iok=aodtv72_listhistory(historyrec,itype,srcID,stormID,retstrng);
  strcpy(listing,retstrng);
  listing[strlen(retstrng)]='\0';

  free(retstrng);
  return 0;
}
