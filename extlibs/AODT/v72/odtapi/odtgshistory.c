/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv72_readhistoryfile(void);

int aodtv72_gethistoryfile( char *outchar )
/* return history file name to application from AODT library
   Inputs : none
   Outputs: AODT library history file name
   Return : -1 : error reading history file
             0 : o.k.
*/
{
  int iret;
  /* check for valid history file name */
  if(hfile_v72!=(char *)NULL) {
    /* set history file name to return variable */
    strcpy(outchar,hfile_v72);
    outchar[strlen(hfile_v72)]='\0';  /* added by CDB */
    iret=0;
  } else {
    iret=-1;
  }

  return iret;
}

int aodtv72_sethistoryfile( char *inchar )
/* set history file name in AODT library and read history file into AODT library memory
   Inputs : history file name
   Outputs: none
   Return : -1 : error reading history file
	     11: successful read of history file
*/
{
  int iok,iret;

  /* assign history file name to AODT library variable */
  strcpy(hfile_v72,inchar);
  hfile_v72[strlen(inchar)]='\0';  /* added by CDB */
  
  /* read history file into AODT memory 
     function will return negative for error, or
     positive value for number of successful records read */
  iok=aodtv72_readhistoryfile();

  /* set return error code */
  iret=iok;
  if(iok>=0) iret=11;

  return iret;
}
