/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv72_getforecastfile( char *outchar,int *type )
/* return AODT library automode forecast file name
   Inputs : none
   Outputs: AODT library forecast file name
            file type : 0-ATCF
	                1-NHC forecast discussion
			2-JTWC forecast
			3-generic
   Return : -51 : error access forecast file 
             14 : o.k.
*/
{
  int iret;

  /* check for valid forecast file name */
  if(strlen(fixfile_v72)>0) {
  /* if(fixfile_v72!=(char *)NULL) { */
    /* set forecast file name to return variable */
    strcpy(outchar,fixfile_v72);
    outchar[strlen(fixfile_v72)]='\0';
    iret=14;
  } else {
    iret=-51;
  }
  *type=ifixtype_v72;

  return iret;
}

int aodtv72_setforecastfile( char *inchar,int type )
/* set forecast file name and type within AODT library memory
   Inputs : AODT library forecast file name
            file type : 0-ATCF
	                1-NHC forecast discussion
			2-JTWC forecast
			3-generic
   Outputs: none
   Return : 0 : o.k.
*/
{
  /* assign forecast file name to AODT library variable */
  strcpy(fixfile_v72,inchar);
  fixfile_v72[strlen(inchar)]='\0';
  ifixtype_v72=type;

  return 0;
}
