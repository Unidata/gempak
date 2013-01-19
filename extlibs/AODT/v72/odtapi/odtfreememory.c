/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

int aodtv72_freememory(void)
/* free any stored areas of memory within AODT library
   Inputs : none
   Outputs: none
   Return :  0 : o.k.
            -1 : error freeing memory
*/
{
  int iret;
  struct odtdata *ptr;                      /* added by CDB */
  struct rinddata *ptr2;

  iret=0;
  if(odthistoryfirst_v72 != (struct odtdata *)NULL) {
    do {
      ptr=odthistoryfirst_v72->nextrec;         /* added by CDB */
      free(odthistoryfirst_v72);
      odthistoryfirst_v72=ptr;                  /* added by CDB */
    } while (odthistoryfirst_v72 != (struct aodtdata *)NULL);      /* added by CDB */
    odthistoryfirst_v72=NULL;
  }
  if(odtcurrent_v72 != (struct odtdata *)NULL) {
    free(odtcurrent_v72);
    odtcurrent_v72=NULL;                        /* added by CDB */
  }
  if(tcircfirst_v72 != (struct ringdata *)NULL) {
    do {
      ptr2=tcircfirst_v72->nextrec;
      free(tcircfirst_v72);
      tcircfirst_v72=ptr2; 
    } while (tcircfirst_v72 != (struct ringdata *)NULL); 
    tcircfirst_v72=NULL;
  }
  if(areadata_v72 != (struct datagrid *)NULL) {
    free(areadata_v72);
    areadata_v72=NULL;                          /* added by CDB */
  }

  if(diagnostics_v72 != (char *)NULL) {
    free(diagnostics_v72);
    diagnostics_v72=NULL;
  }
  if(hfile_v72 != (char *)NULL) {
    free(hfile_v72);
    hfile_v72=NULL;
  }
  if(fixfile_v72 != (char *)NULL) {
    free(fixfile_v72);
    fixfile_v72=NULL;
  }
  if(fcstlat_v72 != (char *)NULL) {
    free(fcstlat_v72);
    free(fcstlon_v72);
    free(fcsttime_v72);
    fcstlat_v72=NULL;
    fcstlon_v72=NULL;
    fcsttime_v72=NULL;
  }

  return iret;

}

