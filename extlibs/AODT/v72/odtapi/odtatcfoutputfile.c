/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

extern int aodtv72_atcffilename(char *,int,char *,char *);

int aodtv72_atcfoutputfile( char *inpath, int itype, char *stormID, char *outfile )
/* return atcf full path and file name given path and storm number.  Will use
   current record date and time info to create name.
   Inputs  : inpath - atcf file path name
             itype  - storm number
   Outputs : outfile- output filename
   Return  :  0 : o.k.
*/
{
    int iok;
    char *retstrng;

    iok=aodtv72_atcffilename(inpath,itype,stormID,outfile);

    return 0;
}
