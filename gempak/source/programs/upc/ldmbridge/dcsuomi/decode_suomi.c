#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netcdf.h>

#include "suomi.h"
#include "ulog.h"

void *decode_suomi(char *infilnam, long miss, int *iret)
{
int ier,cdfid;
static int isinit=0;
suomi_struct *head;

if(isinit == 0)
   {
   isinit = -1;
   putenv("TZ=UTC0");
   tzset();
   }

ier = nc_open(infilnam,0,&cdfid);
if(ier != 0)
   {
   *iret = -1;
   uerror("Could not open %s\0",infilnam);
   return(NULL);
   }
udebug("decode_suomi %s miss %ld ier %d cdfid %d\0",infilnam,miss,ier,cdfid);


head = decode_ncsuomi(cdfid,miss,&ier);

ier = nc_close(cdfid);

*iret = 0;
return(head);
}
