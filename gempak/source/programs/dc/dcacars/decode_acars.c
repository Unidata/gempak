#include <stdio.h>
#include <string.h>
#include <netcdf.h>

#include "acars.h"
#include "ulog.h"

acars_struct *decode_acars(char *infilnam, long miss, int *iret)
{
int ier,cdfid;
int title,version;
acars_struct *head;

ier = nc_open(infilnam,0,&cdfid);
if(ier != 0)
   {
   *iret = -1;
   uerror("Could not open %s\0",infilnam);
   return(NULL);
   }
udebug("decode_acars %s miss %ld ier %d cdfid %d\0",infilnam,miss,ier,cdfid);

ier = nc_inq_attid(cdfid,NC_GLOBAL,"title", &title);
ier = nc_inq_attid(cdfid,NC_GLOBAL,"version", &version);

head = decode_ncacars(cdfid,miss,&ier);

ier = nc_close(cdfid);

*iret = 0;
return(head);
}
