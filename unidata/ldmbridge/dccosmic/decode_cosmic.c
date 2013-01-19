#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <netcdf.h>

#include "cosmic.h"
#include "ulog.h"

cosmic_struct *decode_cosmic(char *infilnam, long miss, int *iret)
{
static int isinit = !0;
int ier,cdfid;
int occattid = -1, refsatid = -1, occsatid = -1;
cosmic_struct *head;

    *iret = 0;

    if(isinit) {
       udebug("setting TZ\0");
       putenv("TZ=UTC0");
       tzset();
       isinit = 0;
    }

    ier = nc_open(infilnam,0,&cdfid); 
    if(ier != 0) {
       *iret = -1;
       uerror("Could not open %s\0",infilnam);
       return(NULL);
    }
    udebug("decode_cosmic %s miss %ld ier %d cdfid %d\0",infilnam,miss,ier,cdfid);
    
    ier = nc_inq_attid(cdfid,NC_GLOBAL,"occ_id", &occattid);
    ier += nc_inq_attid(cdfid,NC_GLOBAL,"reference_sat_id", &refsatid);
    ier += nc_inq_attid(cdfid,NC_GLOBAL,"occulting_sat_id", &occsatid);
    
    if ( ier != 0 ) {
        uerror("decode_cosmic Global att error %d %d %d\0",occattid,refsatid,occsatid);
        *iret = -2;
        head = NULL;
    }
    else {
        head = decode_nccosmic(cdfid,miss,&ier);
    }
    
    ier = nc_close(cdfid);
    
    return(head);
}
