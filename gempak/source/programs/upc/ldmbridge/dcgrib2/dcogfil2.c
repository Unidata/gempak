#include "gb2def.h"
#include <gbcmn.h>
#include <dccmn.h>

#include "dcgrib.h"
#include "mkdirs_open.h"
#include "dcsgrid.h"

void dcogfil2 ( char *gemfil, int *iflno, int maxgrid, Geminfo ginfo)
{
char newfil[512],errstr[512],filnam[512];
int ier,loglev,numerr,imax,i;
long flen;

int gsflag;
float envblk[LLNNAV],anlblk[LLNANL];
int navsz, anlsz, hdrsz;
int test_iflno;

static char errgrp[]="DCGRIB";

css_envr( gemfil, filnam, &ier );

/*
 * See if this file is already open 
 */
dcgfcyl(filnam,&test_iflno,&ier,strlen(filnam));
if(ier == 0)
   {
   *iflno = test_iflno;
   loglev = 3;
   numerr = 0;
   sprintf(errstr,"writing to %s\0",filnam);
   dc_wclg(loglev,errgrp,numerr,errstr,&ier);
   return;
   }


/* 
 * make sure directories to file exist, and create as needed
 */
if (diraccess(filnam,  (R_OK | W_OK), !0) == -1) 
   {
   loglev = 0;
   numerr = 0;
   sprintf(errstr,"can't access directories leading to %s\0", filnam);
   dc_wclg(loglev,errgrp,numerr,errstr,&ier);
   exit(1);
   }

/*
 * create projection information
 * no specgrids possible in grib2
 */

cfl_inqr( filnam, NULL, &flen, newfil, &ier);

if(ier != 0) /* file does not exist (should we unlink a 0 length file?) */
   {
   navsz = LLNNAV;
   anlsz = LLNANL;
   hdrsz = 10;
   for(i=0;i<anlsz;i++) anlblk[i] = 0.0;
   gd_cref(filnam,&navsz,ginfo.navblk,&anlsz,anlblk,&hdrsz,&maxgrid,iflno,&ier,
           strlen(filnam));

   if(ier < 0)
      {
      loglev = 0;
      numerr = 2;
      sprintf(errstr,"Error creating file [%d]: %s\0",
         ier,filnam);
      dc_wclg(loglev,errgrp,numerr,errstr,&ier);
      *iflno = -1;
      return;
      }
   }
else
   {
   gd_opnr(filnam,iflno,&navsz,envblk,&anlsz,anlblk,&hdrsz,&imax,&ier,
      strlen(filnam));
   gr_cnav(ginfo.navblk,envblk,&navsz,&gsflag,&ier);
   if(!gsflag)
      {
      gd_clos(iflno,&ier);
      *iflno = -1;
      loglev = 0;
      numerr = 1;
      sprintf(errstr,"Grid navigation %f %f %s incompatible with file %s\0",
         ginfo.gdsarr[1],ginfo.gdsarr[2],ginfo.cproj,filnam);
      dc_wclg(loglev,errgrp,numerr,errstr,&ier);
      return;
      }
   }


loglev = 1;
numerr = 0;
sprintf(errstr,"Opened %s model:%d grid:%d\0",filnam,pds.process,pds.grid_id);
dc_wclg(loglev,errgrp,numerr,errstr,&ier);

/*
** store file name in open list 
*/
dcgfiles(filnam,iflno,&ier,strlen(filnam));

}
