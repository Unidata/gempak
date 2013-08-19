#include <geminc.h>
#include <gemprm.h>

#include <gbcmn.h>
#include <dccmn.h>

#include "dcgrib.h"
#include "mkdirs_open.h"
#include "dcsgrid.h"

void dcogfil ( char *gemfil, int *iflno, int maxgrid, int specgrid)
{
char newfil[512],errstr[512],filnam[512];
int ier,loglev,numerr,imax,i;
long flen;

int nx,ny;
float lon1,lon2,lat1,lat2;
float ang1,ang2,ang3;
int angflg,gsflag;
float rnvblk[LLNNAV],envblk[LLNNAV],anlblk[LLNANL];
int navsz, anlsz, hdrsz;
int test_iflno;

static char errgrp[]="DCGRIB";

char *gproj;
static char cedproj[]="CED";
static char merproj[]="MER";
static char lccproj[]="LCC";
static char sccproj[]="SCC";
static char strproj[]="STR";

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
 */
if(specgrid != 0)
   {
   gproj = gtype[sgridval].proj;
   lat1 = gtype[sgridval].gar[0];
   lon1 = gtype[sgridval].gar[1];
   lat2 = gtype[sgridval].gar[2];
   lon2 = gtype[sgridval].gar[3];
   ang1 = gtype[sgridval].ang[0];
   ang2 = gtype[sgridval].ang[1];
   ang3 = gtype[sgridval].ang[2];
   nx = gtype[sgridval].nx;
   ny = gtype[sgridval].ny;
   }
else
   {
   switch(gds.grid_proj)
      {
      case 0:
         gproj = cedproj;
         break;
      case 1:
         gproj = merproj;
         break;
      case 3:
	 if ( gds.angle1 < 0 )
            {
            if ( ( gds.flag2 >> 7 ) == 0 )
               {
	       loglev = 0;
               numerr = 4;
               sprintf(errstr,"Warning, SCC grid with unexpected GDS.flag2 %d\0",
		   gds.flag2);
               dc_wclg(loglev,errgrp,numerr,errstr,&ier);
               }
            gproj = sccproj;
            }
	 else
	    gproj = lccproj;

         break;
      case 5:
         gproj = strproj;
         break;
      case 10:
	 gproj = merproj;
         break;
      case 203:
         gproj = cedproj;
         break;
      default:
         loglev = 0;
         numerr = 3;
         sprintf(errstr,"unknown projection %d\0",gds.grid_proj);
         dc_wclg(loglev,errgrp,numerr,errstr,&ier);
         return;
      }
   lon1 = gds.lonll;
   lat1 = gds.latll;
   lon2 = gds.lonur;
   lat2 = gds.latur;
   ang1 = gds.angle1;
   ang2 = gds.angle2;
   ang3 = gds.angle3;
   nx = gds.kx;
   ny = gds.ky;
   }

if(strncmp(gproj,"CED",3) == 0)
   {
   if((ang1 != 0)&&(ang2 != 0))
      angflg = TRUE;
   else
      angflg = FALSE;
   }
else
   angflg = TRUE;

gr_mnav(gproj,&nx,&ny,&lat1,&lon1,&lat2,&lon2,&ang1,&ang2,&ang3,
        &angflg, rnvblk, &ier,strlen(gproj));

if ( ier != 0 )
   {
   loglev = 0;
   numerr = 3;
   sprintf(errstr,"gr_mnav failed %d [%d %d]\0",gds.grid_proj,nx,ny);
   dc_wclg(loglev,errgrp,numerr,errstr,&ier);
   }

cfl_inqr( filnam, NULL, &flen, newfil, &ier);

if(ier != 0) /* file does not exist (should we unlink a 0 length file?) */
   {
   navsz = LLNNAV;
   anlsz = LLNANL;
   hdrsz = 10;
   for(i=0;i<anlsz;i++) anlblk[i] = 0.0;
   gd_cref(filnam,&navsz,rnvblk,&anlsz,anlblk,&hdrsz,&maxgrid,iflno,&ier,
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
   gr_cnav(rnvblk,envblk,&navsz,&gsflag,&ier);
   if(!gsflag)
      {
      gd_clos(iflno,&ier);
      *iflno = -1;
      loglev = 0;
      numerr = 1;
      sprintf(errstr,"Grid navigation %d incompatible with file %s\0",
         pds.grid_id,filnam);
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
