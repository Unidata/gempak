#include <geminc.h>
#include <gemprm.h>
#include <dccmn.h>


#ifdef UNDERSCORE
#define dc_fcyl	dc_fcyl_
#define dc_fcls	dc_fcls_
#define sfnldn	sfnldn_
#define	nldnflush nldnflush_
#define	setcparm setcparm_
#endif

#include "dcuspln.h"

void	dc_fcls ( int *iret);
void    dc_fcyl ( char *filnam, int *iflsrc, char *stnfil, int *iadstn,
                  int *maxtim, int *iflno, int *nparm, char prmlist[][4],
                  int *iret, Cardinal, Cardinal, Cardinal);

/* the order of these known variables to be decoded
** must match the order in setcparm.f 
*/
#define TSEC	0
#define MSEC	1
#define SGNL	2
#define MULT	3
#define SMAJ	4
#define ECNT	5
#define	ANGL	6
#define CHI2	7

void templseq();

int write_point(nldn_file ltgf, nldn_flash flashdat, int *iret)
{
int i,numerr,loglev,ier,isfull;
char errstr[80],filnam[256];
static char tmplnam[256], oldnam[256];
static char errgrp[] = {"write_point"};
static int iflno=-1;
char cparms[40][4],stid[5];
int nparms;
float ndata[8];
static int jparms[40];
static char stnfil[] = {" "};
static int nadstn = 0;
int year, month, day, hour, minute, seconds, isecoff;
int idtarr[5],idate,itime,ihhmm;
time_t obs_time;
char timestr[81],gemtime[20];
struct tm *gmt_time=NULL,new_time;
static int filseq=-1;
/*
printf("flashdat.angle = %f\n", flashdat.angle);
*/
if(iflno < 0)
   {
   tmplnam[0] = '\0'; oldnam[0] = '\0';
   }

obs_time = (time_t) flashdat.sec;
gmt_time = gmtime(&obs_time);
new_time = *gmt_time; /* copy the time since gmtime pointer is volotile */
timestr[0] = '\0';
strftime(timestr,80,"%Y %m %d %H %M %S",&new_time);
sscanf(timestr,"%d %d %d %d %d %d",&year,&month,&day,&hour,&minute,&seconds);
idtarr[0] = year;
idtarr[1] = month;
idtarr[2] = day;
idtarr[3] = hour;
/* if itype ==4 using minute bins, use bin for NN template */
if(ltgf.itype == 4)
   idtarr[4] = ltgf.ibin*(minute / ltgf.ibin);
else
   idtarr[4] = minute;

ti_itoc(idtarr,gemtime,&ier,sizeof(gemtime));
for(i=0;i<sizeof(gemtime);i++)
   if(gemtime[i] <= ' ') gemtime[i] = '\0';

/* get  ihhmm idate and itime for data storage using sfnldn */
idate = (year % 100 ) * 10000 + month * 100 + day;
ihhmm = hour * 100 + minute; /* do ifactor */
if(ltgf.itype == 4)
   itime = ltgf.ifactor*(minute / ltgf.ifactor);
else
   itime = 0;
itime = itime + (hour * 100);

/* calculate seconds in the day */
isecoff = (new_time.tm_hour * 3600) + (new_time.tm_min * 60) + new_time.tm_sec;

cfl_mnam(gemtime, ltgf.gemfil, filnam, &ier );

strcpy(tmplnam,filnam);

if( oldnam[0] == '\0' ) {
   filseq = 0;
   templseq(tmplnam,filnam,filseq);
   dc_fcyl(filnam,&ltgf.iflsrc,stnfil,&nadstn,&ltgf.maxtim,
       &iflno, &nparms, cparms, &ier,
       strlen(filnam),strlen(stnfil),sizeof(cparms[0])); 
   sprintf(errstr,"Opened output file %s\0",filnam);
   loglev = 1; numerr = ier;
   dc_wclg(loglev, errgrp, numerr, errstr, &ier);
   setcparm(cparms,&nparms,jparms,4);
} else {
   if(strcmp(oldnam,tmplnam) != 0) {
      nldnflush(&iflno,&ier);
      dc_fcls(&ier);
      filseq = 0;
      templseq(tmplnam,filnam,filseq);
      dc_fcyl(filnam,&ltgf.iflsrc,stnfil,&nadstn,&ltgf.maxtim,
         &iflno, &nparms, cparms, &ier,
         strlen(filnam),strlen(stnfil),sizeof(cparms[0]));
      sprintf(errstr,"New output file %s\0",filnam);
      loglev = 1; numerr = ier;
      dc_wclg(loglev, errgrp, numerr, errstr, &ier);
      setcparm(cparms,&nparms,jparms,4);
   }
}

/* do the write */
memset(stid,0,5);
if( flashdat.sgnl < 0 )
   strcat(stid,"-   ");
else
   strcat(stid,"+   ");


for(i=0;i<8;i++) ndata[i] = -9999.;

if( jparms[SGNL] != 0 ) ndata[jparms[SGNL] - 1] = flashdat.sgnl;
if( jparms[MULT] != 0 ) ndata[jparms[MULT] - 1] = flashdat.mult;
if( jparms[TSEC] != 0 ) ndata[jparms[TSEC] - 1] = isecoff;
if( jparms[MSEC] != 0 ) ndata[jparms[MSEC] - 1] = flashdat.nsec/1e6;
if( jparms[SMAJ] != 0 ) ndata[jparms[SMAJ] - 1] = flashdat.semimaj;
if( jparms[ECNT] != 0 ) ndata[jparms[ECNT] - 1] = flashdat.eccent;
if( jparms[ANGL] != 0 ) ndata[jparms[ANGL] - 1] = flashdat.angle;
if( jparms[CHI2] != 0 ) ndata[jparms[CHI2] - 1] = flashdat.chisqr;

sprintf(errstr,"%s %6.2f %7.2f\0",gemtime,flashdat.lat,flashdat.lon);

loglev = 3; numerr = 0;
dc_wclg(loglev, errgrp, numerr, errstr, &ier);

isfull = 0;
sfnldn(&iflno,&idate,&itime,stid,&flashdat.lat,&flashdat.lon,
       &ihhmm, ndata, &ier, 4);
if(ier != 0)
   {
   if(ier == -1) isfull = -1;
   sprintf(errstr,"%s %d\0",filnam,ier);
   loglev = 1; numerr = 0;
   dc_wclg(loglev, errgrp, numerr, errstr, &ier);
   }

while((isfull == -1)&&(filseq < 100))
   {
   nldnflush(&iflno,&ier);
   dc_fcls(&ier);
   filseq++;
   templseq(tmplnam,filnam,filseq);
   dc_fcyl(filnam,&ltgf.iflsrc,stnfil,&nadstn,&ltgf.maxtim,
      &iflno, &nparms, cparms, &ier,
      strlen(filnam),strlen(stnfil),sizeof(cparms[0]));
   setcparm(cparms,&nparms,jparms,4);
   printf("got here...need to reload ndata\n");
   sfnldn(&iflno,&idate,&itime,stid,&flashdat.lat,&flashdat.lon,
       &ihhmm, ndata, &ier, 4);
   isfull = ier;
   loglev = 1; numerr = ier;
   sprintf(errstr,"New output file %s\0",filnam);
   dc_wclg(loglev, errgrp, numerr, errstr, &ier);
   }

oldnam[0] = '\0';
strcat(oldnam,tmplnam);
*iret = ier;
return(iflno);
}

void templseq(tmplnam,filnam,iseq)
char *tmplnam,*filnam;
int iseq;
{
char *seqpos,chseq[3];

filnam[0] = '\0';
strcat(filnam,tmplnam);

seqpos = (char *)strstr(filnam,"@@");
chseq[0] = '\0';
sprintf(chseq,"%02d\0",iseq);
if(seqpos != NULL)
   {
   seqpos[0] = chseq[0];
   seqpos[1] = chseq[1];
   }
else
   {
   seqpos = (char *)strstr(filnam,"_%%");
   if(seqpos != NULL)
      {
      if(iseq == 0)
         seqpos[0] = '\0';
      else
         {
         seqpos[1] = chseq[0];
         seqpos[2] = chseq[1];
         }
      }
   }
}

