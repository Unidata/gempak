/*
 *      Copyright 1996, University Corporation for Atmospheric Research.
 *         Not for Direct Resale. All copies to include this notice.
 *	
 *	Chiz/Unidata	04/96
 */
#include <geminc.h>
#include <gemprm.h>
/*
#include <stdio.h>
#include <string.h>
#include <math.h>
*/


#ifdef UNDERSCORE
#define dcwwstore dcwwstore_

#define sf_ddat sf_ddat_
#define sf_tnxt sf_tnxt_
#define sf_tstn sf_tstn_
#define sf_wsdd sf_wsdd_
#endif

#define MAXVARS 15
#define SEVERE	1
#define	TORNADO	2
#define EW	1
#define NS	2
#define ES	3
#define KM_SM	1.609347    /* Statute miles to kilometers */

#define ISSUE	0
#define TYPE	1
#define START	2
#define STOP	3
#define WNUM	4
#define LAT1	5
#define LON1	6
#define LAT2	7
#define LON2	8
#define LAT3	9
#define LON3	10
#define LAT4	11
#define LON4	12
#define CANCEL	13
#define CORR    14


void dcwwstore(int *iflno, char *gemtime, float rdata[], int *iret)
{
float slat=-9999.0,slon=-9999.0,selv=0;
char state[]="--", country[]="US";
char stid[9],ostid[9];
int isnum,wnum;
int hour;
int ier;
int exist;
int ispri;
int loglev,ilog;
char logid[]="DCWATCH";
char errstr[81];

*iret = 0;

wnum = (int)rdata[WNUM];
sprintf(stid,"WW%04d  \0",wnum);
sprintf(ostid,"        \0");



if(rdata[CANCEL] != 0)
   {
   loglev = 1;
   errstr[0] = '\0';
   sprintf(errstr,"watch %d is cancelled\0",wnum);
   dc_wclg( loglev, logid, *iret, errstr, &ilog);
   sf_tstn(iflno,stid,&ier,sizeof(stid)-1);
   if(ier == 0)
      {
      sf_tnxt(iflno,gemtime,&ier,strlen(gemtime));
      if(ier == 0) 
         {
         sf_rdat(iflno,rdata,&hour,&ier);
         rdata[CANCEL] = 1;
         if(ier == 0)
            sf_wdat(iflno,&hour,rdata,&ier);
         }
      else
         {
         *iret = -10;
         loglev = 1;
         errstr[0] = '\0';
         sprintf(errstr,"time not found to cancel %d\0",ier);
         dc_wclg( loglev, logid, *iret, errstr, &ilog);
         }
      }
   else
      {
      *iret = -10;
      loglev = 1;
      errstr[0] = '\0';
      sprintf(errstr,"watch %d was not in file %d\0",wnum,ier);
      dc_wclg( loglev, logid, *iret, errstr, &ilog);
      }
   return;
   }

hour = ((int)rdata[ISSUE]) % 10000;

/* If time exists, and watch number exist, and this is not a
   correction- it is a duplicate....return */
sf_stim(iflno,gemtime,&ier,strlen(gemtime));
if(ier == 0)
   {
   sf_sstn(iflno,stid,ostid,&isnum,&slat,&slon,&selv,&ispri,&ier,
		    sizeof(stid)-1,sizeof(ostid)-1);
   if((ier == 0)&&(rdata[CORR] == 0))
      {
      loglev = 1;
      errstr[0] = '\0';
      sprintf(errstr,"Duplicate report for %s\0",stid);
      dc_wclg( loglev, logid, ier, errstr, &ilog);
      return;
      }
   }

if(rdata[CORR] == 0)   /* not a correction, not a duplicate */
   {
   loglev = 2;
   errstr[0] = '\0';
   sprintf(errstr,"writing %s %s\0",gemtime,stid);
   dc_wclg( loglev, logid, *iret, errstr, &ilog);
   sf_wsdd(iflno,gemtime,stid,&wnum,&slat,&slon,&selv,state,country,&hour,rdata,&ier,
      strlen(gemtime),sizeof(stid)-1,strlen(state),strlen(country));
   }
else
   {                   /* Correct report */
   loglev = 1;
   errstr[0] = '\0';
   sprintf(errstr,"correction received: %s\0",stid);
   dc_wclg( loglev, logid, *iret, errstr, &ilog);
   sf_tstn(iflno,stid,&ier,sizeof(stid)-1);
   if(ier == 0)
      {
      sf_tnxt(iflno,gemtime,&ier,strlen(gemtime));
      if(ier == 0)
         {
         sf_ddat(iflno,&ier);
         if(ier != 0)
            {
            loglev = 1;
            errstr[0] = '\0';
            sprintf(errstr,"Error deleting old data: WW%04d\0",wnum);
            dc_wclg( loglev, logid, ier, errstr, &ilog);
            }
         sf_wdat(iflno,&hour,rdata,&ier); /* write corrected report */
         }
      else
         {
         loglev = 1;
         errstr[0] = '\0';
         sprintf(errstr,"time not found to correct %s %s\0",
                 stid,gemtime);
         dc_wclg( loglev, logid, ier, errstr, &ilog);
         /* so lets still write this correction as a new entry */
         sf_wsdd(iflno,gemtime,stid,&wnum,&slat,&slon,&selv,state,country,
            &hour,rdata,&ier,strlen(gemtime),sizeof(stid)-1,strlen(state),strlen(country));
         }
      }
   else
      {
      loglev = 1;
      errstr[0] = '\0';
      sprintf(errstr,"didn't find station in file WW%04d\0",wnum);
      dc_wclg( loglev, logid, ier, errstr, &ilog);
      /* so lets still write this correction as a new entry */
      sf_wsdd(iflno,gemtime,stid,&wnum,&slat,&slon,&selv,state,country,
         &hour,rdata,&ier,strlen(gemtime),sizeof(stid)-1,strlen(state),
         strlen(country));
      }
   }

}
