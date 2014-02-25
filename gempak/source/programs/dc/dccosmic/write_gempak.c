#include <geminc.h>
#include <gemprm.h>
#include <bridge.h>
#include <mkdirs_open.h>
#include "ulog.h"
#include "cosmic.h"

#ifdef UNDERSCORE
#define	sf_wsdd	sf_wsdd_
#define dc_fcyl	dc_fcyl_
#define dc_fint	dc_fint_
#endif

void    dc_fint ( int *numfil, int *itype, char *parmfl, int *iret, size_t);
void    dc_fcyl ( char *filnam, int *iflsrc, char *stnfil, int *iadstn,
                  int *maxtim, int *iflno, int *nparm, char prmlist[][4],
                  int *iret, size_t, size_t, size_t);

void	sf_uare ( int *isffln, char *area, int *newfil, char *arecur,
		  char *stn, int *iret, size_t, size_t, size_t);
void	sf_snxt ( int *isffln, char *stid, int *istnm, float *slat, float *slon,
		  float *selv, int *ispri, int *iret, size_t);
void	sf_wsdd ( int *isffln, char *dattim, char *stid, int *istnm,
		  float *slat, float *slon, float *selv, char *stat, char *coun,
		  int *ihhmm, float *sfdata, int *iret, size_t, size_t, 
		  size_t, size_t);

#define MAXLEV	1
#define NUMVARS	6
#define HGHT	0
#define	TMPC	1
#define	DWPC	2
#define	PRES	3
#define	REFA	4
#define	REFO	5

char dcdlog[DCMXLN];
char cprgnm[DCMXLN];
int ipid;
int ivrblv=2;
int irltim=G_TRUE;
int logflg=G_FALSE;
extern int filebin;


void write_gempak(char *ofil, cosmic_struct *head, char *logfname, int *iret)
{
cosmic_struct *p;
int i,ier;
char dattim[12],fdattim[12],gemfil[256];

/* note: maxstat not used for ship files */
int maxstat=9999,maxtim=25000,iflno;
int maxfiles=2,iflsrc=2 /*SHIP FILE */;
int nparms;
char packfl[]="cosmic.pack";
char statfl[]="profiler_fsl.stn";
char prmlist[MMPARM][4];

char stid[9],state[3],coun[3];
float slat[1],slon[1],selv[1];
int stnm[1],numlevs,itim;
float dataray[MAXLEV*MMPARM];
int pos[NUMVARS];
int isdup;
float latdif,londif;
int nnam;
float rnam;
int ifactor;
int ispri;
char area[40],areacur[40];
int newfil;

dcdlog[0] = '\0';
if(logfname != NULL)
   strcpy(dcdlog,logfname);
else
   strncat(dcdlog,"-",1);
cprgnm[0] = '\0';
sprintf(cprgnm,"dccosmic\0");
ipid = (int) getpid();

udebug("write_gempak %s logs %s\0",ofil,logfname);
/* make sure we can get to directory for output */
if(diraccess(ofil, (R_OK | W_OK), !0) == -1)
   {
   serror("Couldn't access directories leading to %s", ofil);
   *iret = -1;
   return;
   }

memset(prmlist,'\0',4*MMPARM);

in_bdta(&ier);

dc_fint(&maxfiles,&iflsrc,packfl,&ier,strlen(packfl));

p = head;
while(p != NULL)
   {
   fdattim[0] = '\0';
   if((strstr(ofil,"NN") != 0)&&(filebin > 0))
      {
      ifactor = (p->minute / filebin) * filebin;
      sprintf(fdattim,"%02d%02d%02d/%02d%02d\0",
         p->year%100,p->month,p->day,p->hour,ifactor);
      }
   else
      sprintf(fdattim,"%02d%02d%02d/%02d%02d\0",
         p->year%100,p->month,p->day,p->hour,p->minute);

   cfl_mnam(fdattim, ofil, gemfil, &ier);
   if(ier != 0)
      uerror("cfl_mnam[%d] %s\0",ier,fdattim);

   dattim[0] = '\0';
   sprintf(dattim,"%02d%02d%02d/%02d%02d\0",
      p->year%100,p->month,p->day,p->hour,p->minute);

   i = 0;
   dc_fcyl(gemfil,&iflsrc,statfl,&maxstat,&maxtim,&iflno,&i,prmlist,&ier,
           strlen(gemfil),strlen(statfl),sizeof(prmlist[0]));
   if(ier != 0) 
      {
      uerror("Could not open gempak file %s\0",gemfil);
      *iret = -1;
      return;
      }
   if(i > MMPARM)
      uerror("Returned more parameters than allowed: %d\0",MMPARM);
   if(i > 0) nparms = i;
   for(i=0;i<NUMVARS;i++) pos[i] = -1;
   for(i=0;i<nparms;i++)
      {
      if(strncmp(prmlist[i],"PRES",4) == 0)
         pos[PRES] = i;
      if(strncmp(prmlist[i],"HGHT",4) == 0)
         pos[HGHT] = i;
      if(strncmp(prmlist[i],"TMPC",4) == 0)
         pos[TMPC] = i;
      if(strncmp(prmlist[i],"DWPC",4) == 0)
         pos[DWPC] = i;
      if(strncmp(prmlist[i],"REFA",4) == 0)
         pos[REFA] = i;
      if(strncmp(prmlist[i],"REFO",4) == 0)
         pos[REFO] = i;
      }

   udebug("Station data for: %s %s\0",dattim,gemfil);
   udebug("   Lat: %7.2f Lon: %7.2f Alt: %7.2f  Pres: %7.2f Tmpc %7.2f Dwpc %7.2f Refa %7.2f Refo %7.2f\0",
      p->Lat, p->Lon, p->Alt, p->press, p->tmpc, p->dwpc, p->refa, p->refo);

   isdup = 0; 
   sf_stim(&iflno,dattim,&ier,strlen(dattim));

   /*
   if(ier == 0)
      {
      sprintf(area,"@%s\0",p->tailNumber);
      memset(stid,' ',sizeof(stid)); memset(areacur,' ',sizeof(areacur)); newfil = 1;
      sf_uare(&iflno,area,&newfil,areacur,stid,&ier,strlen(area),sizeof(areacur),sizeof(stid));
      while((ier == 0)&&(isdup == 0))
         {
         sf_snxt(&iflno,stid,stnm,slat,slon,selv,&ispri,&ier, sizeof(stid));
         if(ier == 0)
            {
            latdif = fabs(slat[0] - p->Lat);
            londif = fabs(slon[0] - p->Lon);
	    ** lat and lon are truncated to nearest .01 degree in sf_wsdd **
            if((latdif < .0100)&&(londif < .0100))
              {
              uinfo("Station %s [%f %f] at %s already in file\0",
                 p->tailNumber,p->Lat,p->Lon,dattim);
              ** isdup can be set to not store data already received **
              ** note that really need to compare "SECS" field **
              isdup = 1;
              }
            }
         }
      }
   */


   if(isdup == 0)
      {
      udebug("Storing [%f %f] at %s\n",p->Lat,p->Lon,dattim);
      stid[0] = '\0'; strncat(stid,"COSMIC",6);
      stnm[0] = -99999;
      slat[0] = p->Lat;
      slon[0] = p->Lon;
      selv[0] = p->Alt;
      strcpy(state,"--\0");
      strcpy(coun,"--\0");
      
      numlevs = 0;
      if(pos[PRES] >= 0)
         dataray[numlevs*nparms + pos[PRES]] = p->press;
      if(pos[HGHT] >= 0)
         dataray[numlevs*nparms + pos[HGHT]] = p->Alt;
      if(pos[TMPC] >= 0)
         dataray[numlevs*nparms + pos[TMPC]] = p->tmpc;
      if(pos[DWPC] >= 0)
         dataray[numlevs*nparms + pos[DWPC]] = p->dwpc;
      if(pos[REFA] >= 0)
         dataray[numlevs*nparms + pos[REFA]] = p->refa;
      if(pos[REFO] >= 0)
         dataray[numlevs*nparms + pos[REFO]] = p->refo;
   
      itim = p->hour*100 + p->minute;
      numlevs++; 
      sf_wsdd(&iflno,dattim,stid,stnm,slat,slon,selv,state,coun,&itim,dataray,&ier,
         strlen(dattim),strlen(stid),strlen(state),strlen(coun));
      if(ier != 0)
	uerror("Error writing data to %s [%d]\0",gemfil,ier);

      }

   p = p->next;
   }

if(iflno != -1) sf_clos(&iflno,&ier);
*iret = 0;
}
