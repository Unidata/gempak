#include <geminc.h>
#include <gemprm.h>

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "ulog.h"
#include "suomi.h"
#include "bridge.h"

#include <mkdirs_open.h>

#ifdef UNDERSCORE
#define dc_fcyl	dc_fcyl_
#define dc_fint	dc_fint_
#define sf_astn	sf_astn_
#define sf_atim	sf_atim_
#define sf_fstn	sf_fstn_
#define sf_ftim	sf_ftim_

#define	sf_wdat	sf_wdat_
#define sf_clos	sf_clos_
#endif

void	dc_fint	( int *numfil, int *itype, char *parmfl, int *iret, size_t);
void	dc_fcyl ( char *filnam, int *iflsrc, char *stnfil, int *iadstn,
		  int *maxtim, int *iflno, int *nparm, char prmlist[][4], 
		  int *iret, size_t, size_t, size_t);
void	sf_astn ( int *isffln, int *nstn, char stid[][8], int *istnm,
		  float *slat, float *slon, float *selv, char stat[][2],
		  char coun[][2], int *ispri, int *nadd, int *iret,
		  size_t, size_t, size_t);
void	sf_atim ( int *isffln, char *dattim, int *iret, size_t);
void	sf_fstn	( int *isffln, char *stn, int *iret, size_t);
void	sf_ftim ( int *isffln, char *dattim, int *iret, size_t);
void	sf_wdat ( int *isffln, int *ihhmm, float *data, int *iret);

#define	MAXPRM	100

#define NUMVARS	11
#define	PRES	0
#define	TMPC	1
#define	RELH	2
#define	PWVM	3
#define PWVE	4
#define DELW	5
#define DELD	6
#define	DELT	7
#define	DELF	8
#define	PIFC	9
#define MFLG    10

char dcdlog[DCMXLN];
char cprgnm[DCMXLN];
int ipid;
int ivrblv=2;
int itmout=600;
int irltim=G_TRUE;
int logflg=G_FALSE;

void write_gempak( char *ofil, suomi_struct *head, char *logfname, int *iret)
{
suomi_struct *p;
suomi_obs *s;
int i,ier;
char dattim[12],gemfil[256];

/* note: maxstat is LLMXST - 1990 if no station table exists */
int maxstat=1000,maxtim=LLMXTM,iflno=-1;
int maxfiles=2,iflsrc=1;
int nparms;
char packfl[]="suomi.pack";
char statfl[]="suomi.tbl";
char prmlist[MAXPRM][4];

char stid[1][8],state[1][2],coun[1][2];
float slat[1],slon[1],selv[1];
int stnm[1],nadd;
float dataray[MAXPRM];
int ihhmm;
int pos[NUMVARS];
int proces;
int ispri;

dcdlog[0] = '\0';
if(logfname != NULL)
   strcpy(dcdlog,logfname);
else
   strncat(dcdlog,"-",1);
cprgnm[0] = '\0';
sprintf(cprgnm,"dcsuomi\0");
ipid = (int) getpid();

udebug("write_gempak %s logs %s\0",ofil,logfname);
/* make sure we can get to directory for output */
if(diraccess(ofil, (R_OK | W_OK), !0) == -1)
   {
   serror("Couldn't access directories leading to %s", ofil);
   *iret = -1;
   return;
   }

memset(prmlist,'\0',4*MAXPRM);

in_bdta(&ier);

dc_fint(&maxfiles,&iflsrc,packfl,&ier,strlen(packfl));

p = head;
while(p != NULL)
   {
   s = p->head;
   while(s != NULL)
      {
      dattim[0] = '\0';
      sprintf(dattim,"%02d%02d%02d/%02d%02d\0",
         s->year%100,s->month,s->day,s->hour,s->minute);

      memset(gemfil,'\0',256);
      cfl_mnam(dattim,ofil,gemfil,&ier);
      if(ier != 0)
         {
         uerror("cfl_mnam[%d] %s\0",ier,dattim);
         *iret = -1;
         return;
         }

      dc_fcyl(gemfil,&iflsrc,statfl,&maxstat,&maxtim,&iflno,&i,prmlist,&ier,
           strlen(gemfil),strlen(statfl),sizeof(prmlist[0]));

      if(ier != 0)
         {
         uerror("Could not open gempak file %s\0",gemfil);
         *iret = -1;
         return;
         }

      if(i > MAXPRM)
         uerror("Returned more parameters than allowed: %d\0",MAXPRM);
      if(i > 0) nparms = i;
      for(i=0;i<NUMVARS;i++) pos[i] = -1;
      for(i=0;i<nparms;i++)
         {
         if(strncmp(prmlist[i],"PRES",4) == 0)
            pos[PRES] = i;
         if(strncmp(prmlist[i],"TMPC",4) == 0)
            pos[TMPC] = i;
         if(strncmp(prmlist[i],"RELH",4) == 0)
            pos[RELH] = i;
         if(strncmp(prmlist[i],"PWVM",4) == 0)
            pos[PWVM] = i;
         if(strncmp(prmlist[i],"PWVE",4) == 0)
            pos[PWVE] = i;
         if(strncmp(prmlist[i],"DELW",4) == 0)
            pos[DELW] = i;
         if(strncmp(prmlist[i],"DELD",4) == 0)
            pos[DELD] = i;
         if(strncmp(prmlist[i],"DELT",4) == 0)
            pos[DELT] = i;
         if(strncmp(prmlist[i],"DELF",4) == 0)
            pos[DELF] = i;
         if(strncmp(prmlist[i],"PIFC",4) == 0)
            pos[PIFC] = i;
         if(strncmp(prmlist[i],"MFLG",4) == 0)
            pos[MFLG] = i;
         }

      proces = 1;
      sf_ftim ( &iflno, dattim, &ier, strlen(dattim) );
      if(ier != 0)
         {
         sf_atim( &iflno, dattim, &ier, strlen(dattim) );
         if(ier == 0)
            sf_ftim ( &iflno, dattim, &ier, strlen(dattim) );
         else
            proces = 0;
         }

     
      memset(stid[0],' ',sizeof(stid[0]));
      memcpy(stid[0],p->stid,strlen(p->stid));
      sf_fstn ( &iflno, stid[0], &ier, sizeof(stid[0]));

      if(ier != 0)
         {
         i = 1; 
         stnm[0] = -99999;
         slat[0] = p->Lat;
         slon[0] = p->Lon;
         selv[0] = p->Alt;
         memcpy(state[0],"--",2);
         memcpy(coun[0],"--",2);
         ispri = 0;

         sf_astn(&iflno,&i,stid,stnm,slat,slon,selv,state,coun,
            &ispri, &nadd, &ier, sizeof(stid[0]), sizeof(state[0]), sizeof(coun[0]));
         if(ier == 0)
            sf_fstn ( &iflno, stid[0], &ier, sizeof(stid[0]) );
         else
            proces = 0;
         }

      if(proces == 1)
         {
         for(i=0;i<nparms;i++)
            dataray[i] = RMISSD;
         if(pos[PRES] >= 0)
	    dataray[pos[PRES]] = s->pres;
         if(pos[TMPC] >= 0)
	    dataray[pos[TMPC]] = s->tmpc;
         if(pos[RELH] >= 0)
	    dataray[pos[RELH]] = s->relh;
         if(pos[PWVM] >= 0)
	    dataray[pos[PWVM]] = s->pwv;
         if(pos[PWVE] >= 0)
	    dataray[pos[PWVE]] = s->pwv_err;
         if(pos[DELW] >= 0)
	    dataray[pos[DELW]] = s->wet_delay;
         if(pos[DELD] >= 0)
	    dataray[pos[DELD]] = s->model_dry_delay;
         if(pos[DELT] >= 0)
	    dataray[pos[DELT]] = s->total_delay;
         if(pos[DELF] >= 0)
	    dataray[pos[DELF]] = s->final_dry_delay;
         if(pos[PIFC] >= 0)
	    dataray[pos[PIFC]] = s->pifact;
         if(pos[MFLG] >= 0)
	    dataray[pos[MFLG]] = s->met_flag;

         ihhmm = s->hour*100 + s->minute;
         sf_wdat(&iflno, &ihhmm, dataray, &ier);
	 uinfo("wrote %s %s %d\n",p->stid,dattim,ier);
         }

      s = s->next;
      }
   p = p->next;
   }

if(iflno != -1) sf_clos(&iflno,&ier);
*iret = 0;
}

