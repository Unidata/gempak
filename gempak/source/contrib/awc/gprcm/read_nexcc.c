/********************************************************/
/* READ_NEXCC.C                                         */
/* Chiz/Unidata 10/99                                   */
/*                                                      */
/* Routines for plotting NEXCC portion of RCM bulletin. */
/*                                                      */
/* void read_nexcc(bultin,lenbul,RADARS,NEXINDX,	*/
/*			time_str,trange)		*/
/* char *bultin         RCM bulletin                    */
/* int  lenbul          length of RCM bulletin          */
/* rad_struct RADARS[]  RADAR site structure		*/
/* int NEXINDX		nexrad index to struct		*/
/* time_t start_time	start time for data		*/
/* time_t end_time	end time for data		*/
/********************************************************/
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <time.h>
#include "rcm.h"


/*
 * Internal routine
 */
void read_nmes(char *cpos, int nmes, rad_struct RADARS[], int ndex)
{
int i;
char *spos;

if(nmes > 0)
   {
   i = 0; spos = cpos;
   while(i < nmes)
      {
      while((spos[0] != 0)&&((spos[0] == ' ')||(spos[0] == ','))) spos++;
      if(spos[0] != 'M')
         {
         printf("incorrect MESO identifier %s\n",RADARS[ndex].idlst);
         return;
         }
      else
         {
         RADARS[ndex].meso[i].ggg[0] = '\0'; strncat(RADARS[ndex].meso[i].ggg,spos+3,3);
         spos+=6;
         i++;
         }
      }
   RADARS[ndex].nmeso = nmes;
   }
}

/*
 * Internal routine
 */
void read_tvs(char *cpos, int nmes, rad_struct RADARS[], int ndex)
{
int i;
char *spos;

if(nmes > 0)
   {
   i = 0; spos = cpos;
   while(i < nmes)
      {
      while((spos[0] != 0)&&((spos[0] == ' ')||(spos[0] == ','))) spos++;
      if(strncmp(spos,"TVS",3) != 0)
         {
         printf("incorrect TVS identifier %s\n",RADARS[ndex].idlst);
         return;
         }
      else
         {
         RADARS[ndex].tvs[i].ggg[0] = '\0'; strncat(RADARS[ndex].tvs[i].ggg,spos+5,3);
         spos+=8;
         i++;
         }
      }
   RADARS[ndex].ntvs = nmes;
   }
}


void    read_nexcc(char *bultin, int lenbul, rad_struct RADARS[], int NEXINDX, time_t start_time, time_t end_time)
{
char *cpos,*spos;
char buf[512];
int state;
int ilen;
int lsiz;
int i,iret,nextoff;
int num_inten;
int day,month,year,hour,minute;
int NMES,NTVS,NCEN;
int NF;

struct tm new_time;
time_t radar_time;


cpos = NULL; i = 0;

while((i < lenbul-7)&&(cpos == NULL))
   {
   if(memcmp(bultin+i,"/NEXRCC",7) == 0)
      cpos = bultin+i;
   else
      i++;
   }
if(cpos == NULL) return;

cpos = cpos + getstr(cpos,buf) + 1; /* NEXRCC */
cpos = cpos + getstr(cpos,buf) + 1; /* St ID */
cpos = cpos + getstr(cpos,buf) + 1; /* ddmmyyHHNN */
ilen = sscanf(buf,"%2d%2d%2d%2d%2d",&day,&month,&year,&hour,&minute);
if(ilen != 5)
   {
   printf("could not get time from %s\n",buf);
   return;
   }
/* see if time is in range */
new_time.tm_year = year;
new_time.tm_mon = month - 1; /* month offset from 0 */
new_time.tm_mday = day;
new_time.tm_hour = hour;
new_time.tm_min = minute;
new_time.tm_sec = 0;
if(new_time.tm_year < 70) new_time.tm_year += 100;
new_time.tm_isdst = -1;
radar_time = mktime(&new_time);
if((radar_time < start_time)||(radar_time > end_time)) return;

while((spos = (char *)next_group(cpos,lenbul - (cpos - bultin))) != NULL)
   {
   NF = 0;
   if(strncmp(spos,"/ENDCC",6) == 0) return;
   if(strncmp(spos,"/NTVS",5) == 0)
      {
      sscanf(spos+5,"%d:",&NTVS); NF++;
      if((NTVS > 0)&&(RADARS[NEXINDX].mode > 1)) 
         read_tvs(spos+9,NTVS,RADARS,NEXINDX);
      }
   if(strncmp(spos,"/NMES",5) == 0)
      {
      sscanf(spos+5,"%d:",&NMES); NF++;
      if((NMES > 0)&&(RADARS[NEXINDX].mode > 1)) 
         read_nmes(spos+9,NMES,RADARS,NEXINDX);
      }
   if(strncmp(spos,"/NCEN",5) == 0)
      {
      sscanf(spos+5,"%d:",&NCEN); NF++;
      /*if(NCEN > 0 ) printf("found NCEN %d\n",NCEN);*/
      }
   if(strncmp(spos,"/PCTR",5) == 0)
      {
      printf("found PCTR %s\n",RADARS[NEXINDX].idlst); NF++;
      }
   if(strncmp(spos,"/LEWP",5) == 0)
      {
      printf("found LEWP %s\n",RADARS[NEXINDX].idlst); NF++;
      }
   if(strncmp(spos,"/BASE",5) == 0)
      {
      printf("found BASE %s\n",RADARS[NEXINDX].idlst); NF++;
      }
   if(strncmp(spos,"/MALF",5) == 0)
      {
      printf("found MALF %s\n",RADARS[NEXINDX].idlst); NF++;
      }
   if(strncmp(spos,"/MLTLVL",7) == 0)
      {
      printf("found MLTLVL %s\n",RADARS[NEXINDX].idlst); NF++;
      }
   if(strncmp(spos,"/EYE",5) == 0)
      {
      printf("found EYE %s\n",RADARS[NEXINDX].idlst); NF++;
      }


   if(NF == 0)
      {
      printf("NEXCC group ");
      while((spos-bultin < lenbul)&&(spos[0] > ' '))
         {
         printf("%c",spos[0]); 
         spos++;
         }
      printf("\n");
      }
   cpos = spos+1;
   }

}

