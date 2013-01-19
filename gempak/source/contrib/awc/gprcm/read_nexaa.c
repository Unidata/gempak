/********************************************************/
/* READ_NEXAA.C						*/
/* Chiz/Unidata 10/99                                   */
/*                                                      */
/* Routines for plotting NEXAA portion of RCM bulletin.	*/
/*							*/
/* void read_nexaa(bultin,lenbul,clat,clon,time_str,	*/
/*		   trange,obs_time,mode,valid)		*/
/* char *bultin		RCM bulletin			*/
/* int	lenbul		length of RCM bulletin		*/
/* float clat,clon	lat/lon of radar site		*/
/* time_t start_time	start time for data		*/
/* time_t end_time	end time for data		*/
/* int *mode		radar operating mode		*/
/* int *valid		flag for valid observation	*/
/* time_t *obs_time;	radar time (plotted)		*/
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

static int mdrplot=0;

void set_mdrplot(int *i)
{
mdrplot = *i;
}

/*
 * Internal routine
 */
void cenaa(cpos,nmes,RADARS,ndex)
char *cpos;
int nmes;
rad_struct RADARS[];
int ndex;
{
int i;
char *spos;
int sped,drct;

if(nmes > 0)
   {
   i = 0; spos = cpos;
   while(i < nmes)
      {
      while((spos[0] != 0)&&((spos[0] == ' ')||(spos[0] == ','))) spos++;
      if(spos[0] != 'C')
         {
         printf("incorrect identifier\n");
         return;
         }
      else
         {
         sscanf(spos+7,"%3d%3d",&drct,&sped); 
         RADARS[ndex].cntr[i].spd =  (float)sped;
         RADARS[ndex].cntr[i].dir = (float)drct;
         RADARS[ndex].cntr[i].ggg[0] = '\0';
         strncat(RADARS[ndex].cntr[i].ggg,spos+3,3);
         spos+=13;
         i++;
         }
      }
   RADARS[ndex].ncntr = nmes;
   }
}

/*
 * Internal routine
 */
void read_mdr(char *pos, float clat, float clon, int mode)
{
int ilen,i,nextoff,iret;
int num_inten;
float x,y;
char *spos,buf[512];

spos = pos;

ilen = getstr(spos,buf); 

if(strncmp(buf,"/NI",3) == 0)
   sscanf(buf+3,"%d",&num_inten);
else
   num_inten = -1;

if(num_inten > 0)
   {
   get_xy(clat,clon,&x,&y);
   spos = spos + ilen + 1;
   i = 0;
   while(i < num_inten)
      {
      while((spos[0] != 0)&&(spos[0] == ' ')) spos++; 
      if(spos[0] == '/')
         {
         printf("read error encountered / %d\n",i);
         return;
         }
      i += get_nextggg(spos,buf,num_inten - i,&nextoff,&iret);
      if(iret != 0)
         {
         printf("read error (expected %d, found %d)\n",num_inten,i);
         return;
         }
      else
         {
         spos += nextoff;
         grid_ggg(buf,x,y,mode);
         }
      }
   }
}



void read_nexaa(char *bultin, int lenbul, rad_struct RADARS[], int NEXINDX, 
                time_t start_time, time_t end_time, int *mode, int *valid)
{
char *cpos,*spos;
char buf[512];
int state;
int ilen;
int lsiz;
int i,iret,nextoff;
int num_inten;
int day,month,year,hour,minute;
int NCEN;
char timeformat[80];

struct tm new_time;
time_t radar_time;

cpos = NULL; i = 0;

*mode = MDNA; *valid = 0;

while((i < lenbul-7)&&(cpos == NULL))
   {
   if(memcmp(bultin+i,"/NEXRAA",7) == 0)
      cpos = bultin+i;
   else
      i++;
   }
if(cpos == NULL) return;

cpos = cpos + getstr(cpos,buf) + 1; /* NEXRAA */
cpos = cpos + getstr(cpos,buf) + 1; /* St ID */
cpos = cpos + getstr(cpos,buf) + 1; /* ddmmyyHHNN */
ilen = sscanf(buf,"%2d%2d%2d%2d%2d",&day,&month,&year,&hour,&minute);
if(ilen != 5)
   {
   printf("could not get time from %s\n",buf);
   *valid = -1;
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

if((radar_time < start_time)||(radar_time > end_time)) 
   {
#ifdef DEBUG
   strftime(timeformat,80,"%Y %m %d: %H%M",&new_time);
   printf("%s obs time %s not in valid range\n",RADARS[NEXINDX].idlst,timeformat); 
   printf("look here %ld %ld %ld\n",radar_time,start_time,end_time);
#endif
   *valid = -1;
   return;
   }

if(radar_time <= RADARS[NEXINDX].obs_time)
   {
#ifdef DEBUG
   printf("Already plotted %s %d <= %d\n",RADARS[NEXINDX].idlst,radar_time,RADARS[NEXINDX].obs_time);
#endif
   *mode = -9;
   return;
   }
else
   RADARS[NEXINDX].obs_time = radar_time;

ilen = getstr(cpos,buf); /* Edited/unedited */
ilen += findchr(cpos+ilen);
ilen = getstr(cpos+ilen,buf); /* RADNE, RADOM if present */
if((buf[0] != '\0')&&(buf[0] != '/'))
   {
   if(strncmp(buf,"RADNE",5)==0) 
      {
      *mode = MDNE;
      return;
      }
   if(strncmp(buf,"RADOM",5)==0) 
      {
      *mode = MDOM; 
      return;
      }
   }

while((spos = (char *)next_group(cpos,lenbul - (cpos - bultin))) != NULL)
   {
   if(strncmp(spos,"/ENDAA",6) == 0) return;
   if(strncmp(spos,"/NCEN",5) == 0)
      {
      sscanf(spos+5,"%d:",&NCEN); 
      if((NCEN > 0)&&(*mode > 1)) 
         cenaa(spos+9,NCEN,RADARS,NEXINDX);
      }
   if((strncmp(spos,"/MT",3) == 0)&&(*mode > 1))
      {
      if(spos[6] != ':') 
         {
         printf("%s bad MT format\n",RADARS[NEXINDX].idlst);
         }
      else
         {
         sscanf(spos+3,"%d",&RADARS[NEXINDX].maxtop);
         strncpy(RADARS[NEXINDX].maxtop_ggg,spos+7,3);
         }
      }
   if(strncmp(spos,"/MDCLAR",7) == 0) *mode = MDCLAR; 
   if(strncmp(spos,"/MDPCPN",7) == 0) *mode = MDPCPN; 

   if((strncmp(spos,"/NI",3) == 0)&&((2 - *mode) < mdrplot))
      read_mdr(spos,RADARS[NEXINDX].stnlat,RADARS[NEXINDX].stnlon,*mode);

   cpos = spos + 1;
   }

return;


}

