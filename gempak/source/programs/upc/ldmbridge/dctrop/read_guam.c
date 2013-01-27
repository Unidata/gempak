#include <stdio.h>
#include <string.h>

#ifdef UNDERSCORE
#define read_guam       read_guam_
#endif

#define MAXVARS	3

#define FCHR    0
#define SKNT    1
#define SCAT    2



int guam_position(pos,date,lat,lon)
char *pos;
int *date;
float *lat,*lon;
{
int space;
char *cpos,*spos;
char tempstr[20];

      sscanf(pos,"%d",date);
      cpos= (char *)strstr(pos,"---");
      if(cpos == NULL) return(-1);
      spos = cpos + 4;

      /* a line can have "NEAR, like 051200Z8 --- NEAR 9.3N2 148.4E7" */
      /* skip until we hit a digit */
      while ((spos[0] != '\0')&&
        ((spos[0] < '0')||(spos[0] > '9')))
	   spos++;
      if(spos[0] == '\0') return(-1);
 
      cpos = spos;

      space = 0;
      while((space < strlen(cpos))&&
         (((cpos[space] >= '0')&&(cpos[space] <= '9'))||(cpos[space] == '.')))
         space++;
      tempstr[0] = '\0';
      strncat(tempstr,cpos,space);
      sscanf(tempstr,"%f",lat);
      if(cpos[space] == 'S') *lat = *lat * -1;
      cpos = cpos + space;
      space = 0;
      while((space < strlen(cpos))&&(cpos[space] != ' ')) space++;
      while((space < strlen(cpos))&&(cpos[space] == ' ')) space++;
      cpos = cpos + space;
      space = 0;
      while((space < strlen(cpos))&&
         (((cpos[space] >= '0')&&(cpos[space] <= '9'))||(cpos[space] == '.')))
         space++;
      tempstr[0] = '\0';
      strncat(tempstr,cpos,space);
      sscanf(tempstr,"%f",lon);
      if(cpos[space] == 'W') *lon = *lon * -1;

      return(0);

      
}


void read_guam(char *bultin, int *lenbul, int *start, int *numhrs,
	float slat[], float slon[], float rdata[][MAXVARS],
	int iotarr[], int *rtype, char *name, int *name_len, int *dis_num)
/*char *bultin;
int *lenbul, *start;
float slat[],slon[],rdata[][MAXVARS];
int iotarr[5];
int *rtype;
char *name;
int *name_len,*dis_num;*/
{

/* A WTPN3[1-5] Bulletin 
WTPN31 PGTW 280900
1. TROPICAL DEPRESSION 02W WARNING NR 01
   01 ACTIVE TROPICAL CYCLONE IN NORTHWESTPAC
   MAX SUSTAINED WINDS BASED ON ONE-MINUTE AVERAGE
    ---
   WARNING POSITION:
   280600Z6 --- 06.0N6 166.0E3
     MOVEMENT PAST SIX HOURS - 025 DEGREES AT 02 KTS
     POSITION ACCURATE TO WITHIN 060 NM
     POSITION BASED ON CENTER LOCATED BY A COMBINATION OF
     SATELLITE AND SYNOPTIC DATA
   PRESENT WIND DISTRIBUTION:
   MAX SUSTAINED WINDS - 030 KT, GUSTS 040 KT
   REPEAT POSIT: 06.0N6 166.0E3

WTPN32 PGTW 110300^M^M
1. TROPICAL DEPRESSION 24W WARNING NR 008^M^M
   01 ACTIVE TROPICAL CYCLONE IN NORTHWESTPAC^M^M
   MAX SUSTAINED WINDS BASED ON ONE-MINUTE AVERAGE^M^M
    ---^M^M
   WARNING POSITION:^M^M
   110000Z2 --- 17.4N2 117.0E9^M^M
     MOVEMENT PAST SIX HOURS - 270 DEGREES AT 07 KTS^M^M
     POSITION ACCURATE TO WITHIN 060 NM^M^M
     POSITION BASED ON CENTER LOCATED BY A COMBINATION OF^M^M
     SATELLITE AND SYNOPTIC DATA^M^M
   PRESENT WIND DISTRIBUTION:^M^M
   MAX SUSTAINED WINDS - 025 KT, GUSTS 035 KT^M^M
   REPEAT POSIT: 17.4N2 117.0E9^M^M
    ---^M^M
   FORECASTS:^M^M
   12 HRS, VALID AT:^M^M
   111200Z5 --- 17.2N0 116.2E0^M^M
   MAX SUSTAINED WINDS - 025 KT, GUSTS 035 KT^M^M
   VECTOR TO 24 HR POSIT: 265 DEG/ 05 KTS^M^M
    ---^M^M
   24 HRS, VALID AT:^M^M
   120000Z3 --- 17.1N9 115.1E8^M^M
   MAX SUSTAINED WINDS - 030 KT, GUSTS 040 KT^M^M
   VECTOR TO 36 HR POSIT: 270 DEG/ 07 KTS^M^M
    ---^M^M
   36 HRS, VALID AT:^M^M
   121200Z6 --- 17.1N9 113.7E2^M^M
   MAX SUSTAINED WINDS - 040 KT, GUSTS 050 KT^M^M
   RADIUS OF 035 KT WINDS - 090 NM^M^M
   VECTOR TO 48 HR POSIT: 300 DEG/ 07 KTS^M^M
    ---^M^M
   EXTENDED OUTLOOK:^M^M
   48 HRS, VALID AT:^M^M
   130000Z4 --- 17.8N6 112.4E8^M^M
   MAX SUSTAINED WINDS - 050 KT, GUSTS 065 KT^M^M
   RADIUS OF 050 KT WINDS - 030 NM^M^M
   RADIUS OF 035 KT WINDS - 110 NM^M^M
   VECTOR TO 72 HR POSIT: 320 DEG/ 07 KTS^M^M
    ---^M^M
   72 HRS, VALID AT:^M^M
   140000Z5 --- 19.9N9 110.4E6^M^M
   MAX SUSTAINED WINDS - 065 KT, GUSTS 080 KT^M^M
   RADIUS OF 050 KT WINDS - 040 NM OVER WATER^M^M
   RADIUS OF 035 KT WINDS - 140 NM NORTHEAST SEMICIRCLE^M^M
                                   OVER WATER^M^M
                            110 NM ELSEWHERE OVER WATER^M^M
    ---^M^M

*/

char *pos,*cpos, *hold;
float lat,lon;
int space;
char valid[40];
int sped, scat;
int date,year,month,day,hour;
int rdata_count=0;
int count, test;

bultin[*lenbul] = '\0';
name[0] = '\0';
*dis_num = 0;
*numhrs = 0;

pos = (char *)strtok(bultin,"\n");

while((pos != NULL) && (strstr(pos,"WTPN3")==NULL))
         pos = (char *)strtok(NULL,"\n");

if(pos == NULL) 
   {
   *rtype = -1;
   return;
   }

/* look for a line starting with "1. " */
while ((pos != NULL) && (strncmp(pos,"1. ",3)!=0))
   pos = (char *)strtok(NULL,"\n");
if(pos == NULL)
   {
   *rtype = -1;
   return;
   }

hold = pos + strlen(pos) + 1;

cpos = (char *)strstr(pos,"TROPICAL");
if(cpos != NULL)
   {
   pos = (char *)strstr(cpos," NR ");
   if(pos != NULL) sscanf(pos+3,"%d",dis_num);
   pos = (char *)strtok(cpos," ");
   pos = (char *)strtok(NULL," ");
   pos = (char *)strtok(NULL," ");
   sprintf(name,"%s\0",pos);
   *name_len = strlen(name);
   }
else
   {
   cpos = (char *)strstr(pos,"TYPHOON");
   if(cpos !=NULL)
      {
      pos = (char *)strstr(cpos," NR ");
      if(pos != NULL) sscanf(pos+3,"%d",dis_num);
      pos = (char *)strtok(cpos," ");
      pos = (char *)strtok(NULL," ");
      sprintf(name,"%s\0",pos);
      *name_len = strlen(name);
      }
   else
      {
      *rtype = -1;
      return;
      }
   }


pos = (char *)strtok(hold,"\n");
while((pos != NULL) && (strstr(pos,"WARNING POSITION:") == NULL))
   pos = strtok(NULL,"\n");

if(pos == NULL) 
   {
   *rtype = -1;
   return;
   }

if(strstr(pos,"WARNING POSITION:") != NULL)
   {
   pos = strtok(NULL,"\n");

   test = guam_position(pos,&date,&lat,&lon);
   slat[0] = lat;
   slon[0] = lon;
   day = date / 10000;
   hour = (date % 10000) / 100;
   iotarr[2] = day;
   iotarr[3] = hour;
   }




while(( pos != NULL) && 
   (strstr(pos,"FORECASTS:") == NULL) &&
   (strstr(pos,"MAX SUSTAINED WINDS - ") == NULL))
   {
   pos = (char *)strtok(NULL,"\n");
   } 

sped = -9999;
hour = 0;

if((pos != NULL) && (strstr(pos,"MAX SUSTAINED WINDS - ") != NULL))
   {
   cpos = strstr(pos,"MAX SUSTAINED WINDS - ");
   sscanf(cpos+21,"%d",&sped);
   scat = -9999;
   if(sped <= 33) scat = 7;
   if(sped > 33) scat = 6;
   if(sped > 64) scat = 1;
   if(sped > 83) scat = 2;
   if(sped > 96) scat = 3;
   if(sped > 113) scat = 4;
   if(sped > 134) scat = 5;

   /*rdata_count = 0;*/
   rdata[0][FCHR] = hour;
   rdata[0][SCAT] = scat;
   rdata[0][SKNT] = sped;
   }

rdata_count++;

  
while( pos != NULL)
   {
   pos = (char *)strtok(NULL,"\n");
   while((pos != NULL) && (strstr(pos,"HRS, VALID AT:") == NULL))
      pos = strtok(NULL,"\n");
   
   if(pos != NULL) 
      {
      sscanf(pos,"%d",&hour);

      pos = (char *)strtok(NULL,"\n");
      if(pos != NULL)
         {
         test = guam_position(pos,&date,&lat,&lon);
         if(test < 0) 
            continue;

         slat[rdata_count] = lat;
         slon[rdata_count] = lon;
         
         pos = (char *)strtok(NULL,"\n");
         if(pos != NULL)
            {
            sped = -9999;
            cpos = strstr(pos,"MAX SUSTAINED WINDS - ");
            if(cpos != NULL) sscanf(cpos+21,"%d",&sped);
            scat = -9999;
            if(sped <= 33) scat = 7;
            if(sped > 33) scat = 6;
            if(sped > 64) scat = 1;
            if(sped > 83) scat = 2;
            if(sped > 96) scat = 3;
            if(sped > 113) scat = 4;
            if(sped > 134) scat = 5;
            rdata[rdata_count][FCHR] = hour;
            rdata[rdata_count][SCAT] = scat;
            rdata[rdata_count][SKNT] = sped;
            }
         else
            *rtype = -100;
            /*uerror("invalid forecast intensity format\n");*/
         rdata_count++;
         }
      else
         *rtype = -101;
         /*uerror("invalid forecast hour format\n");*/
         
      }

   }

*numhrs = rdata_count;
}

