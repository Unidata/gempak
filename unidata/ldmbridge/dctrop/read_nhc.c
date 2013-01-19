#include <stdio.h>
#include <string.h>

#ifdef UNDERSCORE
#define read_nhc	read_nhc_
#endif

#define MAXVARS	3

#define FCHR	0
#define SKNT	1
#define SCAT	2

void read_nhc(char *bultin, int *lenbul, int *start, int *numhrs,
        float slat[], float slon[], float rdata[][MAXVARS],
        int aotarr[], int *rtype, char *name, int *name_len, int *dis_num)
/*
void read_nhc(bultin,lenbul,start,slat,slon,rdata,aotarr,rtype,name,name_len,dis_num)
char *bultin;
int *lenbul, *start;
float slat[],slon[],rdata[][MAXVARS];
int aotarr[5];
int *rtype;
char *name;
int *name_len,*dis_num;*/
{
char *pos,*cpos, *hold;
float lat,lon;
int space;
char title[80], valid[40];
int skt;
int year,month,day,hour;
int rdata_count=0;


bultin[*lenbul] = '\0';
name[0] = 'A'; name[1] = '\0';
title[0] = '\0';

*numhrs = 0;

/* FOR INTERGOVERNMENTAL USE ONLY removed in 1997 */
pos = (char *)strtok(bultin,"\n");
while((pos != NULL)&&(strstr(pos,"DISCUSSION NUMBER") == NULL))
   pos = (char *)strtok(NULL,"\n");

if(pos == NULL) 
   {
   *rtype = -1;
   return;
   }

cpos = (char *)strstr(pos,"DISCUSSION NUMBER");
sscanf(cpos+17,"%d",dis_num);

hold = pos + strlen(pos) + 1;
if((strncmp(pos,"TROPICAL",8)==0)||
   (strncmp(pos,"SUBTROPICAL",11)==0)||
   (strncmp(pos,"EXTRATROPICAL",13)==0))
   {
   cpos = (char *)strtok(pos," ");
   strcat(title,cpos);
   cpos = (char *)strtok(NULL," ");
   strcat(title," ");
   strcat(title,cpos);
   cpos = (char *)strtok(NULL," ");
   strcat(title," ");
   strcat(title,cpos);
   sprintf(name,"%s\0",cpos);
   *name_len = strlen(name);
   }
else
   if(strncmp(pos,"HURRICANE",9)==0)
      {
      cpos = (char *)strtok(pos," ");
      strcat(title,cpos);
      cpos = (char *)strtok(NULL," ");
      strcat(title," ");
      strcat(title,cpos);
      sprintf(name,"%s\0",cpos);
      *name_len = strlen(name);
      }

/*printf("name %s title %s\n",name,title);*/

pos = (char *)strtok(hold,"\n");

while(pos != NULL)
   {
   if((strncmp(pos,"INITIAL    ",9)==0)||
      (strncmp(pos,"12HR VT    ",9) == 0) ||
      (strncmp(pos,"24HR VT    ",9) == 0) ||
      (strncmp(pos,"36HR VT    ",9) == 0) ||
      (strncmp(pos,"48HR VT    ",9) == 0) ||
      (strncmp(pos,"72HR VT    ",9) == 0))
      {
      space = 9;
      while((pos[space]!='\0')&&(pos[space] == ' '))
         space += 1;

      valid[0] = '\0';
      strncat(valid,pos+space,5);

      while((pos[space]!='\0')&&(pos[space] > ' '))
           space = space + 1;

      sscanf(pos+space,"%f %*c %f %*c %d",&lat,&lon,&skt);

      if(strncmp(pos,"INITIAL    ",9)==0)
         {
         /*rdata_count=0;*/
         rdata[rdata_count][FCHR] = 00;

         aotarr[4] = 0;
         sscanf(valid+3,"%d",&hour);
         aotarr[3] = hour;
         sscanf(valid,"%d",&day);
         aotarr[2] = day;
         hour = hour * 100;
         }
      else
         {
         /*rdata_count = rdata_count + 1;*/
         sscanf(pos,"%d",&hour);
         rdata[rdata_count][FCHR] = hour;
         }

      rdata[rdata_count][SKNT] = skt;

      lon = -lon ;
      slat[rdata_count] = lat;
      slon[rdata_count] = lon;

      if(skt <= 33) rdata[rdata_count][SCAT] = 7;
      if(skt > 33) rdata[rdata_count][SCAT] = 6;
      if(skt > 64) rdata[rdata_count][SCAT] = 1;
      if(skt > 83) rdata[rdata_count][SCAT] = 2;
      if(skt > 96) rdata[rdata_count][SCAT] = 3;
      if(skt > 113) rdata[rdata_count][SCAT] = 4;
      if(skt > 134) rdata[rdata_count][SCAT] = 5;

      rdata_count++;


      }

/* 12-72 *
*/

   pos = (char *)strtok(NULL,"\n");
   }

*numhrs = rdata_count;
}

