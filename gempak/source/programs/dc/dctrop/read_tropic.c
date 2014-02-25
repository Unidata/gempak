#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>
#include "prod.h"

#ifdef UNDERSCORE
#define open_gem_ship open_gem_ship_
#define sf_wsdd sf_wsdd_
#define sf_clos sf_clos_
#endif

#define MAXVARS	3

/* These are w. pacific and indian ocean classifications */
#define CYCLONE 0
#define DISTURBANCE 1
#define DEPRESSION 2
#define STORM 3
#define TYPHOON 4

int iflno=0;
int FCHR, SKNT, SCAT;

int guam_position(pos,date,lat,lon)
char *pos;
int *date;
float *lat,*lon;
{
int space;
char *cpos;
char tempstr[20];

      sscanf(pos,"%d",date);
      cpos= (char *)strstr(pos,"---");
      cpos = cpos + 4;
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

      
}


void read_guam(the_prod,gemfile,packing,maxtim,yymmdd)
prod *the_prod;
char *gemfile,*packing;
int maxtim,yymmdd;
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
*/

int space;
char *pos,*cpos,*hold;
char name[30],tempstr[20];
char title[30];
int date;
float lat,lon;
int skt;
int dis_num;
char stormfile[256];
int hour;
int test,ier;
int sped,scat;
float rdata[MAXVARS];
char gtime[20];
int isnum;
float selv;
char state[3],country[3];
char stid[9];

isnum = -9999;
selv = 0;
sprintf(state,"--\0");
sprintf(country,"--\0");

title[0] = '\0';
name[0] = '\0';
gtime[0] = '\0';

pos = (char *)strtok((char *)the_prod->bytes,"\n");
while((pos != NULL) && (strstr(pos,"WTPN3")==NULL))
         pos = (char *)strtok(NULL,"\n");

if(pos == NULL) exit(1);

      pos = (char *)strtok(NULL,"\n");
      hold = pos + strlen(pos) + 1;

      cpos = (char *)strstr(pos,"TROPICAL CYCLONE");
      if(cpos != NULL)
         {
         pos = (char *)strtok(cpos," ");
         pos = (char *)strtok(NULL," ");
         pos = (char *)strtok(NULL," ");
         sprintf(name,"%s\0",pos);
         skt = CYCLONE;
         pos = (char *)strtok(NULL,")");
         if(pos != NULL)
            {
            space = 0;
            while((space < strlen(pos)) && (pos[space] != '(')) space = space + 1;
            sprintf(title,"%s\0",pos+space+1);
            }
         }
      else
         {
         cpos = (char *)strstr(pos,"TROPICAL DEPRESSION");
         if(cpos !=NULL)
            { 
            pos = (char *)strtok(cpos," ");
            pos = (char *)strtok(NULL," ");
            pos = (char *)strtok(NULL," ");
            name[0] = '\0';
            sprintf(name,"%s\0",pos);
            skt = DEPRESSION;
            }
         else
            {
            cpos = (char *)strstr(pos,"TROPICAL STORM");
            if(cpos !=NULL)
               {
               pos = (char *)strtok(cpos," ");
               pos = (char *)strtok(NULL," ");
               pos = (char *)strtok(NULL," ");
               name[0] = '\0';
               sprintf(name,"%s\0",pos);
               skt = STORM;
               }
            else
               {
               cpos = (char *)strstr(pos,"TYPHOON");
               if(cpos !=NULL)
                  {
                  pos = (char *)strtok(cpos," ");
                  pos = (char *)strtok(NULL," ");
                  sprintf(name,"%s\0",pos);
                  skt = TYPHOON;
                  pos = (char *)strtok(NULL,")");
                  if(pos != NULL)
                     {
                     space = 0;
                     while((space < strlen(pos)) && (pos[space] != '(')) space = space + 1;
                     sprintf(title,"%s\0",pos+space+1);
                     }
                  }
               else
                  {
                  cpos = (char *)strstr(pos,"TROPICAL DISTURBANCE");
                  if(cpos !=NULL)
                     {
                     pos = (char *)strtok(cpos," ");
                     pos = (char *)strtok(NULL," ");
                     pos = (char *)strtok(NULL," ");
                     name[0] = '\0';
                     sprintf(name,"%s\0",pos);
                           for(skt=0;skt < strlen(name);skt++)
                              if(name[skt] < 32) name[skt] = '\0';
                     skt = DISTURBANCE;
                     }
                  else
                     {
                     uerror("Unknown Format %s\n",pos);
                     exit(-1);
                     }
                  }
               }
            }
         }


   pos = (char *)strtok(hold,"\n");
   while((pos != NULL) && (strstr(pos,"WARNING POSITION:") == NULL))
      pos = strtok(NULL,"\n");

   if(pos == NULL) 
      {
      uerror(" FOund no WARNING POSITION:\n");
      exit(0);
      }

   if(strstr(pos,"WARNING POSITION:") != NULL)
      {
      pos = strtok(NULL,"\n");

      test = guam_position(pos,&date,&lat,&lon);

      stid[0] = '\0';
      if(skt == DISTURBANCE)
         {
         sprintf(stormfile,"%s/S%s.gem\0",gemfile,name);
         strncat(stid,"S",1);
         strncat(stid,name,7);
         }
      else
         {
         sprintf(stormfile,"%s/%s.gem\0",gemfile,name);
         strncat(stid,name,8);
         }
      while(strlen(stid) < 8) strncat(stid," ",1);
         

      if ((iflno = open_gem_ship(stormfile, packing, &maxtim, 
            &FCHR, &SKNT, &SCAT,
            strlen(stormfile), strlen(packing))) == -1) {
          serror("can't open %s", stormfile);
          exit(1);
          }

      /*printf("stormfile %s\n",stormfile);*/

      /*
      switch(skt)
         {
         case CYCLONE:
            printf("C000 %6.2f %7.2f  col:lcyan:wx:@\n",lat,lon);
            break;
         case TYPHOON:
            printf("C000 %6.2f %7.2f  col:lcyan:wx:@+\n",lat,lon);
            break;
         case STORM:
            printf("C000 %6.2f %7.2f  col:lcyan:wx:@\n",lat,lon);
            break;
         case DEPRESSION:
            printf("C000 %6.2f %7.2f  col:lcyan:wx:+\n",lat,lon);
            break;
         case DISTURBANCE:
            printf("C000 %6.2f %7.2f  col:lcyan:tx:*\n",lat,lon);
            break;
         default:
            uerror("Unknown classification\n");
         }
      */
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
      if(sped > 64) scat = 1;
      if(sped > 83) scat = 2;
      if(sped > 96) scat = 3;
      if(sped > 113) scat = 4;
      if(sped > 134) scat = 5;
      if((sped > 33)&&(sped <= 64)) scat = 6;
      if(sped <= 33) scat = 7;
      sprintf(gtime,"%04d%02d/%04d\0",yymmdd/100,date/10000,date%10000);
      rdata[FCHR] = hour;
      rdata[SCAT] = scat;
      rdata[SKNT] = sped;
      uinfo("%s F%03d %d  lat %f lon %f sped %d\n",gtime,hour,date,lat,lon,sped);
      sf_wsdd(&iflno,gtime,stid,&isnum,&lat,&lon,&selv,state,country,
            &hour,rdata,&ier,
            strlen(gtime),strlen(stid),strlen(state),strlen(country));
      if(ier != 0) uerror("error: sf_wsdd [%d]\n",ier);
      }

  
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
            pos = (char *)strtok(NULL,"\n");
            if(pos != NULL)
               {
               sped = -9999;
               cpos = strstr(pos,"MAX SUSTAINED WINDS - ");
               if(cpos != NULL) sscanf(cpos+21,"%d",&sped);
               scat = -9999;
               if(sped > 64) scat = 1;
               if(sped > 83) scat = 2;
               if(sped > 96) scat = 3;
               if(sped > 113) scat = 4;
               if(sped > 134) scat = 5;
               if((sped > 33)&&(sped <= 64)) scat = 6;
               if(sped <= 33) scat = 7;
               rdata[FCHR] = hour;
               rdata[SCAT] = scat;
               rdata[SKNT] = sped;
               uinfo("%s F%03d %d  lat %f lon %f sped %d\n",gtime,hour,date,lat,lon,sped);
               sf_wsdd(&iflno,gtime,stid,&isnum,&lat,&lon,&selv,state,country,
                  &hour,rdata,&ier,
                  strlen(gtime),strlen(stid),strlen(state),strlen(country));
               }
            else
               uerror("invalid forecast intensity format\n");
            }
         else
            uerror("invalid forecast hour format\n");
         
         }

      }
}

void read_nhc(the_prod,gemfile,packing,maxtim,yymmdd)
prod *the_prod;
char *gemfile,*packing;
int maxtim,yymmdd;
{
char state[3],country[3];
float selv;
float rdata[MAXVARS];
char gtime[20];
char name[30];
char title[80];
char valid[40];
int skt;

char line[4096];
char event[5];
char stid[9];
char LAT[5],LON[6];
char sym[20];
char type;
float lat,lon;
int year,month,day,hour;
char *pos,*cpos,*hold;
int bytes;
int isnum,ier,dis_num,space;
isnum = -9999;
selv = 0;
sprintf(state,"--\0");
sprintf(country,"--\0");
name[0] = 'A'; name[1] = '\0';
title[0] = '\0';
valid[0] = '\0';

   pos = (char *)strtok((char *)the_prod->bytes,"\n");
   while((pos != NULL) && (strstr(pos,"WTNT4")==NULL) &&
      (strstr(pos,"WTPA4")==NULL) &&
      (strstr(pos,"WTPZ4")==NULL))
         pos = (char *)strtok(NULL,"\n");

   if(pos == NULL) exit(1);

   if(strstr(pos,"WTPA4") != NULL)
         {
         /* New TCDAT2 line or TCDEP2 not in HNL WTPA yet */
         }
      else
         pos = (char *)strtok(NULL,"\n"); 

 /* ...For intergovernmental use only line */
      pos = (char *)strtok(NULL,"\n");
      pos = (char *)strtok(NULL,"\n");
      cpos = (char *)strstr(pos,"DISCUSSION NUMBER");
      sscanf(cpos+17,"%d",&dis_num);
     
      hold = pos + strlen(pos) + 1; 
      if((strncmp(pos,"TROPICAL",8)==0)||
         (strncmp(pos,"SUBTROPICAL",11)==0))
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
            }
 
      uinfo("%s filename %s discussion number %d\0",title,name,dis_num);
      pos = (char *)strtok(hold,"\n");


      /* now lets try to open a file using the storm name/number */
      line[0] = '\0';
      sprintf(line,"%s/%s.gem\0",gemfile,name);
      if ((iflno = open_gem_ship(line, packing, &maxtim, 
            &FCHR, &SKNT, &SCAT,
            strlen(line), strlen(packing))) == -1) {
          serror("can't open %s", line);
          exit(1);
          }

      if((FCHR < 0) || (FCHR >= MAXVARS)) exit(-1);
      if((SKNT < 0) || (SKNT >= MAXVARS)) exit(-1);
      if((SCAT < 0) || (SCAT >= MAXVARS)) exit(-1);

      while(pos != NULL) 
         {
         if(strncmp(pos,"INITIAL    ",11)==0)
            {
            space = 12;
            while((pos[space]!='\0')&&(pos[space] > ' '))
                 space = space + 1;
            strncat(valid,"FORECAST VALID ",15);
            strncat(valid,pos+12,space-12);
            valid[0] = '\0';
            strncat(valid,pos+12,5);
            sscanf(pos+space,"%f %*c %f %*c %d",&lat,&lon,&skt);
      
            stid[0] = '\0';
            sprintf(stid,"%c%c%c%c%c    ",name[0],pos[0],pos[1],pos[2],pos[3]);
            uinfo("%c%c%c%c%c    ",name[0],pos[0],pos[1],pos[2],pos[3]);

            gtime[0] = '\0';
            sprintf(gtime,"%04d%c%c/%c%c00\0",yymmdd/100,
               valid[0],valid[1],valid[3],valid[4]);
            isnum = -9999;
            selv = 0;
            sscanf(valid+3,"%d",&hour);
            hour = hour * 100;
            rdata[FCHR] = 00;
            rdata[SKNT] = skt;
           
            lon = -lon ;
            uinfo("%5.1f   %6.1f",lat,lon);
            uinfo("  ur:tx:%sZ   col:yellow:",valid);
            if(skt > 64)
               {
               rdata[SCAT] = 1;
               uinfo("wx:@+    ll:tx:%d\n",skt);
               }
            if(skt > 83) rdata[SCAT] = 2;
            if(skt > 96) rdata[SCAT] = 3;
            if(skt > 113) rdata[SCAT] = 4;
            if(skt > 134) rdata[SCAT] = 5;
            if((skt > 33)&&(skt <= 64))
               {
               uinfo("wx:@     ll:tx:%d\n",skt);
               rdata[SCAT] = 6;
               }
            if(skt <= 33)
               {
               rdata[SCAT] = 7;
               uinfo("tx:+     ll:tx:%d\n",skt);
               }

            sf_wsdd(&iflno,gtime,stid,&isnum,&lat,&lon,&selv,state,country,
               &hour,rdata,&ier,
            strlen(gtime),strlen(stid),strlen(state),strlen(country));
            if(ier != 0) uerror("sf_wsdd ier = %d\0",ier);

            } /* initial */
         if((strncmp(pos,"12HR VT    ",11) == 0) ||
            (strncmp(pos,"24HR VT    ",11) == 0) ||
            (strncmp(pos,"36HR VT    ",11) == 0) ||
            (strncmp(pos,"48HR VT    ",11) == 0) ||
            (strncmp(pos,"72HR VT    ",11) == 0))
            {
            space = 12;
            while((pos[space]!='\0')&&(pos[space] > ' '))
               space = space + 1;
            valid[0] = '\0';
            strncat(valid,pos+12,5);
            uinfo("%c%c%c%c%c    ",name[0],pos[0],pos[1],pos[2],pos[3]);
            sscanf(pos+space,"%f %*c %f %*c %d",&lat,&lon,&skt);
            lon = -lon;
            uinfo("%5.1f   %6.1f",lat,lon);
            uinfo("  ur:tx:%sZ   col:yellow:",valid);
            if(skt > 64)
               {
               rdata[SCAT] = 1;
               uinfo("wx:@+    ll:tx:%d\n",skt);
               }
            if(skt > 83) rdata[SCAT] = 2;
            if(skt > 96) rdata[SCAT] = 3;
            if(skt > 113) rdata[SCAT] = 4;
            if(skt > 134) rdata[SCAT] = 5;
            if((skt > 33)&&(skt <= 64))
               {
               rdata[SCAT] = 6;
               uinfo("wx:@     ll:tx:%d\n",skt);
               }
            if(skt <= 33)
               {
               rdata[SCAT] = 7;
               uinfo("tx:+     ll:tx:%d\n",skt);
               }
        
            rdata[SKNT] = skt; 
            sscanf(pos,"%d",&hour); 
            rdata[FCHR] = hour;
            sf_wsdd(&iflno,gtime,stid,&isnum,&lat,&lon,&selv,state,country,
               &hour,rdata,&ier,
               strlen(gtime),strlen(stid),strlen(state),strlen(country));         
            if(ier != 0) uerror("sf_wsdd ier = %d\0",ier);
            }


         pos = (char *)strtok(NULL,"\n");
         }
      
      

   sf_clos(&iflno,&ier);
}


read_tropic(gemfile, packing, logfile, yymmdd, timeout, maxtim)
char *gemfile; /* the path of the storm directory */
char *packing;
char *logfile;
int  yymmdd;
int  timeout;
int maxtim;
{
char line[256];
int year,month,day;
time_t clock;
struct tm *gmt_time=NULL,new_time;
char *realtime;
struct prod the_prod;
int bytes;


if(yymmdd < 0)
   {
   realtime = (char *)malloc(81);
   clock = time(NULL);
   gmt_time = gmtime(&clock);
   new_time = *gmt_time;
   strftime(realtime,80,"%y %m %d %H",&new_time);
   sscanf(realtime,"%d %d %d",&year,&month,&day);
   yymmdd = ((year*100)+month)*100 + day;
   free(realtime);
   }


while(1)
   {
   bytes = get_prod(timeout, &the_prod);
   if (bytes == 0)
      continue;

   line[0] = '\0';
   set_wmohdr((char *)(the_prod.bytes+32));
   strcpy(line,(char *)wmohdr());
   
   if((strstr(line,"WTNT4")!=NULL) ||
      (strstr(line,"WTPA4")!=NULL) ||
      (strstr(line,"WTPZ4")!=NULL))
      {
      unotice("calling read_nhc %s\n",line);
      read_nhc(&the_prod,gemfile,packing,maxtim,yymmdd);
      continue;
      }
   else
      if(strstr(line,"WTPN3")!=NULL)
         {
         unotice("calling read_guam %s\n",line);
         read_guam(&the_prod,gemfile,packing,maxtim,yymmdd);
         continue;
         }

   }
exit(0);         
}
