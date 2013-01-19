#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <geminc.h>
#include <gemprm.h>

#include "mkdirs_open.h"

#define MAXSEGS 2000
#define TORNADO	0
#define SEVERE	1
#define WINTER	2
#define SPECIAL	3
#define NONPRCP	4
#define FWATCH	5
#define FWARN	6
#define FSTATE  7
#define HURRS	8

#ifdef UNDERSCORE
#define ti_mdif		ti_mdif_
#endif

/* global time for plotting valid watches */
char valid[12];

int CLASS;
int PIL;

FILE *fp;

int PRIOR,FUTURE,ADVANCE;


int is_valid(expires,isval)
/**********************************************************************
*  This subroutine determines the time difference between the bulletin
*  expiration time, and the plot valid time (current or user selected).
*  If the bulletin expiration time is not set, the subroutine returns
*  -1, otherwise the return value from ti_mdif is returned.
*  A time difference < 0 signifies the bulletin has expired.
***********************************************************************/
char *expires;
int *isval;
{
int iexpire[5],ivalid[5];
int ier,i,idif;

if(expires[0] == '\0') return(-1);

sscanf(valid,"%2d%2d%2d/%2d%2d",ivalid,ivalid+1,ivalid+2,ivalid+3,ivalid+4); 
iexpire[0] = ivalid[0];
iexpire[1] = ivalid[1];
sscanf(expires,"%2d%2d%2d",iexpire+2,iexpire+3,iexpire+4);
if((iexpire[2] < 3)&&(ivalid[2] > 26))
   {
   iexpire[1] += 1;
   if(iexpire[1] > 12)
      {
      iexpire[1] = 1;
      iexpire[0] += 1;
      }
   }
ti_mdif(iexpire,ivalid,&idif,&ier);
*isval = idif;
return(ier);
}




typedef struct zonelist {
   char *zone;
   struct zonelist *next;
   } zonelist;

void do_all(zone,nzones,head)
char *zone;
int *nzones;
zonelist **head;
{
char line[256],lzone[7];
FILE *g;
zonelist *zone_list;

char *maps,*zmaps,state[3];
char *pos,sstr[7];
int znum;

maps = (char *)getenv("ZONEMAPS");
if(maps == NULL)
   {
   printf("can't find maps\n");
   return;
   }
state[0] = tolower(zone[0]);
state[1] = tolower(zone[1]);
state[2] = '\0';

/* open map file, return if an error occurs */
zmaps = (char *)malloc(strlen(maps)+15);
sprintf(zmaps,"%s/%s-zone.bna\0",maps,state);
if((g = fopen(zmaps,"r")) == NULL)
   {
   printf("Couldn't open %s\n",zmaps);
   free(zmaps);
   return;
   }

sstr[0] = '\0'; sprintf(sstr,"%c%c ZONE\0",zone[0],zone[1]);

while(fgets(line,255,g) != NULL)
   {
   pos = (char *)strchr(line,',');

   if((pos != NULL)&&(strncmp(pos+2,sstr,7)==0))
      {
      sscanf(pos+10,"%d",&znum);
      lzone[0] = '\0';
      sprintf(lzone,"%c%cZ%03d\0",zone[0],zone[1],znum);
      zone_list = (zonelist *)malloc(sizeof(zonelist));
      zone_list->zone = (char *)malloc( (strlen(lzone)+1)*sizeof(char) );
      strcpy(zone_list->zone,lzone);
      zone_list->next = *head;
      *head = zone_list;
      *nzones = *nzones + 1;
      }
   }
free(zmaps);
fclose(g);

}


void do_range(zline,lzone,cpos,nzones,head)
/**********************************************************************
*  This routine creates a zone/county entry for each number in the 
*  range zone1>zone2.
***********************************************************************/
char *zline,*lzone;
int *cpos;
int *nzones;
zonelist **head;
{
char zone[20];
int i,zstart,zstop;
zonelist *zone_list;
char *tpos;


if(zline[*cpos] != '>')
   {
   printf("error in zone range %d %s\n",cpos,zline);
   return;
   }

*cpos = *cpos + 1;
zone[0] = '\0';
strncat(zone,lzone,3);

tpos = zline + (*cpos);
if(strncmp(tpos,zone,3) == 0) /* the second number in range is ssZ### */
   *cpos = *cpos + 3;

i = 0;
while((zline[*cpos+i] >= '0')&&(zline[*cpos+i] <= '9'))
   {
   strncat(zone,zline+*cpos+i,1);
   i++;
   }
*cpos = *cpos + i;
sscanf(lzone+3,"%d",&zstart);
sscanf(zone+3,"%d",&zstop);
i = zstart+1;
while((i <= zstop)&&(i < 1000))
   {
   zone[0] = '\0';
   sprintf(zone,"%03d\0",i);
   lzone[3] = '\0';
   strncat(lzone,zone,strlen(zone));
   zone_list = (zonelist *)malloc(sizeof(zonelist));
   zone_list->zone = (char *)malloc( (strlen(lzone)+1)*sizeof(char) );
   strcpy(zone_list->zone,lzone);
   zone_list->next = *head;
   *head = zone_list;
   *nzones = *nzones + 1;
   i++;
   }
}


void get_zones(line,wws,argc,argv,outf)
/**********************************************************************
*  This subroutine determines the county/zone identifiers and expiration
*  time from the bulletin identification line, and calls the mapping
*  routine. A linked list is used to store the county/zone ids found
*  in the identifier line. Each county is plotted by traversing the
*  list.
***********************************************************************/
char *line;
char *wws;
int argc;
char *argv[];
char *outf;
{
int nzones;
char *pos,zone[20],lzone[20],*zline,subject[25];
int cpos,llen,i,spos,zstart,zstop,line2;
int FOUND;

char expires[13],command[256];

zonelist *zone_list,*head;

lzone[0] = '\0';
expires[0] = '\0';
nzones = 0;

zone_list = head = NULL;

llen = strlen(line);
zline = (char *)malloc(llen+1);
zline[0] = '\0';

for(i=0;i<llen;i++)
   if(line[i] >= ' ') strncat(zline,line+i,1);

llen = strlen(zline);

/* printf("\nzones %s\n",zline); fips */

cpos = 0; spos = 0; line2 = 0;
while((cpos < llen)||(line2 == 0))
   {
   if(cpos >= llen)
      {
      line2 = -1;
      if((expires[0] == '\0')&&(cpos-spos >= 6)) /* check for no '-' after expiration date */
         {
         FOUND = 0;
         for(i=spos;i<cpos;i++) if((zline[i] < '0')||(zline[i] > '9')) FOUND = 1;
         if(FOUND == 0) strncat(expires,zline+spos,12);
         /*if(FOUND == 0)
           printf("Assuming expires %s\n",expires);   fips*/
         }
      if(expires[0] == '\0')
         {
         cpos = 0; spos = 0;
         if(fgets(line,255,fp) == NULL) continue;
         line2 = 0;
         llen = strlen(line);
         free(zline);
         zline = (char *)malloc(llen+1);
         zline[0] = '\0';

         for(i=0;i<llen;i++)
            if(line[i] >= ' ') strncat(zline,line+i,1);
         /*printf("  + %s\n",zline); fips*/
         }
      continue;
      }

   if((zline[cpos] == '-') || (zline[cpos] == '>'))
      {
      zone[0] = '\0';
      strncat(zone,zline+spos,cpos-spos);
      if(strcmp(zone,"DDHHNN") == 0)
	{ /* somebody goofed expiration time */
	expires[0] = valid[4];
	expires[1] = valid[5];
	expires[2] = valid[7];
	expires[3] = valid[8];
	expires[4] = valid[9];
	expires[5] = valid[10];
	}
      if((zone[0] >= 'A')&&(zone[0] <= 'Z'))
         {
         if(strncmp(zone+2,"ZALL",4) == 0)
            do_all(zone,&nzones,&head);
         else
            {
            lzone[0] = '\0';
            strncat(lzone,zone,strlen(zone));
            zone_list = (zonelist *)malloc(sizeof(zonelist));
            zone_list->zone = (char *)malloc( (strlen(lzone)+1)*sizeof(char) );
            strcpy(zone_list->zone,lzone);
            zone_list->next = head;
            head = zone_list;
            /*printf("look %d %s\n",nzones,head->zone);*/
            nzones++;
            if(zline[cpos] == '>') 
               do_range(zline,lzone,&cpos,&nzones,&head);
            }
         }
      else
         if(strlen(zone) >= 6)
            {
            strncat(expires,zone,12);
            }
         else
            {
            if((lzone[0] != '\0')&&(zone[0] != '\0'))
               {
               lzone[3] = '\0';
               strncat(lzone,zone,strlen(zone));
               zone_list = (zonelist *)malloc(sizeof(zonelist));
               zone_list->zone = (char *)malloc( (strlen(lzone)+1)*sizeof(char) );
               strcpy(zone_list->zone,lzone);
               zone_list->next = head;
               head = zone_list;
               nzones++;
               if(zline[cpos] == '>')
                  do_range(zline,lzone,&cpos,&nzones,&head);
               }
            else
               if(zone[0] != '\0') printf("Improper zone %s\n",zone);
            }

      cpos = cpos + 1;
      while((zline[cpos] == ' ')&&(cpos < llen)) cpos += 1;
      spos = cpos;
      }
   else
      cpos = cpos + 1;
   }

/*if(expires[0] == '\0') printf("length of line %d\n",strlen(line));*/

zone_list = head;
/*if(head != NULL) printf("expires %s\n  zones ",expires); i = 0;*/
subject[0] = '\0';
switch(CLASS)
   {
   case TORNADO:
	strcat(subject,"Tornado Warning\0");
        break;
   case SEVERE:
	strcat(subject,"Thunderstorm Warning\0");
        break;
   case WINTER:
	strcat(subject,"Winter-Wx Warning\0");
        break;
   case SPECIAL:
	strcat(subject,"Special Statement\0");
        break;
   case NONPRCP:
	strcat(subject,"Non-precip Warning\0");
        break;
   case FWATCH:
	strcat(subject,"Flood Watch\0");
        break;
   case FWARN:
	strcat(subject,"Flood Warning\0");
        break;
   case FSTATE:
	strcat(subject,"Flood Statement\0");
        break;
   case HURRS:
	strcat(subject,"Hurricane Statement\0");
        break;
   default:
	strcat(subject,"Weather Warning\0");
   }
while(head != NULL)
   {
   /*get_map(head->zone,expires); dont draw map */
   for(i=1;i<argc-1;i++)
      {
      command[0] = '\0';
#ifndef LINUX
      sprintf(command,"mailx -s \"%s\" %s < %s\0",subject,argv[argc-1],outf);
#else
      sprintf(command,"mail -s \"%s\" %s < %s\0",subject,argv[argc-1],outf);
#endif
      if(strcmp(head->zone,argv[i]) == 0) system(command);
      }

   zone_list = head;
   head = head->next;
   free(zone_list->zone);
   free(zone_list);
   }

free(zline);

}

void get_valid(wws)
/**********************************************************************
*  This routine returns the time for the plot. Current time is determined
*  from the system time, otherwise the user must provide the yymmdd/hhmm
*  time for the plot.
***********************************************************************/
char *wws;
{
int istarr[5],irtarr[5];
int ier, itype=1;

if((wws[0] == 'c')||(wws[0] == 'C'))
   {
   css_gtim ( &itype, valid, &ier);
   }
else
   {
   valid[0] = '\0';
   strncat(valid,wws,11);
   }

}


int check_bullet(line,state)
char *line;
int state;
{

if((line[0] < 'A')||(line[0] > 'Z')) return(0); /* first 2 letters must be state */
if((line[1] < 'A')||(line[1] > 'Z')) return(0); /* first 2 letters must be state */
/* allow for STZALL or STZ###-etc */
if(strncmp(line+3,"ALL",3) == 0) 
   {
   if((line[6] == '-')||(line[6] == '>')) return(1);
   }
if((line[3] < '0')||(line[3] > '9')) return(0); /* next 3 letters are fips number */
if((line[4] < '0')||(line[4] > '9')) return(0); /* next 3 letters are fips number */
if((line[5] < '0')||(line[5] > '9')) return(0); /* next 3 letters are fips number */
if((line[6] == '-')||(line[6] == '>')) return(1);
/*if(state == 2) printf("is this a fips %s?\n",line); fips*/

return(0);
}

int main(int argc, char *argv[])
/**********************************************************************
*  This routine is the driver routine. It should be called with the
*  information for the bulletin file and plot attributes. After
*  a valid bulletin is found, the first line of the county/zone
*  identifier is passed to the plotting routine. The file handle for
*  The bulletin file is stored in a global variable in case a second
*  line is used - which must be read if the expiration time is not
*  found on the first line.
***********************************************************************/
{

char line[4096];
char bulletin[81];
char tempnam[80];
int DONE;
int STATE;
int i;
int iret;
char *buf,*cpos,strpart[80];
static char wws[]="current";
int in_file,out_file,rlen;
mode_t omode=S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH;

if(argc < 3)
   {
   printf("Usage: cat file | fips zone1 [...zonen] address@email\n");
   exit(-1);
   }

tempnam[0] = '\0';
sprintf(tempnam,"/tmp/.tmpfips.XXXXXX\0");
mktemp(tempnam);

in_file = STDIN_FILENO;
out_file = mkdirs_open(tempnam,O_WRONLY|O_CREAT|O_TRUNC,omode);
if(out_file < 0)
   {
   printf("could not open temporary file %s\n",tempnam);
   exit(-1);
   }
buf = (char *)malloc(8196);
while((rlen = read(in_file,buf,8196)) > 0) write(out_file,buf,rlen);
free(buf);
close(out_file);

get_valid(wws);

DONE = 0;

STATE = 0;
fp = fopen(tempnam,"r");
while((DONE == 0)&&(fgets(line,255,fp)!=NULL))
   {
   if((STATE > 0)&&(strchr(line,'\001') != 0))
      {
      if(STATE == 2) printf(": %s does not conform\n",bulletin);
      STATE = 0;
      }
   if((STATE == 0) &&
      ((strstr(line,"WUUS") != 0)||(strstr(line,"WFUS") != 0)||
       (strstr(line,"WWUS") != 0)||(strstr(line,"RWUS") != 0)||
       (strstr(line,"WRUS") != 0)||(strstr(line,"WGUS") != 0) ))
      {
      CLASS = -1; PIL = -1;
      /* make a first stab */
      if(strstr(line,"WFUS") != 0) CLASS = TORNADO; 
      if(strstr(line,"WUUS") != 0) CLASS = SEVERE; 
      if(strstr(line,"WWUS4") != 0) CLASS = WINTER; 
      if(strstr(line,"WWUS8") != 0) CLASS = SPECIAL; 
      if(strstr(line,"WWUS7") != 0) CLASS = NONPRCP; 
      if(strstr(line,"RWUS") != 0) CLASS = FWATCH;
      if(strstr(line,"WRUS") != 0) CLASS = FWARN;
      if(strstr(line,"WGUS") != 0) CLASS = FSTATE;
      if(strstr(line,"WWUS31") != 0) CLASS = HURRS;
      STATE = 1;
      bulletin[0] = '\0';
      for(i=0;i<strlen(line);i++)
         if(line[i] >= ' ') strncat(bulletin,line+i,1);
      /*printf("\n\nbulletin %s ",bulletin); fips */
      continue;
      }
   switch(STATE)
      {
      case 1:
         if(line[0] != '\0')
            {
            if(strncmp(line,"WSW",3) == 0) PIL = WINTER;
            if(strncmp(line,"NPW",3) == 0) PIL = NONPRCP;
            if(strncmp(line,"SPS",3) == 0) PIL = SPECIAL;
            if(strncmp(line,"FLW",3) == 0) PIL = FWARN;
            if(strncmp(line,"SVR",3) == 0) PIL = SEVERE;
            if(strncmp(line,"TOR",3) == 0) PIL = TORNADO;
            if((PIL != -1)&&(PIL != CLASS))
               {
               CLASS = PIL;
               }
            }
         STATE = 2;
         break;
      case 2:
      case 4:
         if(strlen(line) < 7) continue; /* look for a FIPS line */
         if(((line[2] == 'C')||(line[2] == 'Z'))&&(check_bullet(line,STATE) == 1))
            {
            get_zones(line,wws,argc,argv,tempnam);
            STATE = 4;
            }
         break;
      }
   
   
   }

fclose(fp);
if(unlink(tempnam) != 0) printf("Could not delete temp file %s\n",tempnam);

exit(0);
}
