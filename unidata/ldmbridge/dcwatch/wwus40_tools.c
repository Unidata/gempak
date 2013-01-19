/*
 *      Copyright 1996, University Corporation for Atmospheric Research.
 *         Not for Direct Resale. All copies to include this notice.
 *	
 *	Chiz/Unidata	04/96
 */
#include <stdio.h>
#include <string.h>
#include <math.h>


#define SEVERE	1
#define	TORNADO	2
#define EW	1
#define NS	2
#define ES	3


void strip(bulletin)
char *bulletin;
{
int count;

for(count = 0; count < strlen(bulletin);count++)
   if((bulletin[count] < ' ')&&(bulletin[count] != '\n'))
      bulletin[count] = ' ';
}

int get_issue(bulletin,iscor)
char *bulletin;
int *iscor;
{
char *cpos;
int issue=-9999;
int status;
char *tpos;

*iscor = 0;

if((cpos = strstr(bulletin,"MKC AWW")) == NULL)
   cpos = strstr(bulletin,"SPC AWW");

if(cpos != NULL)
   {
   status = sscanf(cpos+7,"%d",&issue);
   if(status != 1) issue = -9999;
   }
else
   {
   if((cpos = strstr(bulletin,"MKC WW")) == NULL)
      cpos = strstr(bulletin,"SPC WW");
   if(cpos != NULL)
      {
      status = sscanf(cpos+6,"%d",&issue);
      if(status != 1) issue = -9999;
      }
   }

tpos = strchr(cpos,'\n');
if(tpos != NULL)
   {
   cpos = strstr(cpos,"COR");
   if((cpos != NULL)&&(cpos < tpos))
      *iscor = 1;
   }


return(issue);
}




int get_watchnum(bulletin,wnum,type,start,stop)
char *bulletin;
int *wnum,*type;
int *start,*stop;
{
char *cpos;
int count,status;

*start = -9999;
*stop = -9999;
cpos = strstr(bulletin,"\nWW ");
if(cpos != NULL)
   {
   sscanf(cpos+4,"%d",wnum);
   }
else
   return(-1);

if(strstr(cpos,"CANCELLED") != NULL) return(-10);

cpos = strchr(cpos+4,' ');
*type = 0;
if(strncmp(cpos+1,"SEVERE",6) == 0)
   *type = SEVERE;
if(strncmp(cpos+1,"TORNADO",7) == 0)
   *type = TORNADO;

if(*type > 0)
   {
   count = 7;
   while((cpos[count] != '\0')&&((cpos[count] < '0')||(cpos[count] > '9')))
      count++;
   if(cpos[count] != '\0')
      {
      status = sscanf(cpos+count,"%6dZ - %6dZ",start,stop);
      if(status != 2)
         *start = *stop = -9999;
      }
   }

return(0);

}

int get_coords(bulletin,loc1,head1,stid1,loc2,head2,stid2,dist,dir)
char *bulletin,*stid1,*stid2,*head1,*head2;
int *loc1,*loc2;
int *dist,*dir;
{
char *cpos;
int status,count;

*loc1 = *loc2 = -9999;
stid1[0] = '\0';
stid2[0] = '\0';
head1[0] = '\0';
head2[0] = '\0';

cpos = strstr(bulletin,"AXIS..");
if(cpos != NULL)
   {
   sscanf(cpos+6,"%d",dist);
   }
else
   return(-1);

*dir = 0;
if(strstr(cpos,"EAST AND WEST") != NULL)
   *dir = EW;
else 
   if(strstr(cpos,"NORTH AND SOUTH") != NULL)
      *dir = NS;
   else
      if(strstr(cpos,"EITHER SIDE") != NULL)
         *dir = ES;

if(*dir != 0)
   {
   cpos = strchr(cpos,'\n');
   if(cpos == NULL) return(-1);
   cpos = cpos + 1;
   status = sscanf(cpos,"%d%s %3s/",loc1,head1,stid1);
   if(status < 3)
      {
      sscanf(cpos,"%3s/",stid1);
      *loc1 = 0;
      }
   count = 0;
   while((cpos[count] != '\0')&&(cpos[count] != '\n')&&(cpos[count] != '-'))
      count++;
   if(cpos[count] == '-')
      {
      status = sscanf(cpos+count+1,"%d%s %3s/",loc2,head2,stid2);
      if(status < 3)
         {
         sscanf(cpos+count+1,"%3s/",stid2);
         *loc2 = 0;
         }
      }
   else
      uerror("oops check count %d %d\n",count,cpos[count]);
   }
   


return(0);
}

int get_angle(compass,angle)
char *compass;
float *angle;
{
if(strcmp(compass,"NNE") == 0)
   {
   *angle = 22.5;
   return(0);
   }
if(strcmp(compass,"ENE") == 0)
   {
   *angle = 67.5;
   return(0);
   }
if(strcmp(compass,"NE") == 0)
   {
   *angle = 45;
   return(0);
   }
if(strcmp(compass,"E") == 0)
   {
   *angle = 90;
   return(0);
   }
if(strcmp(compass,"ESE") == 0)
   {
   *angle = 112.5;
   return(0);
   }
if(strcmp(compass,"SSE") == 0)
   {
   *angle = 157.5;
   return(0);
   }
if(strcmp(compass,"SE") == 0)
   {
   *angle = 135;
   return(0);
   }
if(strcmp(compass,"S") == 0)
   {
   *angle = 180;
   return(0);
   }
if(strcmp(compass,"SSW") == 0)
   {
   *angle = 202.5;
   return(0);
   }
if(strcmp(compass,"WSW") == 0)
   {
   *angle = 247.5;
   return(0);
   }
if(strcmp(compass,"SW") == 0)
   {
   *angle = 225;
   return(0);
   }
if(strcmp(compass,"W") == 0)
   {
   *angle = 270;
   return(0);
   }
if(strcmp(compass,"WNW") == 0)
   {
   *angle = 292.5;
   return(0);
   }
if(strcmp(compass,"NNW") == 0)
   {
   *angle = 337.5;
   return(0);
   }
if(strcmp(compass,"NW") == 0)
   {
   *angle = 315;
   return(0);
   }
if(strcmp(compass,"N") == 0)
   {
   *angle = 0;
   return(0);
   }

*angle = -9999;
return(-1);
}


