/*
 *      Copyright 1996, University Corporation for Atmospheric Research.
 *         Not for Direct Resale. All copies to include this notice.
 *	
 * 					return codes
 *					-3      Second line not found
 *					-2	End of bulletin
 *					-1	Not a STORM bulletin
 *					0       Return with data
 *					1       Not a data line
 *	Chiz/Unidata	04/96
 */
#include <stdio.h>
#include <string.h>
#include <math.h>

#ifdef UNDERSCORE
#define read_storm	read_storm_
#endif


void read_storm(bultin, lenbul, ibpnt, rdata, iret)
char *bultin;
int *lenbul,*ibpnt;
float rdata[];
int *iret;
{
char line[512];
char *pos,*cpos;
int ier,FOUND;

char type;
int day,hour;
char LAT[5],LON[6];
float lat,lon;

if(*lenbul <= *ibpnt) 
   {
   *iret = -2;
   return;
   }


bultin[*lenbul] = '\0';

if((strstr(bultin,"WWUS60")==NULL) &&
   (strstr(bultin,"NWUS2") == NULL))
   {
   *iret = -1;
   return;
   }

pos = strchr(bultin+*ibpnt-1,'\n');

if(pos == NULL)
   {
   *iret = -2;
   /*printf("no line feed found %d %d\n",*ibpnt,*lenbul);*/
   return;
   }

line[0] = '\0';
strncat(line,bultin+*ibpnt-1,pos-(bultin+*ibpnt-1));
   
*ibpnt = *ibpnt + pos-(bultin+*ibpnt-1) +1;
*iret = 1;
if(strlen(line) < 4)
   {
   return;
   }

/*printf("read_storm line %d %d\n",*ibpnt - 1,pos-(bultin+*ibpnt-1));*/
if((line[3] >= '0')&&(line[3] <= '9'))
   {
   /*printf("look at line %s\n",line);*/
   type = line[6];
   sscanf(line+72,"%2d",&day);
   sscanf(line+75,"%4d",&hour);
   /*printf("type = %c Day %d hour %d\n",type,day,hour);*/
   FOUND = 0;
   while((*ibpnt < *lenbul)&&(FOUND == 0))
      {
      cpos = strchr(bultin+*ibpnt-1,'\n');
      if(pos == NULL) 
         {
         *ibpnt = *lenbul;
         printf("oops\n");
         continue;
         }
      line[0] = '\0';
      strncat(line,bultin+*ibpnt-1,cpos-(bultin+*ibpnt-1));
      *ibpnt = *ibpnt + cpos-(bultin+*ibpnt-1) +1;
      if(strlen(line) > 68) FOUND = 1;
      }
   if(FOUND == 0) 
      {
      *iret = -3;
      return;
      }

   /*printf("line2 %s\n",line);*/

   strncpy(LAT,line+69,4); LAT[4] = '\0';
   strncpy(LON,line+73,5); LON[5] = '\0';
   sscanf(LAT,"%f",&lat);
   sscanf(LON,"%f",&lon);
   lat = lat / 100;
   lon = lon / -100;
   switch(type)
      {
      case 'T':
         rdata[0] = 1;
         break;
      case 'W':
         rdata[0] = 2;
         break;
      case 'A':
         rdata[0] = 3;
         break;
      case 'G':
         rdata[0] = 4;
         break;
      default:
         rdata[0] = 9;
      }
   rdata[1] = 0;
   rdata[2] = day;
   rdata[3] = hour;
   rdata[4] = lat;
   rdata[5] = lon;
   }
else
   return;

*iret = 0;

return;

}
