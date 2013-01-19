/*
 *      Copyright 1996, University Corporation for Atmospheric Research.
 *         Not for Direct Resale. All copies to include this notice.
 *	
 *	Chiz/Unidata	04/96
 */
#include <stdio.h>
#include <string.h>
#include <math.h>

#ifdef UNDERSCORE
#define dcwwus40 dcwwus40_
#define lc_fstn lc_fstn_
#endif

#define SEVERE	1
#define	TORNADO	2
#define EW	1
#define NS	2
#define ES	3
#define KM_SM	1.609347    /* Statute miles to kilometers */

#define MAXVARS 15
#define ISSUE	0
#define TYPE	1
#define START	2
#define STOP	3
#define WNUM	4
#define LAT1	5
#define LON1	6
#define LAT2	7
#define LON2	8
#define LAT3	9
#define LON3	10
#define LAT4	11
#define LON4	12
#define CANCEL	13
#define CORR    14



void dcwwus40(bulletin,lenbul,ibpnt,stntbl,rdata,iret)
/*******************************************************
			return values
			 -1 = Bad Issue Format
			 -2 = Bad Watch Number
			 -3 = Bad Coordinates
			 -4 = Station not found
			 -5 = Bad station angle
			 -6 = Bad orientation
			 10 = Watch Cancelled
			 20 = Correction
********************************************************/
char *bulletin;
int  *lenbul, *ibpnt;
char *stntbl;
float rdata[];
int *iret;
{
int ier;
int issue;
int wnum,type;
int start,stop;
int dist,dir;
char stid1[9],stid2[9];
int loc1,loc2;
char head1[5],head2[5];
float lat1,lon1,lat2,lon2;
float angle1,angle2;
float x,y;
float lat_11,lon_11,lat_12,lon_12;
float lat_21,lon_21,lat_22,lon_22;
float ang,slope;
int iscor;

*iret = 0;

bulletin[*lenbul] = '\0';
strip(bulletin);

issue = get_issue(bulletin,&iscor);
if(issue < 0)
   {
   *iret = -1;
   return;
   }
else
   {
   rdata[ISSUE] = issue;
   rdata[CORR] = iscor;
   }

ier = get_watchnum(bulletin,&wnum,&type,&start,&stop);
if(ier == -1)
   {
   *iret = -2;
   return;
   }
else
   {
   rdata[TYPE] = type;
   rdata[WNUM] = wnum;
   rdata[START] = start;
   rdata[STOP] = stop;
   }

if(ier == -10)
   {
   rdata[CANCEL] = 1;
   *iret = 10;
   return;
   }
else
   rdata[CANCEL] = 0;

ier = get_coords(bulletin,&loc1,head1,stid1,&loc2,head2,stid2,&dist,&dir);
if(ier != 0)
   {
   *iret = -3;
   return;
   }


lc_fstn(stntbl,stid1,&lat1,&lon1,&ier,strlen(stntbl),strlen(stid1));
if(ier != 0)
   {
   *iret = -4;
   return;
   }
    
lc_fstn(stntbl,stid2,&lat2,&lon2,&ier,strlen(stntbl),strlen(stid2));
if(ier != 0)
   {
   *iret = -4;
   return;
   }
    
if(loc1 > 0)
   {
   ier = get_angle(head1,&angle1);
   if(ier != 0) 
      {
      *iret = -5;
      return;
      }
   x = -cos((angle1+90.) * M_PI / 180.);
   y = sin((angle1+90.) * M_PI / 180.);
   lat1 = lat1 + (y * loc1 * KM_SM / 111.11 );
   lon1 = lon1 + (x * loc1 * KM_SM / (111.11 * cos(lat1*M_PI / 180.)));
   }
if(loc2 > 0)
   {
   ier = get_angle(head2,&angle2);
   if(ier != 0) 
     { 
      *iret = -5;
     return;
     }
   x = -cos((angle2+90.) * M_PI / 180.);
   y = sin((angle2+90.) * M_PI / 180.);
   lat2 = lat2 + (y * loc2 * KM_SM / 111.11 );
   lon2 = lon2 + (x * loc2 * KM_SM / (111.11 * cos(lat2*M_PI / 180.)));
   }

switch(dir)
   {
   case EW:
      lat_11 = lat1;
      lat_12 = lat1;
      lat_21 = lat2;
      lat_22 = lat2;
      lon_11 = lon1 - dist * KM_SM / (111.11 * cos(lat1*M_PI / 180.));
      lon_12 = lon1 + dist * KM_SM / (111.11 * cos(lat1*M_PI / 180.));
      lon_21 = lon2 - dist * KM_SM / (111.11 * cos(lat2*M_PI / 180.));
      lon_22 = lon2 + dist * KM_SM / (111.11 * cos(lat2*M_PI / 180.));
      break;
   case NS:
      lon_11 = lon1;
      lon_12 = lon1;
      lon_21 = lon2;
      lon_22 = lon2;
      lat_11 = lat1 + dist * KM_SM / 111.11;
      lat_12 = lat1 - dist * KM_SM / 111.11;
      lat_21 = lat2 + dist * KM_SM / 111.11;
      lat_22 = lat2 - dist * KM_SM / 111.11;
      break;
   case ES:
      if(((lon2 - lon1) != 0)&&((lat2 - lat1) != 0))
         {
         slope = (lat2 - lat1)/(lon2 - lon1);
         ang = (M_PI/2.) - fabs(atan(slope));
         x = dist * cos(ang);
         y = dist * sin(ang);
         if(slope < 0) y = -y;
         lat_11 = lat1 + y * KM_SM / 111.11;
         lat_12 = lat1 - y * KM_SM / 111.11;
         lat_21 = lat2 + y * KM_SM / 111.11;
         lat_22 = lat2 - y * KM_SM / 111.11;
         lon_11 = lon1 - x * KM_SM / (111.11 * cos(lat1*M_PI / 180.));
         lon_12 = lon1 + x * KM_SM / (111.11 * cos(lat1*M_PI / 180.));
         lon_21 = lon2 - x * KM_SM / (111.11 * cos(lat2*M_PI / 180.));
         lon_22 = lon2 + x * KM_SM / (111.11 * cos(lat2*M_PI / 180.));
         }
      if(lon2 == lon1) /* same as EW */
         {
         lat_11 = lat1;
         lat_12 = lat1;
         lat_21 = lat2;
         lat_22 = lat2;
         lon_11 = lon1 - dist * KM_SM / (111.11 * cos(lat1*M_PI / 180.));
         lon_12 = lon1 + dist * KM_SM / (111.11 * cos(lat1*M_PI / 180.));
         lon_21 = lon2 - dist * KM_SM / (111.11 * cos(lat2*M_PI / 180.));
         lon_22 = lon2 + dist * KM_SM / (111.11 * cos(lat2*M_PI / 180.));
         }
      if(lat2 == lat1) /* same as NS */
         {
         lon_11 = lon1;
         lon_12 = lon1;
         lon_21 = lon2;
         lon_22 = lon2;
         lat_11 = lat1 + dist * KM_SM / 111.11;
         lat_12 = lat1 - dist * KM_SM / 111.11;
         lat_21 = lat2 + dist * KM_SM / 111.11;
         lat_22 = lat2 - dist * KM_SM / 111.11;
         }
      break;
   default:
      *iret = -6;
      return;
   }

rdata[LAT1] = lat_11;
rdata[LON1] = lon_11;
rdata[LAT2] = lat_12;
rdata[LON2] = lon_12;
rdata[LAT3] = lat_21;
rdata[LON3] = lon_21;
rdata[LAT4] = lat_22;
rdata[LON4] = lon_22;

if(rdata[CORR] != 0)
   *iret = 20;

}
