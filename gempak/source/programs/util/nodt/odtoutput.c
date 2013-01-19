#include "inc/odt.h"

void textscreenoutput( struct odtdata * );
void lldms( float,int *,int *,int * );

extern void yddmy( int,int *,int *,int * );
extern void xprintf( char * );
extern float getpwval( int,float );

/* extern global variables */
extern logical owind,spotanal,o48hr;
extern char iout[200],scenetype[10][6];
extern int  idomain;

void textscreenoutput(struct odtdata *odtcurrent)
/* Output information to text screen.
    Inputs  : odtcurrent - structure containting current image information
    Outputs : none
*/
{
  int   day,mon,year;
  int   degree,minute,second,xr9=0,xrap=0;
  char  cdate[13],ctime[13],clat[13],clon[13];
  char  cmin[3],csec[3];
  char  latc[2]="N",lonc[2]="W";
  char  cmon[12][4]={ "JAN","FEB","MAR","APR","MAY","JUN",
                      "JUL","AUG","SEP","OCT","NOV","DEC" };
  char  cpw[2][9]={ "Pressure", "Wind(kt)" };
  char  cyn[2][4]={ "OFF","ON " };
  char  cland[2][10]={ "OVER LAND","         " };
  char  cbasin[2][9]={ "ATLANTIC","PACIFIC " };
  float pwi,cloudtemp;

  /* convert Julian date/time to day/month/year format */
  yddmy(odtcurrent->IR.date,&day,&mon,&year);
  /* format character string for date output */
  sprintf(cdate,"%2d %3s %4d",day,cmon[mon-1],year);
  /* format character string for time output */
  sprintf(ctime,"  %6d UTC",odtcurrent->IR.time);

  /* convert xx.xxxx latitude format to degree/minute/second format */
  lldms(odtcurrent->IR.latitude,&degree,&minute,&second);
  sprintf(cmin,"%2d",minute); if(minute<10) sprintf(cmin,"0%1d",minute);
  sprintf(csec,"%2d",second); if(second<10) sprintf(csec,"0%1d",second);
  if(odtcurrent->IR.latitude<0.0F) strncpy(latc,"S",1);
  /* format character string for latitude output */
  sprintf(clat,"%3d:%2s:%2s %1s",degree,cmin,csec,latc);
  /* convert xx.xxxx longitude format to degree/minute/second format */
  lldms(odtcurrent->IR.longitude,&degree,&minute,&second);
  sprintf(cmin,"%2d",minute); if(minute<10) sprintf(cmin,"0%1d",minute);
  sprintf(csec,"%2d",second); if(second<10) sprintf(csec,"0%1d",second);
  if(odtcurrent->IR.longitude<0.0F) strncpy(lonc,"E",1);
  /* format character string for longitude output */
  sprintf(clon,"%3d:%2s:%2s %1s",degree,cmin,csec,lonc);

  /* determine pressure/wind speed in relation to final CI # */
  pwi=getpwval(owind,odtcurrent->IR.CI);

  /* determine Rule 9 and Rapid Deepening screen output values */
  if(odtcurrent->IR.rule9>=2) xr9=1;
  if(odtcurrent->IR.rapid>=2) xrap=1;
  cloudtemp=odtcurrent->IR.cloudt;
  if(odtcurrent->IR.rapid>1) cloudtemp=MIN(odtcurrent->IR.cloudt,odtcurrent->IR.meancloudt);

  /* send results to the screen */
  sprintf(iout,"\n****************************************************\n\n");xprintf(iout);
  sprintf(iout,"                      UW-CIMSS                      \n");xprintf(iout);
  sprintf(iout,"           Objective Dvorak Technique (ODT)         \n");xprintf(iout);
  sprintf(iout,"         Tropical Cyclone Intensity Algorithm       \n\n");xprintf(iout);
  sprintf(iout,"             ----- Current Analysis ----- \n");xprintf(iout);
  sprintf(iout,"     Date : %12s    Time : %12s\n",cdate,ctime);xprintf(iout);
  sprintf(iout,"      Lat : %12s     Lon : %12s\n\n",clat,clon);xprintf(iout);
  if(!spotanal) {
    sprintf(iout,"      CI-No./%8s   T-No.(Ave)   T-No.(Raw)\n",cpw[owind]);xprintf(iout);
    sprintf(iout,"        %3.1f /%6.1f         %3.1f         %3.1f\n\n",
            odtcurrent->IR.CI,pwi,odtcurrent->IR.Tfinal,odtcurrent->IR.Traw);xprintf(iout);
  } else {
    sprintf(iout,"                  CI No./%8s \n",cpw[owind]);xprintf(iout);
    sprintf(iout,"                    %3.1f /%6.1f\n\n",odtcurrent->IR.Traw,pwi);xprintf(iout);
  }
  sprintf(iout," Eye Temp : %5.1f C      Surrounding Temp : %5.1f C\n",
           odtcurrent->IR.eyet,cloudtemp);xprintf(iout);
  sprintf(iout,"                     Distance from Center : %3d km\n",odtcurrent->IR.ringdist);xprintf(iout);
  sprintf(iout," Scene Type : %5s                Basin : %8s\n",scenetype[odtcurrent->IR.scenetype],cbasin[idomain]);xprintf(iout);
  sprintf(iout,"                 %11s\n",cland[odtcurrent->IR.land-1]);xprintf(iout);
  if(!spotanal) {
    sprintf(iout," Flags :   Rule 9 : %3s  Rapid : %3s  48 Hour : %3s\n",cyn[xr9],cyn[xrap],cyn[o48hr]);xprintf(iout);
  }
  sprintf(iout,"\n****************************************************\n\n");xprintf(iout);
}

void lldms(float llval,int *degree,int *minute,int *second)
/* Convert degree.degree to degree/minute/second format.
   Inputs  : llval  - latitude/longitude to convert
   Outputs : degree - degrees
             minute - minutes
             second - seconds
*/
{
  int deg;
  float min,sec;

  deg=(int)llval;
  min=(llval-(float)deg)*60.0F;
  sec=(min-(float)((int)min))*60.0F;
  *degree=ABS(deg);
  *minute=ABS((int)min);
  *second=ABS((int)sec);
}
