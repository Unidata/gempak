/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int  aodtv72_textscreenoutput(char *);
void aodtv72_msg( int, int, float, float, char *,char *);
void aodtv72_lldms( float,int *,int *,int * );
void aodtv72_listforecast( char * );

int aodtv72_textscreenoutput(char *string) 
/* Output information to text screen.
    Inputs  : global structure odtcurrent_v72 containting current image information
    Outputs : none
*/
{
  int   day,mon,year,ibasin,iok;
  int   degree,minute,second,xr8=0,xr9=0,xrpd=0;
  int   cloudcat,eyecat,cfft;
  int   ptr,ptr2,nptr;
  char  *cdate,*ctime,*clat,*clon;
  char  *cmin,*csec,*clatmax,*clonmax;
  char  *iout2;
  char  latc[2]="N",lonc[2]="W";
  char  cr9[3][7]={ "OFF   ","ON    ","WEAKEN" };
  char  crpd[4][7]={ "OFF   ","FLAG  ","ON    ","ON    " };
  char  cbd[7][8]={ "LOW CLD","OFF WHT","DK GRAY","MD GRAY","LT GRAY","BLACK  ","WHITE  " };
  char cr8[30][10]={ "NO LIMIT ","0.5T/6hr ","1.2T/6hr ","1.7T/12hr","2.2T/18hr","2.7T/24hr",
                     "         ","         ","0.1T/hour","0.5T/hour",
                     "NO LIMIT ","0.5T/6hr ","1.7T/6hr ","2.2T/12hr","2.7T/18hr","3.2T/24hr",
                     "         ","         ","0.1T/hour","0.5T/hour",
                     "NO LIMIT ","0.5T/6hr ","0.7T/6hr ","1.2T/12hr","1.7T/18hr","2.2T/24hr",
                     "         ","         ","0.1T/hour","0.5T/hour" };
  char  basin[4][13]={ "ATLANTIC    ","WEST PACIFIC","EAST PACIFIC","INDIAN      " };
  char  cuse[7][25]={ "MANUAL",
                      "FORECAST INTERPOLATION","LAPLACIAN ANALYSIS",
                      "WARMEST PIXEL SEARCH","SPIRAL ANALYSIS",
                      "RING/SPIRAL COMBINATION","LINEAR EXTRAPOLATION" };
  char  *scenetype,*scenetype2;
  char  *scenetypemax,*scenetypemax2;
  char  *scenetypemaxll,*scenetypemaxll2;
  char  *eyermw;
  char  *version;
  float pwip,pwiw,cloudtemp,arcd,arcdmax,sdist,xlat,xlon,m;
  logical bettercb=FALSE,rmwflag=FALSE;

  version=(char *)calloc((size_t)25,sizeof(char));
  eyermw=(char *)calloc((size_t)5,sizeof(char));
  cdate=(char *)calloc((size_t)13,sizeof(char));
  ctime=(char *)calloc((size_t)13,sizeof(char));
  clat=(char *)calloc((size_t)13,sizeof(char));
  clon=(char *)calloc((size_t)13,sizeof(char));
  cmin=(char *)calloc((size_t)3,sizeof(char));
  csec=(char *)calloc((size_t)3,sizeof(char));
  clatmax=(char *)calloc((size_t)13,sizeof(char));
  clonmax=(char *)calloc((size_t)13,sizeof(char));
  scenetype=(char *)calloc((size_t)50,sizeof(char));
  scenetype2=(char *)calloc((size_t)50,sizeof(char));
  scenetypemax=(char *)calloc((size_t)50,sizeof(char));
  scenetypemax2=(char *)calloc((size_t)50,sizeof(char));
  scenetypemaxll=(char *)calloc((size_t)50,sizeof(char));
  scenetypemaxll2=(char *)calloc((size_t)50,sizeof(char));
  iout2=(char *)calloc((size_t)5000,sizeof(char));

  /* convert Julian date/time to day/month/year format */
  aodtv72_yddmy(odtcurrent_v72->IR.date,&day,&mon,&year);
  /* format character string for date output */
  sprintf(cdate,"%2.2d %3s %4d",day,cmon_v72[mon-1],year);
  cdate[strlen(cdate)]='\0';
  /* format character string for time output */
  sprintf(ctime,"  %6.6d UTC",odtcurrent_v72->IR.time);
  ctime[strlen(ctime)]='\0';

  /* convert xx.xxxx latitude format to degree/minute/second format */
  xlat=odtcurrent_v72->IR.latitude;
  xlon=odtcurrent_v72->IR.longitude;
  aodtv72_lldms(xlat,&degree,&minute,&second);
  sprintf(cmin,"%2.2d",minute);
  sprintf(csec,"%2.2d",second);
  if(xlat<0.0) strncpy(latc,"S",1);
  /* format character string for latitude output */
  sprintf(clat,"%3d:%2s:%2s %1s",degree,cmin,csec,latc);
  clat[strlen(clat)]='\0';
  /* convert xx.xxxx longitude format to degree/minute/second format */
  aodtv72_lldms(xlon,&degree,&minute,&second);
  sprintf(cmin,"%2.2d",minute);
  sprintf(csec,"%2.2d",second);
  if(xlon<0.0) strncpy(lonc,"E",1);
  /* format character string for longitude output */
  sprintf(clon,"%3d:%2s:%2s %1s",degree,cmin,csec,lonc);
  clon[strlen(clon)]='\0';
  /* determine current ocean basin in which storm is located */
  ibasin=aodtv72_oceanbasin(xlat,xlon);

  /* determine Dvorak pressure/wind speed in relation to final CI # */
  pwip=aodtv72_getpwval(0,odtcurrent_v72->IR.CI);
  pwiw=aodtv72_getpwval(1,odtcurrent_v72->IR.CI);

  /* determine Rule 8 and Rule 9 screen output values */
  xr8=odtcurrent_v72->IR.rule8;
  if(odtcurrent_v72->IR.rule9==1) xr9=1;
  xrpd=odtcurrent_v72->IR.rapiddiss;
  cloudtemp=odtcurrent_v72->IR.cloudt;

  /* determine scenetype to be output to screen */
  eyecat=odtcurrent_v72->IR.eyescene;
  cloudcat=odtcurrent_v72->IR.cloudscene;
  cfft=odtcurrent_v72->IR.cloudfft;
  if(cloudcat==2) {
    sprintf(scenetype,"%s*",cloudtype_v72[cloudcat]);
    scenetype[strlen(scenetype)]='\0';
  } else if(cloudcat==3) {
    arcd=(float)(odtcurrent_v72->IR.ringcbval-1)/24.0;
    arcdmax=(float)(odtcurrent_v72->IR.ringcbvalmax-1)/25.0;
    if(arcdmax>arcd) bettercb=TRUE;
    sprintf(scenetype,"CURVED BAND with %4.2f ARC in %s*",arcd,cbd[odtcurrent_v72->IR.ringcb]);
    scenetype[strlen(scenetype)]='\0';
    if(bettercb) {
      sprintf(scenetypemax,"Maximum CURVED BAND with %4.2f ARC in %s*",arcdmax,cbd[odtcurrent_v72->IR.ringcb]);
      scenetypemax[strlen(scenetypemax)]='\0';
      /* convert xx.xxxx latitude format to degree/minute/second format */
      aodtv72_lldms(odtcurrent_v72->IR.ringcblatmax,&degree,&minute,&second);
      sprintf(cmin,"%2d",minute); if(minute<10) sprintf(cmin,"0%1d",minute);
      sprintf(csec,"%2d",second); if(second<10) sprintf(csec,"0%1d",second);
      if(odtcurrent_v72->IR.ringcblatmax<0.0) strncpy(latc,"S",1);
      /* format character string for latitude output */
      sprintf(clatmax,"%3d:%2s:%2s %1s",degree,cmin,csec,latc);
      clatmax[strlen(clatmax)]='\0';
      /* convert xx.xxxx longitude format to degree/minute/second format */
      aodtv72_lldms(odtcurrent_v72->IR.ringcblonmax,&degree,&minute,&second);
      sprintf(cmin,"%2d",minute); if(minute<10) sprintf(cmin,"0%1d",minute);
      sprintf(csec,"%2d",second); if(second<10) sprintf(csec,"0%1d",second);
      if(odtcurrent_v72->IR.ringcblonmax<0.0) strncpy(lonc,"E",1);
      /* format character string for longitude output */
      sprintf(clonmax,"%3d:%2s:%2s %1s",degree,cmin,csec,lonc);
      clonmax[strlen(clonmax)]='\0';
      sprintf(scenetypemaxll," at Lat:%12s  Lon:%12s*",clatmax,clonmax);
      scenetypemaxll[strlen(scenetypemaxll)]='\0';
    }
  } else if(cloudcat==4) {
    sdist=odtcurrent_v72->IR.eyecdosize/110.0;
    if(sdist<1.30) {
      sprintf(scenetype,"SHEAR (%4.2f^ TO DG)*",sdist);
    } else {
      sprintf(scenetype,"SHEAR (>1.25^ TO DG)*");
    }
    scenetype[strlen(scenetype)]='\0';
  } else {
    if(eyecat<=2) {
      sprintf(scenetype,"%s *",eyetype_v72[eyecat]);
      if(eyecat<=2) rmwflag=TRUE;
      printf("rmw=%f\n",odtcurrent_v72->IR.rmw);
      if(odtcurrent_v72->IR.rmw<0.0) {
        if(eyecat==1) { 
          sprintf(eyermw,"<10");
        } else {
          sprintf(eyermw,"N/A");
        }
      } else {
        sprintf(eyermw,"%3d",(int)odtcurrent_v72->IR.rmw);
        /* if(eyecat==1) sprintf(eyermw,"<10"); */
      }
    } else {
      sprintf(scenetype,"%s CLOUD REGION*",cloudtype_v72[cloudcat]);
    }
    scenetype[strlen(scenetype)]='\0';
  }
  /* if((hfile_v72!=(char *)NULL)&&(odthistoryfirst==0)) { */
  if(((strlen(hfile_v72)!=0))&&(odthistoryfirst_v72==0)) {
    cloudcat=0;
    if(osstr_v72>1.0) {
      sprintf(scenetype,"USER DEFINED INITIAL CLASSIFICATION*");
    } else {
      if(osstr_v72==1.0) sprintf(scenetype,"INITIAL CLASSIFICATION*");
    }
    scenetype[strlen(scenetype)]='\0';
  }

  ptr = (int)scenetype;
  ptr2 = (int)strchr(scenetype,'*');
  nptr=ptr2-ptr;
  strcpy(scenetype2,scenetype);
  scenetype2[nptr]='\0';
  if(bettercb) {
    ptr = (int)scenetypemax;
    ptr2 = (int)strchr(scenetypemax,'*');
    nptr=ptr2-ptr;
    strcpy(scenetypemax2,scenetypemax);
    scenetypemax2[nptr]='\0';
    ptr = (int)scenetypemaxll;
    ptr2 = (int)strchr(scenetypemaxll,'*');
    nptr=ptr2-ptr;
    strcpy(scenetypemaxll2,scenetypemaxll);
    scenetypemaxll2[nptr]='\0';
  }

  iok=aodtv72_getversion(version);  /* return 0 */
  /* send results to the screen */
  sprintf(iout2,"\n****************************************************\n\n");strcat(string,iout2);
  sprintf(iout2,"                     UW - CIMSS                     \n");strcat(string,iout2);
  sprintf(iout2,"              ADVANCED DVORAK TECHNIQUE       \n");strcat(string,iout2);
  sprintf(iout2,"                  %17s                \n",version);strcat(string,iout2);
  sprintf(iout2,"         Tropical Cyclone Intensity Algorithm       \n\n");strcat(string,iout2);
  sprintf(iout2,"             ----- Current Analysis ----- \n");strcat(string,iout2);
  sprintf(iout2,"     Date : %12s    Time : %12s\n",cdate,ctime);strcat(string,iout2);
  sprintf(iout2,"      Lat : %12s     Lon : %12s\n\n",clat,clon);strcat(string,iout2);
  if((oland_v72)&&(odtcurrent_v72->IR.land==1)) {
    sprintf(iout2,"              TROPICAL CYCLONE OVER LAND\n");strcat(string,iout2);
    sprintf(iout2,"              NO ADT ANALYSIS AVAILABLE\n");strcat(string,iout2);
  } else {
    sprintf(iout2,"                CI# /Pressure/ Vmax\n");strcat(string,iout2);
    sprintf(iout2,"                %3.1f /%6.1fmb/%5.1fkt\n\n",odtcurrent_v72->IR.CI,pwip+odtcurrent_v72->IR.CIadjp,pwiw);strcat(string,iout2);
    if(strlen(hfile_v72)>0) {
      sprintf(iout2,"      6hr-Avg T#  3hr-Avg T#   Adj T#   Raw T# \n");strcat(string,iout2);
      sprintf(iout2,"         %3.1f         %3.1f        %3.1f      %3.1f\n\n",
              odtcurrent_v72->IR.Tfinal,odtcurrent_v72->IR.Tfinal3,odtcurrent_v72->IR.Traw,odtcurrent_v72->IR.TrawO);strcat(string,iout2);
    }
    sprintf(iout2,"     Latitude bias adjustment to MSLP : %+5.1fmb\n\n",odtcurrent_v72->IR.CIadjp);strcat(string,iout2);
    if(rmwflag) { sprintf(iout2," Estimated radius of max. wind based on IR :%3s km\n\n",eyermw);strcat(string,iout2); }
    sprintf(iout2," Center Temp : %+5.1fC    Cloud Region Temp : %5.1fC\n",
             odtcurrent_v72->IR.eyet,cloudtemp);strcat(string,iout2);
    sprintf(iout2,"\n Scene Type : %s \n",scenetype2);strcat(string,iout2);
    if(bettercb) {
      sprintf(iout2,"              %s \n",scenetypemax2);strcat(string,iout2);
      sprintf(iout2,"              %s \n",scenetypemaxll2);strcat(string,iout2);
    }
    sprintf(iout2,"\n Positioning Method : %s \n",cuse[odtcurrent_v72->IR.autopos]);strcat(string,iout2);
    sprintf(iout2,"\n Ocean Basin : %12s  \n",basin[ibasin]);strcat(string,iout2);
    sprintf(iout2," Dvorak CI > MSLP Conversion Used : %8s  \n",cbasin_v72[idomain_v72]);strcat(string,iout2);
    if(strlen(hfile_v72)>0) {
      sprintf(iout2,"\n Tno/CI Rules : Constraint Limits : %9s\n",cr8[xr8]);strcat(string,iout2);
      sprintf(iout2,"                   Weakening Flag : %6s\n",cr9[xr9]);strcat(string,iout2);
      sprintf(iout2,"           Rapid Dissipation Flag : %6s\n",crpd[xrpd]);strcat(string,iout2);
    }
/*
    if(odtcurrent_v72->IR.sst>-10.0) {
      sprintf(iout2,"\n----------------------------------------------------\n\n");strcat(string,iout2);
      sprintf(iout2,"                     UW - CIMSS                     \n");strcat(string,iout2);
      sprintf(iout2,"     TROPICAL CYCLONE INTENSITY ESTIMATE MODEL\n");strcat(string,iout2);
      sprintf(iout2,"           TIE MODEL - Version 0.0A Beta         \n\n");strcat(string,iout2);
      sprintf(iout2,"           Sea Surface Temperature=%4.1fC\n",odtcurrent_v72->IR.sst);strcat(string,iout2);
      if(odtcurrent_v72->IR.TIEflag>0) {
        sprintf(iout2,"      Current Pressure/Vmax :%6.1fmb/%5.1fkt\n",odtcurrent_v72->IR.TIEraw,aodtv72_ptovmax(odtcurrent_v72->IR.TIEraw));strcat(string,iout2);
        if(strlen(hfile_v72)>0) {
          sprintf(iout2,"     12hr-Avg Pressure/Vmax :%6.1fmb/%5.1fkt\n",odtcurrent_v72->IR.TIEavg,aodtv72_ptovmax(odtcurrent_v72->IR.TIEavg));strcat(string,iout2);
        }
      } else {
        sprintf(iout2,"     TIE Model Intensity Estimate Not Available\n");strcat(string,iout2);
      }
    }
*/
  }
  sprintf(iout2,"\n****************************************************\n\n");strcat(string,iout2);
  string[strlen(string)]='\0';

  free(version); version=NULL;
  free(cdate); cdate=NULL;
  free(ctime); ctime=NULL;
  free(clat); clat=NULL;
  free(clon); clon=NULL;
  free(cmin); cmin=NULL;
  free(csec); csec=NULL;
  free(clatmax); clatmax=NULL;
  free(clonmax); clonmax=NULL;
  free(scenetype); scenetype=NULL;
  free(scenetype2); scenetype2=NULL;
  free(scenetypemax); scenetypemax=NULL;
  free(scenetypemax2); scenetypemax2=NULL;
  free(scenetypemaxll); scenetypemaxll=NULL;
  free(scenetypemaxll2); scenetypemaxll2=NULL;
  free(eyermw); eyermw=NULL;
  free(iout2); iout2=NULL;
  return 0;
}

void aodtv72_lldms(float llval,int *degree,int *minute,int *second)
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
  min=(llval-(float)deg)*60.0;
  sec=(min-(float)((int)min))*60.0;
  *degree=A_ABS(deg);
  *minute=(int)A_ABS(min);
  *second=(int)A_ABS(sec);
}

void aodtv72_msg(int value,int intin,float floatin1,float floatin2,char *charin,char *retstring)
/* Output information message and continue program.
*/
{
  char *strng,*fcststrng;
  char  cuse[6][25]={ "FORECAST INTERPOLATION","LAPLACIAN ANALYSIS",
                      "WARMEST PIXEL SEARCH","SPIRAL ANALYSIS",
                      "RING/SPIRAL COMBINATION","LINEAR EXTRAPOLATION" };

  fcststrng=(char *)calloc((size_t)2000,sizeof(char));
  strng=(char *)calloc((size_t)5000,sizeof(char));

  switch(value) {
    case -1 : sprintf(strng,"Error reading from existing history file %s\n",charin);
             break;
    case -2 : sprintf(strng,"Error creating history file %s\n",charin);
             break;
    case -3 : sprintf(strng,"Error writing to history file %s\n",charin);
             break;
    case -4 : sprintf(strng,"Error finding time in history file %s\n",charin);
             break;
    case -11: sprintf(strng,"Error reading image file %s\n",charin);
             break;
    case -12: sprintf(strng,"Error accessing image file %s\n",charin);
             break;
    case -13: sprintf(strng,"Bad navigation in image file %s\n",charin);
             break;
    case -14: sprintf(strng,"Line/Element mismatch error in image file %s\n",charin);
             break;
    case -15: sprintf(strng,"Multiple bands in image file %s\n",charin);
             break;
    case -16: sprintf(strng,"Latitude/Longitude conversion error in image file %s\n",charin);
             break;
    case -17: sprintf(strng,"Data read off edge of image in image file %s\n",charin);
             break;
    case -21: sprintf(strng,"Invalid storm center location... point not on planet\n");
             break;
    case -22: sprintf(strng,"Error setting up navigation with cursor position\n");
             break;
    case -23: sprintf(strng,"Bad navigation in image file %s\n",charin);
             break;
    case -31: sprintf(strng,"Error accessing topography file %s\n",charin);
             break;
    case -32: sprintf(strng,"Error reading topography file %s\n",charin);
             break;
    case -33: sprintf(strng,"Invalid topography flag value \n");
             break;
    case -41: sprintf(strng,"Fourier Transforma Analysis failure\n");
             break;
    case -43: sprintf(strng,"Error accessing forecast file %s\nError with autopositioning...\nFORECAST INTERPOLATION and LINEAR EXTRAPOLATION failed\n",charin);
             break;
    case -44: sprintf(strng,"Invalid forecast file type %s\nError with autopositioning...\nFORECAST INTERPOLATION and LINEAR EXTRAPOLATION failed\n",charin);
             break;
    case -45: sprintf(strng,"Error reading forecast file %s\nError with autopositioning...\nFORECAST INTERPOLATION and LINEAR EXTRAPOLATION failed\n",charin);
             break;
    case -46: sprintf(strng,"Error with forecast interpolation\nError with autopositioning...\nFORECAST INTERPOLATION and LINEAR EXTRAPOLATION failed\n");
             break;
    case -51: sprintf(strng,"Error with temperature value(s)\n");
             break;
    case -61: sprintf(strng,"Error with START or END date... \nCheck history file name or DATE keyword parameters");
             break;
    case -71: sprintf(strng,"Error writing output ASCII file %s\n",charin);
             break;
    case -72: sprintf(strng,"Error writing output ATCF file %s\n",charin);
             break;
    case -81: sprintf(strng,"Error determining ocean basin\n");
             break;
    case -91: sprintf(strng,"Error inidializing McIDAS environment\n");
             break;
    case -92: sprintf(strng,"Error with mouse button entry during scene override\n");
             break;
    case -99: sprintf(strng,"Exiting ADT\n");
             break;

    case 1 : sprintf(strng,"Performing abbreviated ADT analysis\n");
             break;
    case 2 : sprintf(strng,"Performing full ADT analysis\n");
             break;
    case 11: sprintf(strng,"Utilizing history file %s\n",charin);
             break;
    case 12: sprintf(strng,"Utilizing image data file %s\n",charin);
             break;
    case 13: sprintf(strng,"History file listing written to ASCII file %s\n",charin);
             break;
    case 14: sprintf(strng,"Utilizing forecast file %s\n",charin);
             break;
    case 15: sprintf(strng,"Utilizing SST GRIB file %s\n",charin);
             break;
    case 16: sprintf(strng,"Utilizing topography file %s\n",charin);
             break;
    case 21: sprintf(strng,"Using USER-SELECTED center position  LAT: %6.2f LON: %7.2f\n",floatin1,floatin2);
             break;
    case 22: sprintf(strng,"Using AUTO-SELECTED center position -- LAT: %6.2f LON: %7.2f\n",floatin1,floatin2);
             break;
    case 31: sprintf(strng,"User accepted scene type\n");
             break;
    case 32: sprintf(strng,"User modified scene type\n");
             break;
    case 42: aodtv72_listforecast(fcststrng);
             strcat(retstring,fcststrng);
	     sprintf(strng,"FORECAST INTERPOLATION successful -- LAT: %6.2f LON: %7.2f \n",floatin1,floatin2);
             break;
    case 43: sprintf(strng,"Error accessing forecast file %s\nFORECAST INTERPOLATION failed\nLINEAR EXTRAPOLATION successful -- LAT: %6.2f LON: %7.2f \n",charin,floatin1,floatin2);
             break;
    case 44: sprintf(strng,"Invalid forecast file type %s\nFORECAST INTERPOLATION failed\nLINEAR EXTRAPOLATION successful -- LAT: %6.2f LON: %7.2f \n",charin,floatin1,floatin2);
             break;
    case 45: sprintf(strng,"Error reading forecast file %s\nFORECAST INTERPOLATION failed\nLINEAR EXTRAPOLATION successful -- LAT: %6.2f LON: %7.2f \n",charin,floatin1,floatin2);
             break;
    case 46: sprintf(strng,"Error with forecast interpolation\nFORECAST INTERPOLATION failed\nLINEAR EXTRAPOLATION successful -- LAT: %6.2f LON: %7.2f \n",floatin1,floatin2);
             break;
    case 51: sprintf(strng,"Will utilize %s position -- LAT: %6.2f LON: %7.2f \n",cuse[intin-1],floatin1,floatin2);
             break;
    case 61: sprintf(strng,"OVERWROTE RECORD %d IN HISTORY FILE %s\n",intin,charin);
             break;
    case 62: sprintf(strng,"INSERTING RECORD %d IN HISTORY FILE %s\n",intin,charin);
             break;
    case 63: sprintf(strng,"ADDED RECORD TO EMPTY HISTORY FILE %s\n",charin);
             break;
    case 64: sprintf(strng,"ADDED RECORD TO END OF HISTORY FILE %s\n",charin);
             break;
    case 65: sprintf(strng,"MODIFIED %d SUBSEQUENT RECORDS IN HISTORY FILE %s\n",intin,charin);
             break;
    case 66: sprintf(strng,"DELETED %d RECORD(S) IN HISTORY FILE %s\n",intin,charin);
             break;
    case 67: sprintf(strng,"WROTE %d RECORD(S) TO HISTORY FILE %s\n",intin,charin);
             break;
    case 68: sprintf(strng,"ADDED COMMENT FOR RECORD %d IN HISTORY FILE %s\n",intin,charin);
             break;
    case 71: sprintf(strng,"Tropical cyclone is over land\n");
             break;
    case 72: sprintf(strng,"Ocean basin automatically selected as %s\n",cbasin_v72[intin]);
             break;
    case 73: sprintf(strng,"Ocean basin manually selected as %s\n",cbasin_v72[intin]);
             break;
    case 81: sprintf(strng,"SST value of %4.1fC at cursor location\n",floatin1);
             break;
    case 91: sprintf(strng,"Warmest pixel temperature of %5.1fC at LAT: %6.2f LON: %7.2f\n",(float)intin/10.0,floatin1,floatin2);
             break;
    case 101: sprintf(strng,"Successfully completed listing\n");
             break;
    case 102: sprintf(strng,"Successfully completed graph\n");
             break;
    case 103: sprintf(strng,"Successfully completed record deletion\n");
             break;
    case 104: sprintf(strng,"Successfully completed ADT analysis\n");
             break;
    case 105: sprintf(strng,"WROTE ATCF OUTPUT TO FILE %s\n",charin);
             break;
/*
    case 111: sprintf(strng,"Error accessing SST GRIB file - No TIE Model estimate available\n");
             break;
    case 112: sprintf(strng,"Memory error while reading SST GRIB file - No TIE Model estimate available\n");
             break;
    case 113: sprintf(strng,"Corrupted SST GRIB file - No TIE Model estimate available\n");
             break;
    case 114: sprintf(strng,"Error reading SST GRIB file - No TIE Model estimate available\n");
             break;
    case 115: sprintf(strng,"Invalid SST value - No TIE Model estimate available\n");
             break;
*/
    case 121: sprintf(strng,"OVER and AUTO keywords cannot be used at same time\nDisabling override ability\n");
             break;
    case 500: sprintf(strng,"%s",charin);
	     break;
    default: sprintf(strng,"Invalid Information Code %d\n",value);
             break;
  }

  strcat(retstring,strng);
  retstring[strlen(retstring)]='\0';

  free(strng); strng=NULL;
  free(fcststrng); fcststrng=NULL;

}

void aodtv72_listforecast(char *retstrng)
{
  int iy,idate,imon,iyear,ixtime;
  char *cdate,*fcstout,*iout2;
  float timex,timex2,timex3;
  double curtime;

  cdate=(char *)calloc((size_t)30,sizeof(char));
  fcstout=(char *)calloc((size_t)1000,sizeof(char));
  iout2=(char *)calloc((size_t)1000,sizeof(char));

  /* list current time */
  curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);
  aodtv72_yddmy(curtime,&idate,&imon,&iyear);
  sprintf(iout2,"CURRENT ANALYSIS TIME : %2.2d/%3s/%4d  %4.4dUTC\n",idate,cmon_v72[imon-1],iyear,odtcurrent_v72->IR.time/100);strcat(fcstout,iout2);

  /* list forecast times used for interpolation */
  sprintf(iout2,"        DATE       TIME      LAT       LON\n");strcat(fcstout,iout2);
  iy=0;
  while((iy<5)&&(fcsttime_v72[iy]<9999999.0)) {
    /* convert Julian date/time to day/month/year format */
    aodtv72_yddmy(fcsttime_v72[iy],&idate,&imon,&iyear);
    timex=fcsttime_v72[iy]-(float)(int)fcsttime_v72[iy];
    timex2=(timex*24.);
    timex3=60.0*(timex2-(float)(int)timex2);
    ixtime=((int)timex2*100)+(int)timex3;
    sprintf(cdate,"%2.2d/%3s/%4d  %4.4dUTC",idate,cmon_v72[imon-1],iyear,ixtime);
    cdate[strlen(cdate)]='\0';
    sprintf(iout2,"T%1d  %17s  %7.2f   %7.2f \n",iy,cdate,fcstlat_v72[iy],fcstlon_v72[iy]);strcat(fcstout,iout2);
    iy++;
  }
  strcat(retstrng,fcstout);
  retstrng[strlen(retstrng)]='\0';

  free(cdate); cdate=NULL;
  free(fcstout); fcstout=NULL;
  free(iout2); iout2=NULL;

}
