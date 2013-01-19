/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

/*int aodtv72_readtopo( int,int,int * );*/
int   aodtv72_gettemps( int );
float aodtv72_calceyetemp( int, float, float *, float * ); 
float aodtv72_calccloudtemp( int * );
void  aodtv72_readcirc( int, int );

int aodtv72_gettemps( int keyer )
/* Determine eye and coldest-warmest cloud temperatures.
    Inputs  : keyer      - eye radius (km)
    Outputs : odtcurrent_v72 - structure containing current image information
    Return  : -51 : eye, cloud, or warmest temperature <-100C or >+40C
*/
{

  int cursorx,cursory;
  int iok,idist,iret;
  float cursortemp,eyet,cloudt;
  float cursorlat,cursorlon,warmlat,warmlon,eyeangle,eyedist;

  /* calculate cursorx/cursory from numx/numy... values should be 0.5*numx/y */
  cursorx=areadata_v72->numx/2;
  cursory=areadata_v72->numy/2;
  cursortemp=areadata_v72->temp[cursory][cursorx];
  cursorlat=areadata_v72->lat[cursory][cursorx];
  cursorlon=areadata_v72->lon[cursory][cursorx];

  tcircfirst_v72=(struct ringdata *)calloc(1,sizeof(struct ringdata));
  /* load array containing data on rings around center location */
  aodtv72_readcirc(cursorx,cursory);

  iret=0;
  /* compute eye/warmest pixel temperature */ 
  eyet=aodtv72_calceyetemp(keyer,cursortemp,&eyeangle,&eyedist);
  if((eyet<-100.0)||(eyet>40.0)) iret=-51;

  if(keyer==keyerA_v72) {
    /* set warmest pixel temperature */
    odtcurrent_v72->IR.warmt=eyet;
    /* calculate warmest pixel location */
    aodtv72_distance2( cursorlat, cursorlon, eyedist, eyeangle, &warmlat, &warmlon);
    odtcurrent_v72->IR.warmlatitude=warmlat;
    odtcurrent_v72->IR.warmlongitude=warmlon;
    /* store forecast location temperature in eyet */
    odtcurrent_v72->IR.eyet=cursortemp-273.16;
    free(tcircfirst_v72);
  } else {
    /* set eye temperature */
    odtcurrent_v72->IR.eyet=eyet;
    /* set cloud temperature and ring distance */
    cloudt=aodtv72_calccloudtemp(&idist);
    if((eyet<-100.0)||(eyet>40.0)) iret=-51;
    odtcurrent_v72->IR.cwcloudt=cloudt;
    odtcurrent_v72->IR.cwring=idist;
  }

  return iret;
}

float aodtv72_calceyetemp(int keyer, float cursortemp,float *eyeangle, float *eyedist)
/* Determine eye/warmest temperature by examining the satellite
   data between 0 and 24/75 km from the storm center location.
   Eye temperature will be warmest temperature found.
    Inputs  : keyer      - analysis region radius
              cursortemp - temperature of pixel at cursor location
    Outputs : eyeangle   - angle to warmest temperature in eye region (if found)
              eyedist    - distance to warmest temperature in eye region (if found)
    Return  : return value is warmest eye temperature
*/
{
  float eyetemp;
  struct ringdata *tcirc;

  /* set eye temp to cursor location temperature */
  eyetemp=cursortemp;
  
  *eyedist=0.0;
  *eyeangle=0.0;
  tcirc=tcircfirst_v72;
  while(tcirc!=0) {
    if(tcirc->dist<=(float)keyer) {
      if(tcirc->temp>eyetemp) {
        eyetemp=tcirc->temp;
        *eyedist=tcirc->dist;
        *eyeangle=tcirc->angle;
      }
    }
    tcirc=tcirc->nextrec;
  }

  /* convert temperature to C from K */
  eyetemp=(eyetemp-273.16);

  return eyetemp;
}

float aodtv72_calccloudtemp(int *irdist)
/* Determine surrounding cloud top region temperature by
   examining the satellite data between kstart_v72(24 km) and
   kend_v72 (136 km) from the storm center location.  Cloud
   temperature will be the coldest value of an array
   of warmest ring temperatures (4 km ring sizes for 4 km
   infrared data).  This is the "coldest-warmest" pixel.
    Inputs  : none
    Outputs : irdist - distance (km) from center to cloud top
                       temperature value (distance to ring)
              return value is cloud top temperature value
*/
{
  int iyy,b;
  int numrings=(kend_v72-kstart_v72)/kres_v72,kring;
  /* float maxtemp[200]; */
  float *maxtemp;
  float cloudtemp;
  struct ringdata *tcirc;
  
  cloudtemp=10000.0;

  b=sizeof(float);
  maxtemp=(float *)calloc((size_t)numrings,b);

  /* initialize maxtemp array */
  for(iyy=0;iyy<numrings;iyy++) {
    maxtemp[iyy]=-10000.0;
  }

  tcirc=tcircfirst_v72;
  while(tcirc!=0) {
    if((tcirc->dist>=(float)kstart_v72)&&(tcirc->dist<(float)kend_v72)) {
      kring=((int)tcirc->dist-kstart_v72)/kres_v72;
      if(tcirc->temp>maxtemp[kring]) maxtemp[kring]=tcirc->temp;
    }
    tcirc=tcirc->nextrec;
  }

  /* search maxtemp array for coldest temperature */
  for(iyy=0;iyy<numrings;iyy++) {
    if((maxtemp[iyy]<cloudtemp)&&(maxtemp[iyy]>160.0)) {
      cloudtemp=maxtemp[iyy];
      *irdist=(iyy*kres_v72)+kstart_v72;
    }
  }
  cloudtemp=(cloudtemp-273.16);
  free(maxtemp);

  return cloudtemp;
}

void aodtv72_readcirc(int ixc,int iyc)
/* Read array of satellite data temperatures and load array
   containing temperatures and lat/lon positions.  
    Inputs  : ixc   - element location of center point
              iyc   - line location of center point
    Outputs : global structure tcirc containing temperature/locations
*/
{
  int ixx,iyy,b,count=0;
  float xclat,xclon,xlat,xlon,xdist,xangle;
  struct ringdata *tcirc;

  b=sizeof(struct ringdata);
  /* obtain center/cursor x,y position */
  xclat=areadata_v72->lat[iyc][ixc];
  xclon=areadata_v72->lon[iyc][ixc];

  /* load tcirc array with distance, angle, and temp info */
  for(iyy=0;iyy<areadata_v72->numy;iyy++) {
    for(ixx=0;ixx<areadata_v72->numx;ixx++) {
      xlat=areadata_v72->lat[iyy][ixx];
      xlon=areadata_v72->lon[iyy][ixx];
      aodtv72_distance(xlat,xlon,xclat,xclon,1,&xdist,&xangle);
      if(xdist<=(float)(kend_v72+80)) {    /* add 80.0 to allow for correct calculation of annulus temp */
        if(count==0) {
          tcirc=tcircfirst_v72;
        } else {
          tcirc->nextrec=(struct ringdata *)calloc((size_t)1,b);
          tcirc=tcirc->nextrec;
        } 
        count++;
        tcirc->dist=xdist;
        tcirc->angle=xangle;
        tcirc->temp=areadata_v72->temp[iyy][ixx];
        tcirc->nextrec=NULL; /* make pointer for last record equal to 0 */
      }
    }
  }
}
