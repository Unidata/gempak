#include "inc/odt.h"

int gettemps( float [maxd][maxd],float [maxd][maxd],float [maxd][maxd],int,int,struct odtdata * );
float calceyetemp( float ); 
float calccloudtemp( int * ); 
void readcirc( int,int,int,int,float [maxd][maxd],float [maxd][maxd],float [maxd][maxd] );
void distance( float,float,float,float,int,float *,float * );
int readtopo( int,int,int * );

/* extern global variables */
extern int kstart,kend,keyer,kres;
extern struct ringdata *tcircfirst;
extern char iout[200];

extern void xprintf(char *);

int gettemps(float temps[maxd][maxd],float lats[maxd][maxd],float lons[maxd][maxd],int numx,int numy,
              struct odtdata *odtcurrent)
/* Determine eye and cloud temperatures.
    Inputs  : temps      - array containing temperature data
              lats       - array containing latitude positions
              lons       - array containing longitude positions
              numx       - number of elements in arrays
              numy       - number of lines in arrays
    Outputs : odtcurrent - structure containing current image information
*/
{

  int cursorx,cursory,lastdist=0;
  int xp[5]={2,-2,0,0,0};
  int yp[5]={0,0,2,-2,0};
  int ixcv,iycv,iloop;
  int iok,idist,ielev;
  float lastcloudt=10000.0F;
  float cursortemp,eyet,cloudt;

  /* calculate cursorx/cursory from numx/numy... values should be 0.5*numx/y */
  cursorx=numx/2;
  cursory=numy/2;
  cursortemp=temps[cursory][cursorx];

  /* The following loop will calculate the best (coldest) surrounding
     ring temperature by looking at cursor position value and four
     values around the cursor position (two pixels from center
     at 0, 90, 180 and 270 degrees).  This should help remove
     some of the variance in results due to cursor placement.
     Please note, the final read is at the initial cursor position,
     thus it will be used in the calculation of the statistics, and
     used as the "current position" of the storm... NOT the location
     of the storm where the coldest cloud top ring temperature is
     located! */
  for(iloop=0;iloop<5;iloop++) {
    ixcv=cursorx+xp[iloop];
    iycv=cursory+yp[iloop];

    /* load array of satellite temperature around ixcv,iycv */
    readcirc(ixcv,iycv,numx,numy,temps,lats,lons);

    /* compute cloud temperature */
    cloudt=calccloudtemp(&idist);

    if(cloudt<lastcloudt) {
      lastcloudt=cloudt;
      lastdist=idist;
    }

  }

  /* set cloud temperature and ring distance */
  odtcurrent->IR.cloudt=lastcloudt;
  odtcurrent->IR.ringdist=lastdist;

  /* set latitude/longitude */
  odtcurrent->IR.latitude=lats[cursory][cursorx];
  odtcurrent->IR.longitude=lons[cursory][cursorx];

  /* compute/set eye temperature */ 

  eyet=calceyetemp(cursortemp);
  odtcurrent->IR.eyet=eyet;

  /* determine land/sea flag */

  iok=readtopo((int)odtcurrent->IR.latitude,(int)odtcurrent->IR.longitude,&ielev);
  if(iok<0) {
    return iok;
  }
  odtcurrent->IR.land=iok;
  if(odtcurrent->IR.land==1) {
    sprintf(iout,"\n\n***** TROPICAL CYCLONE IS OVER LAND *****\n\n");
    xprintf(iout);
  }

  return 0;
}

float calceyetemp(float cursortemp)
/* Determine eye temperature by examining the satellite
   data between 0 and 40 km from the storm center location.
   Eye temperature will be warmest temperature found.
    Inputs  : cursortemp - temperature of pixel at cursor location
    Outputs : return value is warmest eye temperature
*/
{
  float eyetemp;
  struct ringdata *tcirc;

  /* set eye temp to cursor location temperature */
  eyetemp=cursortemp;
  
  tcirc=tcircfirst;
  while(tcirc!=0) {
    if(tcirc->dist<=(float)keyer) {
      if(tcirc->temp>eyetemp) eyetemp=tcirc->temp;
    }
    tcirc=tcirc->nextrec;
  }

  /* convert temperature to C from K */
  eyetemp=(eyetemp-273.16F);

  return eyetemp;
}

float calccloudtemp(int *irdist)
/* Determine surrounding cloud top region temperature by
   examining the satellite data between kstart(24 km) and
   kend (136 km) from the storm center location.  Cloud
   temperature will be the coldest value of an array
   of warmest ring temperatures (4 km ring sizes for 4 km
   infrared data).  This is the "coldest-warmest" pixel.
    Inputs  : none
    Outputs : irdist - distance (km) from center to cloud top 
                       temperature value (distance to ring)
              return value is cloud top temperature value
*/
{
  int iyy;
  int numrings=(kend-kstart)/kres,kring;
  float maxtemp[200];
  float cloudtemp;
  struct ringdata *tcirc;
  
  cloudtemp=10000.0F;

  /* initialize maxtemp array */
  for(iyy=0;iyy<numrings;iyy++) {
    maxtemp[iyy]=-10000.0F;
  }

  tcirc=tcircfirst;
  while(tcirc!=0) {
    if((tcirc->dist>=(float)kstart)&&(tcirc->dist<=(float)kend)) {
      kring=((int)tcirc->dist-kstart)/kres;
      if(tcirc->temp>maxtemp[kring]) maxtemp[kring]=tcirc->temp;
    }
    tcirc=tcirc->nextrec;
  }

  /* search maxtemp array for coldest temperature */
  for(iyy=0;iyy<numrings;iyy++) {
    if(maxtemp[iyy]<cloudtemp) {
      cloudtemp=maxtemp[iyy];
      *irdist=(iyy*kres)+kstart;
    }
  }
  cloudtemp=(cloudtemp-273.16F);

  return cloudtemp;
}

void readcirc(int ixc,int iyc,int numx,int numy,
             float temps[maxd][maxd],float lats[maxd][maxd],float lons[maxd][maxd])
/* Read array of satellite data temperatures and load array
   containing temperatures and lat/lon positions.  
    Inputs  : ixc   - element location of center point
              iyc   - line location of center point
              numx  - maximum number of elements in arrays
              numy  - maximum number of lines in arrays
              temps - array containing temperature values
              lats  - array containing latitude values
              lons  - array containing longitude values
    Outputs : global structure tcirc containing temperature/locations
*/
{
  int ixx,iyy,count=0;
  float xclat,xclon,xlat,xlon,xdist,xangle;
  struct ringdata *tcirc;

  /* obtain center/cursor x,y position */
  xclat=lats[iyc][ixc];
  xclon=lons[iyc][ixc];

  /* load tcirc array with distance, angle, and temp info */
  for(iyy=0;iyy<numy;iyy++) {
    for(ixx=0;ixx<numx;ixx++) {
      xlat=lats[iyy][ixx];
      xlon=lons[iyy][ixx];
      distance(xlat,xlon,xclat,xclon,1,&xdist,&xangle);
      if(xdist<=(float)kend) {
        if(count==0) {
          tcirc=tcircfirst;
        } else {
          tcirc->nextrec=(struct ringdata *)malloc(sizeof(struct ringdata));
          tcirc=tcirc->nextrec;
        } 
        count++;
        tcirc->dist=xdist;
        tcirc->angle=xangle;
        tcirc->temp=temps[iyy][ixx];
        tcirc->nextrec=0; /* make pointer for last record equal to 0 */
      }
    }
  }
}

void distance(float rrlat,float rrlon,float pplat,float pplon,int iunit,
              float *dist,float *ang)
/* Calculate distance and angle between two points 
   (rrlat,rrlon and pplat,pplon).
   Inputs  : rrlat - latitude of starting point
             rrlon - longitude of starting point
             rrlat - latitude of ending point
             rrlon - longitude of ending point
             iunit - flag for output unit type (1-km,2-mi,3-nmi)
   Outputs : dist  - distance between two points
             ang   - angle between two points (0=north)
*/
{
  float z=0.017453292F,r=6371.0F;
  float rlat,rlon,plat,plon;
  float crlat,crlon,srlat,srlon;
  float cplat,cplon,splat,splon;
  float xx,yy,zz,idist,xdist,xang;

  rlat=rrlat*z;
  rlon=rrlon*z;
  plat=pplat*z;
  plon=pplon*z;
  crlat=COS(rlat);
  crlon=COS(rlon);
  srlat=SIN(rlat);
  srlon=SIN(rlon);
  cplat=COS(plat);
  cplon=COS(plon);
  splat=SIN(plat);
  splon=SIN(plon);
  xx=(cplat*cplon)-(crlat*crlon);
  yy=(cplat*splon)-(crlat*srlon);
  zz=splat-srlat;
  idist=SQRT((xx*xx)+(yy*yy)+(zz*zz));
  /* xdist is distance in kilometers */
  xdist=(float)(2.0*ASIN((double)idist/2.0)*(double)r);

  if(iunit==2) xdist=((69.0F*xdist)+55.0F)/111.0F;  /* conversion to miles */
  if(iunit==3) xdist=((60.0F*xdist)+55.0F)/111.0F;  /* conversion to nautical miles */
  *dist=xdist;

  xang=0.0F;
  if((float)ABS(xdist)>0.0001F) xang=(float)((SIN((double)(rlon-plon))*
				SIN((3.14159/2.0)-plat))/SIN((double)xdist));
  if(ABS(xang)>1) xang=SIGN(1.000,xang);
  xang=(float)(ASIN(xang)/(double)z);
  if(plat<rlat) xang=180.0F-xang;
  if(xang<0.0) xang=360.0F+xang;
  *ang=xang;
}

int readtopo(int mlat, int mlon, int *ielev)
/* Determine land/water flag for data point using low resolution
   topograpy file TOPOHRES.  
    Inputs  : mlat  - latitude of data point in question
              mlon  - longitude of data point in question
    Outputs : ielev - elevation at location
              return value : <0=error,1=land,2=water 
*/
{
  FILE *fdi;
  int mel=511;
  int i,ichar,error,ilat,jlat,ilon,ib,ii,iel,lat,lon;
  float fel;
  short buf[144];
  long irec,ioff;
  char topofile[200]="\0";

  mlat=mlat*100;
  mlon=mlon*100;
  ioff=0;
  if(mlat<=0) ioff=8100;
  lat=abs(mlat);
  lon=mlon;
  if(lon<0) lon=lon+36000;

  ilat=6*(lat/100);
  jlat=((lat%100)*6)/100;
  ilon=lon/2400;
  irec=(long) (sizeof(buf)*(unsigned int)(((ilat+jlat)*15)+ilon));
  irec=irec+(ioff*(long)sizeof(buf));

  /* determine topography file name and path */
  strncpy(topofile,TOPOPATH,strlen(TOPOPATH));
  strncat(topofile,"TOPOHRES",8);
  fdi = fopen(topofile, "rb") ;

  if (fdi == NULL)
  {
    sprintf(iout,"FAILED TO OPEN TOPOLRES.\n");
    xprintf(iout);
    *ielev=-10;
    return -1;
  }
  error=fseek (fdi,irec,SEEK_SET ) ;
  if(error!=0)
  {
    sprintf(iout,"HRTOPO: FSEEK ERROR\n");
    xprintf(iout);
    *ielev=-20;
    return -2;
  }
  i=(int)fread(&buf,sizeof(buf),1,fdi);
  if(i==0) return -3;
  fclose(fdi);

  ib=((lon%2400)*6)/100;
  ii=(int)buf[ib];
  iel=ii & mel;
  if(iel>=400)
  {
    iel=iel-400;
    iel=-iel;
  }
  fel=30.48F*(float)iel;
  *ielev=(int)fel;
  ichar=(ii/512==0)?2:1;
  return ichar;
}
