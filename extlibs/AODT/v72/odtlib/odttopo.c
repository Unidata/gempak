/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"

int aodtv72_readtopo( char *,float,float,int * );
int aodtv72_readhres(char *,float,float,int *,int *);
                                                                                
int aodtv72_readtopo( char *topofile, float mlat, float mlon, int *ichar)
/* Determine land/water flag for data point using low resolution
   topograpy file TOPOLRES.
    Inputs  : mlat  - latitude of data point in question
              mlon  - longitude of data point in question
    Outputs : ichar - <0=error,1=land,2=water
    Return  : -31 : error accessing topography file
              -32 : error reading topography file
               71 : cyclone is over land
                0 : cyclone is over water
*/
{
  int  b,ixx,iyy,iok,value,elev,iret;
  int  lat,lon,ival,count=0;
  int  ilatB,ilatE,ilonB,ilonE;
  float xlat,xlon;
 
  xlat=10.0*mlat;
  xlon=10.0*mlon;
  ilatB=(int)xlat+5;
  ilatE=(int)xlat-5;
  ilonB=(int)xlon+5;
  ilonE=(int)xlon-5;

  for(lat=ilatB;lat>ilatE;lat--) {
   iyy=900-lat;
    xlat=10.0*(float)lat;
     for(lon=ilonB;lon>ilonE;lon--) {
      ixx=1800-lon;
      xlon=10.0*(float)lon;
      iok=aodtv72_readhres(topofile,xlat,xlon,&value,&elev);
      if(iok<0) return iok;
      ival=(value>3||elev>0) ? 1 : 0;
      count=count+ival;
    }
  }
  *ichar=2;
  iret=0;
  if(count>85) {
    *ichar=1;
    iret=71;
  }

  return iret;
}

int aodtv72_readhres(char *topofile,float dlat,float dlon,int *ichar,int *ielev)
{
 FILE *fdi;
 int mel=511;
 int izz,i,error,ilat,jlat,ilon,ib,ii,iel;
 float xlat,xlon,fel;
 short buf[144];
 long irec,ioff;

 ioff=0;
 if(dlat<=0) ioff=8100;
 xlat=abs(dlat);
 xlon=dlon;
 if(xlon<0) xlon=xlon+36000.0;
 ilat=6*(int)(xlat/100.0);
 jlat=(((int)xlat%100)*6)/100;
 ilon=(int)xlon/2400;
 irec=(long)(sizeof(buf)*(((ilat+jlat)*15)+ilon));
 irec=irec+(ioff*sizeof(buf));

 fdi = fopen(topofile, "rb") ;
 if (fdi == NULL) {
   /* failed to open topography file */
   return -31;
 }
 error=fseek (fdi,irec,SEEK_SET ) ;
 if(error!=0) {
   /* error reading topography file */
   return -32;
 }
 i=fread(&buf,sizeof(buf),1,fdi);

 ib=((((int)xlon%2400)*6)/100);
 ii=buf[ib];
 iel=ii & mel;
 if(iel>=400) {
   iel=iel-400;
   iel=-iel;
 }
 fel=30.48*(float)iel;
 *ielev=(int)fel;
 *ichar=ii/512;

 fclose(fdi);

  return 0;
}
