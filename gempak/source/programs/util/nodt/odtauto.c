#include "inc/odt.h"

/* extern global variables */
extern int kstart,keyer,kres;
extern int ifixtype;
extern char fixfile[200];
extern struct odtdata *odthistoryfirst;
extern char iout[200];

/* external routines */
extern float slopecal( struct odtdata *,double,int );
extern void xprintf(char *);
extern double calctime( int, int );
extern void distance( float, float, float, float, int, float *, float * );
extern void coakley( float [maxd][maxd], float [maxd][maxd], float [maxd][maxd], 
                     int, int, float, float, int, int, 
                     float [4][4], int isav[5] );

/* internal routines */
void odtautomode1( int, int, float *, float *, int * );
void odtautomode2( struct odtdata *, 
                   float [maxd][maxd], float [maxd][maxd] ,float [maxd][maxd], int, int,
                   int * ,float * ,float * );
int readforecast( FILE *, double );
void distance2( float, float, float, float, float *, float * );
int xint( double *, double, float *, int, int, int, float * );
void picklocation( float, float, float, float, int,
                   float, float, float, float, int,
                   float, float, int, float, float, float,
                   float *, float *, int * );
void logspiral( float, float, float *, float *, int, float *, float *, int * );
void laplacian( float [maxd][maxd], int, int, float [maxd][maxd] );
float atoif( char *, int, int );
void calcskew( float *, int, float *, float *, float * );
int idmyyd( int,int,int );
void yddmy( int,int *,int *,int * );

/* global variables for internal routines */
float flat[5],flon[5];
double ftimex[5];

void odtautomode1( int indate, int intime,float *retlat,float *retlon,int *retuse )
/* Determine storm position at time curtime using NHC/JTWC 
   forecast discussion products.  Time and location information
   from these products are then interpolated to time in question
   to derive a estimated storm position.  If position estimation
   cannot be calculated, a lat/lon position of -99,99/-99.99 will
   be returned.
    Inputs  : indate  - current image date
              intime  - current image time
    Outputs : retlat  - estimated latitude position
              retlon  - estimated longitude position
              retuse  - identification value for method used
                         to derive estimated storm location
                         0-error
                         1-quadratic interpolation
                         2-Laplacian analysis
                         3-10^ log spiral analysis
                         4-linear extrapolation from prior locations
*/
{
  FILE *fp;
  int iok,ioklat,ioklon,usethis;
  float telat,telon;
  logical getextrap=FALSE;
  double curtime;
  struct odtdata *odtdummy;

  sprintf(iout,"\n*** Utilizing automatic center finding algorithm ***\n\n");
  xprintf(iout);

  curtime=calctime(indate,intime);

  /* get file information */
  fp=fopen(fixfile,"r+");
  if(fp==0) {
    sprintf(iout,"ERROR OPENING FORECAST FIX FILE %s\n",fixfile);
    xprintf(iout);
    *retlat=-99.99F;
    *retlon=-99.99F;
    *retuse=0;
    getextrap=TRUE;
  } else {
    /* Get forecast positions from NHC (or other) forecast file, if avaialble.
       Otherwise, use linear interpolation of previous locations.
       Will use as "first guess" for Laplacian and Log Spiral analysis. */
    iok=readforecast(fp,curtime);
    if(iok!=0) {
      sprintf(iout,"WILL TRY PREVIOUS POSITION EXTRAPOLATION\n");
      xprintf(iout);
      *retlat=-99.99F;
      *retlon=-99.99F;
      *retuse=0;
      getextrap=TRUE;
    } else {
      /* Call Quadratic Interpolation procedure.  Routine is called 
         for both latitude and longitude interpolations.  If quadratic
         interpolation fails, will recall routine to obtain linear 
         interpolation position */
      ioklat=xint(ftimex,curtime,flat,5,1,1,&telat);
      if(ioklat==3) ioklat=xint(ftimex,curtime,flat,5,1,0,&telat);
      ioklon=xint(ftimex,curtime,flon,5,1,1,&telon);
      if(ioklon==3) ioklon=xint(ftimex,curtime,flon,5,1,0,&telon);
  
      if((ioklat==0)&&(ioklon==0)) {
        /* Good interpolated values... use 'em */
        usethis=1;
        getextrap=FALSE;
        sprintf(iout,"Interpolated forecast position : LAT=%7.2f LON=%7.2f\n",telat,telon);
        xprintf(iout);
      } else {
        /* Bad interpolated values... try extrapolated values */
        getextrap=TRUE;
      }
    }
  } 

  /* try to extrapolate storm location from previous storm locations in history file */
  if(getextrap) {
    /* call slopecal to get y-intercept values for lat/lon values */
    odtdummy=(struct odtdata *)malloc(sizeof(struct odtdata));
    odtdummy->IR.date=indate;
    odtdummy->IR.time=intime;
    telat=slopecal(odtdummy,12.0,2);
    telon=slopecal(odtdummy,12.0,3);
    free(odtdummy);
    if((telat>999.0F)&&(telon>999.0F)) {
      sprintf(iout,"Autopositioning failed... must use USER INPUT\n");
      xprintf(iout);
      usethis=0;
    } else {
      sprintf(iout,"Linear extrapolated position : LAT=%7.2f LON=%7.2f\n",telat,telon);
      xprintf(iout);
      usethis=4;
    }
  }
  
  *retlat=telat;
  *retlon=telon;
  *retuse=usethis;

}

void odtautomode2(struct odtdata *odtcurrent,
                  float temps[maxd][maxd],float lats[maxd][maxd],float lons[maxd][maxd],int numx,int numy,
                  int *retuse,float *retlat,float *retlon)
/* Additional automatic positioning of storm center location 
   using official forecasts from NHC or JTWC as input.  Storm
   location will be estimated using Laplacian analysis (looking
   for large gradients in temperature field, i.e. the eye) or
   an automated 10^ Log Spiral analysis (utilizes Coakely-
   Breatherton analysis to determine cloud temperatures on which
   log spiral is positioned/analyzed).  The final position will
   be determined utilizing various empirically defined confidence
   factors for each method.  The final storm position will be 
   returned along with a position determination flag.
    Inputs  : odtcurrent - structure containing current analysis parameters
              temps      - array of satellite image temperature values
              lats       - array of satellite image latitude positions
              lons       - array of satellite image longitude positions
              numx       - number of elements in temps/lats/lons arrays
              numy       - number of lines in temps/lats/lons arrays
    Outputs : retuse     - flag for center location method utilized
              retlat     - center location latitude
              retlon     - center location longitude
*/
{ 
  int ix,iy,ic1=0,ic2=0,ic3=0;
  int searchrad2=75;
  int threshold1=10,threshold2=20;
  int isav[5],logcnt,usethis;
  float xxrad[4][4],lapvals[maxd][maxd];
  float xdist,xangle,xlatf,xlonf;
  float loglat,loglon,uselat,uselon;
  float avelat1,stdvlat1,skewlat1,avelon1,stdvlon1,skewlon1;
  float avelat2,stdvlat2,skewlat2,avelon2,stdvlon2,skewlon2;
  float templat1[maxd*maxd],templon1[maxd*maxd];
  float templat2[maxd*maxd],templon2[maxd*maxd];
  float templat3[maxd*maxd],templon3[maxd*maxd];
  double curtime;
  char  cuse[4][25]={ "QUADRATIC INTERPOLATION","LAPLACIAN ANALYSIS",
                      "10^ LOG SPIRAL ANALYSIS","LINEAR EXTRAPOLATION" };
  
  /* determine current image time */
  curtime=calctime(odtcurrent->IR.date,odtcurrent->IR.time);

  /* compute Laplacian of scene */
  laplacian(temps,numx,numy,lapvals);

  /* obtain center location of temps array for calculation of distances */
  xlatf=lats[numy/2][numx/2];
  xlonf=lons[numy/2][numx/2];

  /* call Coakley-Breatherton analysis routine to
     determine temperature ranges for 10-degree 
     log spiral analysis */
  coakley(temps,lats,lons,numx,numy,xlatf,xlonf,0,searchrad2,xxrad,isav);
  if(xxrad[3][0]==0.0F) xxrad[3][0]=xxrad[1][0];  /* replace warm with total */
  if(xxrad[0][0]==0.0F) xxrad[0][0]=xxrad[3][0];  /* replace cold with filtered */

  /* check Laplacian values within search radii and set up analysis arrays */
  for(iy=0;iy<numy;iy++) {
    for(ix=0;ix<numx;ix++) {
      distance(xlatf,xlonf,lats[iy][ix],lons[iy][ix],1,&xdist,&xangle);
      if(xdist<=(float)searchrad2) {
        if((lapvals[iy][ix]<(float)threshold1)&&(temps[iy][ix]>=(10.0F*xxrad[3][2]))) {
          templat1[ic1]=lats[iy][ix];
          templon1[ic1]=lons[iy][ix];
          ic1++;
        }
        if((lapvals[iy][ix]<(float)threshold2)&&(temps[iy][ix]>=(10.0F*xxrad[3][2]))) {
          templat2[ic2]=lats[iy][ix];
          templon2[ic2]=lons[iy][ix];
          ic2++;
        }
      }
      if(temps[iy][ix]<=(10.0F*xxrad[0][0])) {
        templat3[ic3]=lats[iy][ix];
        templon3[ic3]=lons[iy][ix];
        ic3++;
      }
    }
  }

  /* determine lat/lon using 10^ log spiral analysis.
     return lat/lon point and number of consecutive arcs
     covered by spiral (logcnt=degrees covered/10) */
  logspiral(xlatf,xlonf,templat3,templon3,ic3,&loglat,&loglon,&logcnt);

  /* determine stdv and skew of Laplacian analysis for
     two different thresholds.  Will try and pinpoint
     eye/warm spot regions from these values */
  if(ic1>0) {
    calcskew(templat1,ic1,&avelat1,&stdvlat1,&skewlat1);
    calcskew(templon1,ic1,&avelon1,&stdvlon1,&skewlon1);
  }
  if(ic2>0) {
    calcskew(templat2,ic2,&avelat2,&stdvlat2,&skewlat2);
    calcskew(templon2,ic2,&avelon2,&stdvlon2,&skewlon2);
  }

  usethis=*retuse;
  picklocation(avelat1,avelon1,stdvlat1,stdvlon1,ic1,
               avelat2,avelon2,stdvlat2,stdvlon2,ic2,
               loglat,loglon,logcnt,
               xlatf,xlonf,(float)curtime,
               &uselat,&uselon,&usethis);
  sprintf(iout,"\nUtilizing %s position : LAT=%7.2f LON=%7.2f\n\n",cuse[usethis-1],uselat,uselon);
  xprintf(iout);
  *retlat=uselat;
  *retlon=uselon;
  *retuse=usethis;

}


void distance2(float rlat,float rlon,float xdist,float xang,
               float *plat,float *plon)
/* Calculate a latitude and longitude position from an 
   initial latitude/longitude and distance/angle values.
    Inputs  : rlat - initial latitude
              rlon - initial longitude
              dist - distance from initial position
              ang  - angle from initial position
    Outputs : plat - derived latitude
              plon - derived longitude
*/
{
  float z=0.017453292F,z90=1.570797F;
  float clat,clon,cltv,cdis,cang;
  float qlat,qlon,argu,tv;
  int iang;

  clat=(90.0F-rlat)*z;
  cltv=clat;
  clon=rlon*z;
  if(rlat<0.0F) {
    cltv=-(90.0F+rlat)*z;
    clon=(rlon-180.0F)*z;
  }
  iang=(int)xang;
  cang=-(float)((float)((540-iang)%360)*z);
  cdis=(xdist/111.1F)*z;
  qlat=ACOS((COS(clat)*COS(cdis))+(SIN(clat)*SIN(cdis)*COS(cang)));
  if((double)ABS(qlat)<0.0000001) {
    qlon=0.0F;
  } else {
    argu=(SIN(cdis)*SIN(cang))/SIN(qlat);
    if(ABS(argu)>1) argu=(float)SIGN(1.0F,argu);
    qlon=ASIN(argu);
    tv=ATAN(SIN(z90-cang))/TAN(z90-cdis);
    if(tv>cltv) qlon=(2.0F*z90)-qlon;
  }
  qlon=clon-qlon;
  *plat=90.0F-(qlat/z);
  *plon=(float)((int)(10000*(qlon/z))%3600000)/10000.0F;

}

int readforecast( FILE *fp,double curtime )
/* Read the forecast file and obtain the forecast positions
   for interpolation of the storm position
    Inputs  : fp      - file pointer
              curtime - current time for interpolation
    Outputs : return value : -1 for error reading forecast file
                              0 for good read of forecast file
              global variables : ftimex, flat, flon
                                 arrays containing previous, current,
                                 and forecast times/positions for use
                                 in quadratic interpolation scheme
                                 [0] - 24 hour old time/postion
                                 [1] - 12 hour old time/postion
                                 [2] - current time/postion
                                 [3] - forecast time/postion
                                 [4] - forecast time/postion
*/
{
  char line[100];
  char l1[10],l2[10],l3[10],l4[10],l5[10],l6[10],l7[10],l1a[10];
  int iz=0,maxline=100,ixx=0,iy=0,islot1=0,islot2=0;
  int imon,imonx,iyear,iyearx,idate,idatex,julday,juldayx,itime;
  float lat[6],lon[6];
  double time[6],searchtime,finit,xtime;
  logical getmonth=FALSE;
  struct odtdata *odthistory;
  char month[12][4]={ "JAN","FEB","MAR","APR","MAY","JUN",
                      "JUL","AUG","SEP","OCT","NOV","DEC" };

  if(ifixtype==0) {  /* NHC */
    /* read NHC forecast file, quit at EOF */
    while(((fgets(line,maxline,fp))!=NULL)&&(ixx<6)) {
      if(ixx==0) {
        (void)sscanf(line,"%s %s %s %s %s %s %s",l1,l2,l3,l4,l5,l6,l7);
      } else {
        (void)sscanf(line,"%s %s %s %s %s %s %s",l1,l1a,l2,l3,l4,l5,l6);
      }
      if((!getmonth)&&((!strncmp(l2,"AM",2))||(!strncmp(l2,"PM",2)))) {
        /* this is reading the header at the top of the forecast file.
           it is done to obtain the current month and year of the forecast
           so that we can check it accurately against the image date/time */
        imonx=0;
        idatex=(int)atoif(l6,1,2);
        iyearx=(int)atoif(l7,1,4);
        while((strncmp(l5,month[imonx],3)!=0)&&(imonx<12)) { imonx++; }
        juldayx=idmyyd(idatex,imonx+1,iyearx);
        getmonth=TRUE;
      }
      imon=imonx;
      iyear=iyearx;
      if(!strncmp(l6,"KTS",3)) {
        idate=(int)atoif(l2,1,7)/10000;
        itime=((int)atoif(l2,1,7)%10000)*100;
        if(idate<idatex) {
          imon++;
          if(imon==12) {
            iyear++;
            imon=0;
          }
          julday=idmyyd(idate,imon+1,iyear);
        } else {
          julday=juldayx+(idate-idatex);
        }
        time[ixx]=calctime(julday,itime);
        lat[ixx]=atoif(l3,1,6);
        lon[ixx]=atoif(l4,1,6);
        ixx++;
      }
    }
    sprintf(iout,"Done reading NHC forecast file\n");
    xprintf(iout);
  } else if(ifixtype==1) {
    /* read JTWC forecast file, quit at EOF */
    /* o.k... since JTWC does not put the month and year
       on their bulletins, they will need to be "made up".
       We will assume that the forecast is current and the
       month and year from the image is equal to the month
       and year of the forecast file.  */
    (void)yddmy((int)curtime,&idatex,&imonx,&iyearx);
    while(((fgets(line,maxline,fp))!=NULL)&&(ixx<6)) {
      (void)sscanf(line,"%s %s %s %s %s",l1,l2,l3,l4,l5);
      if(!strncmp(l2,"---",3)) {
        if(ixx==0) {
          idate=(int)atoif(l1,1,6)/10000;
          itime=((int)atoif(l1,1,6)%10000)*100;
          if(idate==idatex) {
            /* dates are the same... probably o.k.... should check times */
            julday=(int)curtime;
          } else if((idatex-idate)==1) {
            /* this is probably o.k too... forecast date is before image by one day */
            julday=(int)curtime+(idate-idatex);
          } else {
            /* dates are invalid.  Either image date is before forecast or
               is well beyond forecast availability */
            sprintf(iout,"Invalid forecast file\n");
            xprintf(iout);
            return -1;
          } 
          time[ixx]=calctime(julday,itime);
          lat[ixx]=atoif(l4,1,8);
          lon[ixx]=atoif(l5,1,8);
        } else {
          idate=(int)atoif(l1,1,6)/10000;
          itime=((int)atoif(l1,1,6)%10000)*100;
          julday=(int)curtime+(idate-idatex);
          time[ixx]=calctime(julday,itime);
          lat[ixx]=atoif(l3,1,8);
          lon[ixx]=atoif(l4,1,8);
        }
      }
      if((!strncmp(l1,"MAX",3))&&(!strncmp(l4,"-",1))) {
        ixx++;
      }
    }
    sprintf(iout,"Done reading JTWC forecast file\n");
    xprintf(iout);
    ixx--;
  } else {
    sprintf(iout,"ERROR READING FORECAST FILE\n");
    xprintf(iout);
    return -1;
  }

  while((time[iy]<curtime)&&(iy<ixx)) {
    iy++;
  }

  islot1=iy-1;
  if((iy==0)||(iy==ixx)) {
    sprintf(iout,"INVALID FORECAST FILE, IMAGE BEFORE/AFTER FORECAST TIMES\n");
    xprintf(iout);
    return -1;
  }
  while((iz<3)&&((islot1+iz)<6)) {
    /* load forecast times into return arrays */
    ftimex[iz+2]=time[islot1+iz];
    flat[iz+2]=lat[islot1+iz];
    flon[iz+2]=lon[islot1+iz];
    iz++;
  }
  if(iz==2) {
    /* could only obtain one forecast; copy into second forecast slot */
    ftimex[4]=ftimex[3];
    flat[4]=flat[3];
    flon[4]=flon[3];
  }

  /* search history file for 24 and 12 hour old position */
  odthistory=odthistoryfirst;
  finit=ftimex[2];
  searchtime=finit-1.00;   /* 24 hour old forecast */
  while(odthistory!=0) {
    xtime=calctime(odthistory->IR.date,odthistory->IR.time);
    if((xtime>=searchtime)&&(xtime<finit)) {
      ftimex[islot2]=xtime;
      flat[islot2]=odthistory->IR.latitude;
      flon[islot2]=odthistory->IR.longitude;
      islot2++;
      searchtime=searchtime+0.50;  /* 12 hour old position */
    }
    odthistory=odthistory->nextrec;
  }

  /* no times found in history file, use current time/location values */
  if(islot2==0) {
    ftimex[0]=ftimex[2];
    ftimex[1]=ftimex[2];
    flat[0]=flat[2];
    flat[1]=flat[2];
    flon[0]=flon[2];
    flon[1]=flon[2];
  }

  /* only one time period between forecast time and history file start */
  if(islot2==1) {
    ftimex[1]=ftimex[0];
    flat[1]=flat[0];
    flon[1]=flon[0];
  }

/*
  for(iy=0;iy<5;iy++) {
    sprintf(iout,"%d  ftimex=%f flat=%f flon=%f\n",iy,ftimex[iy],flat[iy],flon[iy]);
    xprintf(iout);
  }
*/

  return 0;
}

int xint( double x[5], double xi, float f[5], int n, int iflag, int lflag, float *fi )
/* Perform quadratic interpolation proceedure.
   Routine obtained from Mark DeMaria while at NHC/TPC.

   This routine applies a quadratic interpolation procedure
   to f(x) between x(1) and x(n). f(x) is assumed to be
   represented by quadratic polynomials between the points
   x(i). The polynomials are chosen so that they equal f(i)
   at the points x(i), the first derviatives on either
   side of the interior x(i) match at x(i), and the second
   derivative of the approximated function integrated
   over the domain is minimized.
   Inputs  : x(1),x(2) ... x(n) - The x values (must be sequential)
             f(1),f(2) ... f(n) - The function values
             n                  - The number of x,f pairs
             iflag              - Flag for initialization
                                   =1 for coefficient calculation
                                   =0 to use previous coefficients
             lflag              - Flag for linear interpolation
                                   =0 to perform linear interpolation
                                   =1 to perform quadratic interpolation
             xi                 - The x value at which to interpolate f
   Outputs : fi                 - The interpolated function value
             ierr               - Error flag
                                   =0  Normal return
                                   =1  Parameter nmax is too small
                                   =2  The x values are not sequential
                                   =3  Coefficient iteration did not
                                       converge
                                   =4  Mix-up finding coefficients
                                   =5  if xi .gt. x(n) or .lt. x(1),
                                       xi is set to nearest endpoint
                                       before the interpolation
 
   Note: fi is set to -999.99 if ierr=1,2,3 or 4
*/
{
  int i,ii,k,nit=0,nmax=30;
  double thresh=0.000001,eps=0.0000000001;
  double d,bb,aa,dev,cf0,rel,dsdc1,cft,slt,den,slo;
  double cfsave;
  double ax[30],bx[30],cx[30],dx[30],df[30],gm[30],ct[30];
  logical iterate=TRUE,quad=TRUE,converged=FALSE;

  /* check to make sure nmax is large enough and n is greater than 1 */
  if((n>nmax)&&(n<2)) {
    *fi=-999.99F;
    return 1;
  }

  if(iflag==1) {
    /* perform initialization for later interpolation */
    for(i=0;i<n-1;i++) {
      if(x[i]>x[i+1]) {
        *fi=-999.99F;
        return 2;
      }
    }

    /* check for special case where n=2.  only 
       linear interpolation is possible in this case */ 
    if(n==2) {
      cx[0]=0.0;
      bx[1]=(double)(f[1]-f[0])/(x[1]-x[0]);
      ax[0]=(double)f[0]-(bx[0]*x[0]);
      quad=FALSE;
    }

    if(quad) {
      /* perform quadratic interpolation instead of linear */
  
      /* calculate x and f differences */
      for(i=0;i<n-1;i++) {
        df[i]=(double)(f[i+1]-f[i]);
        dx[i]=(x[i+1]-x[i]);
      }
  
      /* calculate domain size */ 
      d=x[n-1]-x[0];
  
      /* check for linearity of input points */
      bb=(double)(f[1]-f[0])/(x[1]-x[0]);
      aa=(double)f[0]-(bb*x[0]);
      dev=0.0;
      for(i=2;i<n;i++) {
        dev=dev+(double)ABS(aa+(bb*((double)x[i]-(double)f[i])));
      }
  
      if((dev<eps)||(lflag==0)) {
        for(i=0;i<n-1;i++) {
          cx[i]=0.0;
        }
        iterate=FALSE;
      }
  
      if(iterate) {
        /* iterate to find the c-coefficients */
        cx[0]=0.0;
        nit=100;
        slt=0.01;
        cfsave=10000000000.0;
        
        for(k=0;k<nit;k++) {
          /* calculate c values */
          if(!converged) {
            for(i=1;i<n-1;i++) {
              cx[i]= -(cx[i-1]*dx[i-1]/dx[i])-
                      (df[i-1]/(dx[i]*dx[i-1]))+
                      (df[i]/(dx[i]*dx[i]));
            }
    
            /* calculate current value of cost function */
            cf0=0.0;
            for(i=0;i<n-1;i++) {
              cf0=cf0+(cx[i]*cx[i]*dx[i]);
            }
            cf0=(0.5*cf0)/d;
    
            /* check for convergence */
            rel=(double)(ABS(cf0-cfsave)/ABS(cfsave));
            if(rel<thresh) {
              converged=TRUE;
            } else {
              cfsave=cf0;
    
              /* calculate values of LaGrange multipliers */
              gm[n-1]=(cx[n-1]*dx[n-1])/d;
      
              if(n>3) {
                for(i=n-3;i>0;i--) {
                  gm[i]=((cx[i]*dx[i])/d)-((gm[i+1]*dx[i])/dx[i+1]);
                }
              }
      
              /* calculate gradient of cost function with respect to c1 */ 
              dsdc1=(dx[0]*(cx[0]/d))-(gm[1]/dx[1]);
      
              /* adjust cx[0] using trial step */
              ct[0]=cx[0]-slt*dsdc1;
      
              /* calculate remaining c values at trial step */
              for(i=1;i<n-1;i++) {
                ct[i] = -(ct[i-1]*dx[i-1]/dx[i])-
                         (df[i-1]/(dx[i]*dx[i-1]))+
                         (df[i  ]/(dx[i]*dx[i  ]));
              }
      
              /* calculate cost function at trial step */
              cft=0.0;
              for(i=0;i<n-1;i++) {
                cft=cft+(ct[i]*ct[i]*dx[i]);
              }
              cft=(0.5*cft)/d;
      
              /* calculate optimal step length and re-adjust cx[0] */
              den=(2.0*(cft*cf0))+(slt*dsdc1*dsdc1);
              slo=0.0;
              if(den!=0.0) slo=(dsdc1*dsdc1*slt*slt)/den;
      
              cx[0]=cx[0]-(slo*dsdc1);
      
              /* calculate trial step for next time step */
              slt=0.5*slo;
            } /* if-else */
          } /* if converged */
        } /* for loop */
    
        /* iteration did not converge */
        if(!converged) {
          *fi=-999.99F;
          return 3;
        }
      } /* if iterate */
  
      /* ITERATION CONVERGED */
      /* calculate b and a coeffinents */
      for(i=0;i<n-1;i++) {
        bx[i]=(df[i]/dx[i])-(cx[i]*(x[i+1]+x[i]));
        ax[i]=(double)f[i]-(bx[i]*x[i])-(cx[i]*x[i]*x[i]);
      }
    } /* if quad */
  
  } /* if iflag */ 

  /* interpolate the function */
  /* check for xi out of bounds */
  if(xi<x[0]) {
    xi=x[0];
    return 5;
  }
  if(xi>x[n-1]) {
    xi=x[n-1];
    return 5;
  }

  /* find the interval for the interpolation */
  ii=0;
  for(i=1;i<n;i++) {
    if(xi<=x[i]) {
      ii=i-1;
      *fi=(float)(ax[ii]+(bx[ii]*xi)+(cx[ii]*xi*xi));
      return 0;
    }
  }

  *fi=-999.90F;
  return 4;
  
}

void picklocation(float lplat1,float lplon1,float lplats1,float lplons1,int lpnum1,
                  float lplat2,float lplon2,float lplats2,float lplons2,int lpnum2,
                  float splatc,float splonc,int spnumc,
                  float nhclat,float nhclon,float xtime,
                  float *uselat,float *uselon,int *usethis)
/* Determine method location scheme to use by examining
   various empirically-defined confidence factors.  Laplacian
   analysis, 10^ Log Spiral analysis, and NHC/JTWC forecast
   interpolation analysis confidence factors will be derived,
   with the "most confident" value used as the final automatically
   determined storm position.
    Inputs  : lplat1  - Laplacian latitude position (threshold 1)
              lplon1  - Laplacian longitude position (threshold 1)
              lplats1 - Laplacian latitude values std. dev. value (threshold 1)
              lplats1 - Laplacian longitude values std. dev. value (threshold 1)
              lpnum1  - number of points with Laplacian values less than threshold 1
              lplat2  - Laplacian latitude position (threshold 2)
              lplon2  - Laplacian longitude position (threshold 2)
              lplats2 - Laplacian latitude values std. dev. value (threshold 2)
              lplats2 - Laplacian longitude values std. dev. value (threshold 2)
              lpnum2  - number of points with Laplacian values less than threshold 2
              splatc  - 10^ Log Spiral latitude position
              splonc  - 10^ Log Spiral longitude position
              spnumc  - number of points on 10^ Log Sprial
              nhclat  - NHC/JTWC interpolated latitude position
              nhclon  - NHC/JTWC interpolated longitude position
              xtime   - time of current image analysis
    Outputs : uselat  - latitude value to be used
              uselon  - longitude value to be used
              usethis - method utilized in determination of storm postion values
                         1-quadratic interpolation
                         2-Laplacian analysis
                         3-10^ log spiral analysis
                         4-linear extrapolation from prior locations
*/
{
  int conflapl,confsprl,confnhcfg;
  float scalelat1,scalelon1,scalelat2,scalelon2;
  float lapval1,lapval2,maxval1,maxval2,anhc;

  /* Determine the confidence factor for Laplacian lat/lon location.
     "1" values are Laplacian values using first Laplacian threshold value
     "2" values are Laplacian values using second Laplacian threshold value */
  if(lpnum1>=3) {
    /* THERE ARE POINTS MEETING FIRST THRESHOLD
       scalelat and scalelon are basically the maximum number of
       data points available within a certain box around the center
       point.  The stdv is used as the "box size radius", with
       maxval as the theoretical maximum number of points within
       this box.  There might be more than maxval number of points
       obtained by the Laplacian routine because I am only using
       the stdv values and not the absolute ranges of the Laplacian
       threshold points... however, this gives a good estimate as
       to how dense (scatter) the Laplacian points are in the scene. */
    scalelat1=(2.0F*lplats1)*(110.0F/(float)kres);
    scalelon1=(2.0F*lplons1)*(110.0F/(float)kres);
    maxval1=scalelat1*scalelon1;
    lapval1=0.0F;
    if(maxval1>0.0F) {
      lapval1=(float)lpnum1/maxval1;
      if(lapval1>1.0F) lapval1=1.0F;
    }
    if(lapval1>=0.5F) {
      if(lpnum2>=2) {
        /* probably use the Laplacian... most likely an eye */
        scalelat2=(2.0F*lplats2)*(110.0F/(float)kres);
        scalelon2=(2.0F*lplons2)*(110.0F/(float)kres);
        maxval2=scalelat2*scalelon2;
        lapval2=0.0F;
        if(maxval2>0.0F) {
          lapval2=(float)lpnum2/maxval2;
          if(lapval2>1.0F) lapval2=1.0F;
        }
        conflapl=100;
      } else {
        /* maybe use the Laplacian... could be an embedded center */
        lapval2=0.0F;
        conflapl=70;
      }
    } else {
      /* maybe use the Laplacian... could be a very small eye */
      if(lpnum2>=2) {
        lapval2=0.0F;
        conflapl=80;
      } else {
        lapval2=0.0F;
        conflapl=50;
      }
    }
  } else {
    lapval1=0.0F;
    lapval2=0.0F;
    conflapl=0;
  }

  /* Determine the confidence factor for the 10degree log spiral lat/lon location
     Basically, this is just a function of the number of consecutive 10degree
     arcs covered by the log spiral.  Values >=14 are usually less than
     30km error, with values >=18 usually less than 20km error */
  if(spnumc>9) {
    confsprl=10*(spnumc-9);
    confsprl=MIN(90,confsprl);
  } else {
    confsprl=0;
  }

  if(*usethis==1) {
    /* Determine the confidence factor of the NHC forecast lat/lon location
       This value is a function of time from the forecast time, since the
       distance error increases with time (~30km at 0hours,~50km at 6hours) */
    anhc=(float)(((double)xtime-ftimex[2])/0.25);
    confnhcfg=85-(int)(20.0F*anhc);
  } else {
    confnhcfg=50;
  }

  /* Determine which lat/lon point to use by comparing the confidence
     factors for each lat/lon set (Forecast, Laplacian, Log Spiral).
     Generally, if a Laplacian max exists, it will be used, with the Log
     Spiral points used as a backup.  The forecast points will be used
     mostly if the time difference between the current time and the
     forecast time is less than 4 hours. */
  if(confnhcfg>=conflapl) {
    if(confnhcfg>=confsprl) {
      *uselat=nhclat;
      *uselon=nhclon;
      *usethis=*usethis;
    } else {
      *uselat=splatc;
      *uselon=splonc;
      *usethis=3;
    }
  } else {
    if(conflapl>=confsprl) {
      *uselat=lplat1;
      *uselon=lplon1;
      *usethis=2;
    } else {
      *uselat=splatc;
      *uselon=splonc;
      *usethis=3;
    }
  }

}

void logspiral(float lat,float lon,float *xin,float *yin,int np,
               float *bestlat,float *bestlon,int *bestspiral)
/* Determine storm location using 10^ Log-spiral analysis.
   Algorithm will attempt to match the spiral with the image
   pixels at or below the threshold temperature determined
   using Coakley-Breatherton analysis.
    Inputs  : lat     - center latitude of analysis grid
              lon     - center longitude of analysis grid
              xin     - array containing latitude positions of pixels
              yin     - array containing longitude positions of pixels
              np      - total number of points/pixels in arrays
    Outputs : bestlat - best latitude location from analysis
              bestlon - best longitude location from analysis
              bestspiral - number of consecutive arcs through which spiral passes
*/
{
  int ixx,iyy,izz,iskip,rotfac,theta;
  int imaxx,iminx,imaxy,iminy,ycount;
  int spiralconsec,maxconsec,spirallast;
  float xrad=57.29578F,A=25.0F,B=10.0F/xrad;
  float maxx,minx,maxy,miny;
  float glat,glon,xlat,xlon,xdist,xangle;
  float xlatdiff,xlondiff,ylatdiff,ylondiff;
  float thetax,r,thetaval,thetaval2;

  sprintf(iout,"Performing 10^ Log Spiral Analysis\n");
  xprintf(iout);
  /* define analysis box centered on input lat/lon position */
  maxx=lat+0.5F;
  minx=lat-0.5F;
  maxy=lon+0.5F;
  miny=lon-0.5F;
  imaxx=(int)(maxx*10.0F);
  iminx=(int)(minx*10.0F);
  imaxy=(int)(maxy*10.0F);
  iminy=(int)(miny*10.0F);
  *bestspiral=0;

  /* loop through x-axis/elements of analysis grid box */
  for(ixx=iminx;ixx<=imaxx;ixx++) {
    xlat=(float)ixx/10.0F;

    /* loop through y-axis/lines of analysis grid box */
    for(iyy=iminy;iyy<=imaxy;iyy++) {
      xlon=(float)iyy/10.0F;
      iskip=0;

      /* determine distance from each point in box to current location */
      for(izz=0;izz<np;izz++) {
        xlatdiff=(float)ABS((float)(xlat-xin[izz]));
        xlondiff=(float)ABS((float)(xlon-yin[izz]));
        if((xlatdiff<=0.20F)&&(xlondiff<=0.20F)) {
          /* if the lat/lon point is too close to cold cloud tops, do
             not calculate log spiral at this point.  Trying to eliminate
             "false" arc locations by forcing the system to use some
             of the arc points on the spiral away from the start of
             the spiral (were getting "false echos" without this". */
          iskip=1;
          break;
        }
      }

      /* if arc location passes analysis above, proceed with placement of spiral */
      if(iskip==0) {

        /* rotate the arc spiral thru entire revolution at 30 degree intervals */
        for(rotfac=0;rotfac<=330;rotfac=rotfac+30) {
          spiralconsec=1;
          maxconsec=0;
          spirallast=1;

          /* calculate position of each point on spiral from 0 to 540^ */
          for(theta=0;theta<=540;theta=theta+18) {
            thetax=(float)theta/xrad;
            r=A*EXP((B*thetax));
            thetaval=(float)theta+(float)rotfac;
            thetaval2=thetaval+180.0F;
            distance2(xlat,xlon,r,thetaval2,&glat,&glon);
            ycount=0;
            for(izz=0;izz<np;izz++) {
              ylatdiff= (float)ABS((float)(glat-xin[izz]));
              ylondiff= (float)ABS(glon-yin[izz]);
              /* if a point is within 0.1^ latitude/longitude determine distance */
              if((ylatdiff<=0.1F)&&(ylondiff<=0.1F)) {
                distance(glat,glon,xin[izz],yin[izz],1,&xdist,&xangle);
                /* if distance from spiral point is within 6km from an accepted
                   temperature threshold point, count it */
                if(xdist<=6.0F) ycount++;
              }
            }
            /* if there are 4 or more threshold points associated with each 
               spiral point, count within consecutive spiral point counter */
            if(ycount>=4) {
              if(spirallast==1) {
                spiralconsec++;
                /* save spiral that has the maximum consecutive spiral counts 
                   for each rotation though 360^ at each center location */
                if(spiralconsec>maxconsec) maxconsec=spiralconsec;
              }
              spirallast=1;
            } else {
              spiralconsec=1;
              spirallast=0;
            }
            /* if this spiral has the greatest number of consecutive spiral
               points, save the location and number of points */
            if(maxconsec>*bestspiral) {
              *bestspiral=maxconsec;
              *bestlat=xlat;
              *bestlon=xlon;
            }
          }
        } /* rotfac loop */
      } /* iskip if */ 
    } /* iyy loop */
  } /* ixx loop */

}

void laplacian(float data1[maxd][maxd],int maxx,int maxy,float data2[maxd][maxd])
/* Compute Laplacian values for scene.
   Derived from program laplacian.c by John Gauch and Edu Metz
   of the University of Kansas (1994).
    Inputs  : data1 - array containing temperature values of scene
              maxx  - maximum number of elements in data1 array
              maxy  - maximum number of lines in data1 array
    Outputs : data2 - array containing Laplacian values for each point
*/
{
  int x,y;
  float dxx,dyy;

  sprintf(iout,"Performing Laplacian Analysis\n");
  xprintf(iout);
  for(y=1;y<maxy-1;y++) {
    for(x=1;x<maxx-1;x++) {
/*  dx and dy not used 
      dx = (data1[y+1][x+1] + (2.0F*data1[y][x+1]) +
            data1[y-1][x+1] - data1[y+1][x-1] +
            (2.0F*data1[y][x-1]) - data1[y-1][x-1])/8.0;
      dy = (data1[y+1][x+1] + (2.0F*data1[y+1][x]) +
            data1[y+1][x-1] - data1[y-1][x+1] +
            (2.0F*data1[y-1][x]) - data1[y-1][x-1])/8.0;
*/
      dxx = (data1[y+1][x+1] + (4.0F*data1[y][x+1]) +
             data1[y-1][x+1] - (2.0F*data1[y+1][x]) -
             (8.0F*data1[y][x]) - (2.0F*data1[y-1][x]) +
             data1[y+1][x-1] + (4.0F*data1[y][x-1]) +
             data1[y-1][x-1])/6.0F;
      dyy = (data1[y+1][x+1] - (2.0F*data1[y][x+1]) +
             data1[y-1][x+1] + (4.0F*data1[y+1][x]) -
             (8.0F*data1[y][x]) + (4.0F*data1[y-1][x]) +
             data1[y+1][x-1] - (2.0F*data1[y][x-1]) +
             data1[y-1][x-1])/6.0F;
      /* calculate directional derivative */
      /* data3[y][x]=(int)(SQRT((dx*dx)+(dy*dy))); */
      data2[y][x]=dxx+dyy;
    }
  }

  /* handle boundary rows and columns */
  for(x=0;x<maxx;x++) {
    data2[0][x]=data2[1][x];
    data2[maxy-1][x]=data2[maxy-2][x];
  }
  for(y=0;y<maxy;y++) {
    data2[y][0]=data2[y][1];
    data2[y][maxx-1]=data2[y][maxx-2];
  }

}

float atoif(char *ptr,int beg,int end)
/* routine to convert from character to floating point 
    Inputs  : ptr  - pointer to beginning of character string
              beg  - beginning character to convert
              end  - ending character to convert
    Outputs : return value is converted floating point value
*/
{
  int ixx, ichar, mul=1;
  float atoif, fact=1.0F, value=0.0F, denom=1.0F;

  for(ixx=end-1;ixx>=beg-1;ixx--) {
    if((ptr[ixx]=='S')||(ptr[ixx]=='E')) mul=-1;
    ichar=(int)(ptr[ixx])-48;
    if((ichar>=0)&&(ichar<=9)) {
      value=value+((float)ichar*fact);
      fact=fact*10.0F;
    } else if(ichar==-2) {
      denom=fact;
    } else {
      continue;
    }
  }

  atoif = (float)mul*(value/denom);
  return (atoif);

}

void calcskew( float *bin, int nbin, float *ave, float *stdv, float *skew )
/* Calculate average, standard deviation, and skew for a given data set.
    Inputs  : bin  - data array
              nbin - number of points in data array
    Outputs : ave  - average value in array
              stdv - standard deviation of data array
              skew - skew of data array histogram
*/
{
  int ixx;
  float xave,xstdv,xskew;
  float a2sum=0.0F,a3sum=0.0F,nsum=0.0F;

  for(ixx=0;ixx<nbin;ixx++) {
    nsum=nsum+bin[ixx];
  }
  /* compute average value of data array */
  xave=nsum/(float)nbin;

  for(ixx=0;ixx<nbin;ixx++) {
    a2sum=a2sum+((bin[ixx]-xave)*(bin[ixx]-xave));
    a3sum=a3sum+((bin[ixx]-xave)*(bin[ixx]-xave)*(bin[ixx]-xave));
  }
  /* calculate standard deviation of data array */
  xstdv = (float)sqrt((1.0/((double)nbin-1.0))*(double)a2sum);

  /* calculate skew of data array */
  xskew=((1.0F/((float)nbin-1.0F))*a3sum)/(xstdv*xstdv*xstdv);

  /* return desired values */
  *ave=xave;
  *stdv=xstdv;
  *skew=xskew;

}
int idmyyd(int day,int mon,int year)
/* Convert dd/mm/yy to yyyyddd format.
   this routine was originally taken from McIDAS
   program idmyyd.for.
   Inputs  : day   - day
             month - month (integer)
             year  - year (YY or YYYY)
   Outputs : return value is Julian date or 0 (bad input data)
*/
{
  int mtbl[12]={0,31,59,90,120,151,181,212,243,273,304,334};
  int julday=0,iday;

  /* perform a couple quality checks for day/month */
  if(day<1||day>31) return julday;
  if(mon<1||mon>12) return julday;

  iday=day+mtbl[mon-1];
  if(year<1900) {
    if(year>70) {
      year=1900+year;
    } else {
      year=2000+year;
    }
  }
  /* Leap year check */
  if(((year%4)==0)&&(mon>2)) iday=iday+1;
  julday=(year*1000)+iday;

  return julday;
}

void yddmy(int syd,int *day,int *mon,int *year)
/* Convert yyyyddd to dd/mm/yy format.
   Inputs  : syd   - Julian day (yyyyddd)
   Outputs : day   - date
             month - month
             year  - year (yyyy)
*/
{
  int dn[2][13]={ {0,31,59,90,120,151,181,212,243,273,304,334,365},
                  {0,31,60,91,121,152,182,213,244,274,305,335,366} };
  int iyy,idd,imm,ily=0;

  iyy=syd/1000;
  if(iyy<1900) {
    if(iyy>70) {
      iyy=iyy+1900;
    } else {
      iyy=iyy+2000;
    }
  }
  idd=(syd%1000);
  if((iyy%4)==0) ily=1;
  for(imm=0;imm<13;imm++) {
    if(idd<=dn[ily][imm]) break;
  }
  *mon=imm;
  *day=idd-dn[ily][imm-1];
  *year=iyy;

}
