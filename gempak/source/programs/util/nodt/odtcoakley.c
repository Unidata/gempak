#include "inc/odt.h"

extern char iout[200];

/* routines defined in this program */
void coakley( float [maxd][maxd], float [maxd][maxd], float [maxd][maxd], 
              int, int, float, float, int, int,
              float [4][4], int isav[5] );
void MNANSD( float [maxd][maxd], int, int, int, float *, float *, int * );
void HWORK( float *, int, int *, int * );
void FIT2( int *, int, float, float, int, int, int, int *, float *, 
           float *, float * );
void TPGAUS( int, int, float *, float, float, float * );
void CURVE( int, float, float, int, float, float, float, int * );
float fgauss( float, float, float, float );
void HISTM( float *, int, int, int, int *, int *, float *, float * );
void HISTD( float *, int, int, int, float *, float * );
int ihistl( float, float, float );
void SMOOFT( float *, int, float );
void REALFT( float *, int, int );
void FOUR1( float *, int, int );
void XAVG( float *, int, float *, float *, float *, float * );

extern void xprintf(char *);
extern void distance( float,float,float,float,int,float *,float * );

void coakley(float temps[maxd][maxd],float lats[maxd][maxd],float lons[maxd][maxd],
             int numx,int numy,float xlatf,float xlonf,int rin,int rout,
             float xxrad[4][4],int isav[5])
/* Perform Coakley-Breatherton analysis on scene.  C-B analysis analyzes
   3X3 squares and computs its standard deviation and mean temperature.  By
   identifying groups of 3X3 boxes with different mean temp. vs. stdv. 
   characteristics, warm and cold clusters can be identified.  This analysis
   is primarily used for identifying cloud contamination within a FOV.
   The ODT will use the different cloud clusters to objectively identify
   the coldest and warmest scene temperature clusters for analysis within
   the 10^ Log Spiral routine.
    Inputs  : temps  - array of temperature values for total FOV box
              lats   - array of latitude values for total FOV box
              lons   - array of longitude values for total FOV box
              numx   - number of elements in total FOV box
              numy   - number of lines in total FOV box
              xlatf  - center latitide of total FOV box
              xlonf  - center longitide of total FOV box
              rin    - inner radius for analysis (data subset)
              rout   - outer radius for analysis (data subset)
    Outputs : xxrad  - array containing cluster analysis parameters
                           row 0 : cold cluster sample values
                           row 1 : total sample values
                           row 2 : filtered sample values
                           row 3 : warm cluster sample values
                        column 0 : average temperature of sample
                        column 1 : standard deviation of sample
                        column 2 : minimum value of sample
                        column 3 : maximum value of sample
*/
{
  int   i,ix,iy,np,lside=3,idx=0,kno=0,jjnum=0,kknum=0;
  int   jslot[bufsiz],kslot[bufsiz];
  float valth=10.0F;
  float xm[bufsiz],xs[bufsiz],xdist,xangle;
  float xmf[bufsiz],xcf[bufsiz],xwf[bufsiz];
  float ihfov[maxd][maxd],xarray[bufsiz];
  
  sprintf(iout,"Performing Coakley-Breatherton Analysis\n");
  xprintf(iout);
  for(iy=0;iy<numy;iy++) {
    for(ix=0;ix<numx;ix++) {
      ihfov[iy][ix]=0.0F;
    }
  }
  for(iy=0;iy<numy;iy++) {
    for(ix=0;ix<numx;ix++) {
      distance(lats[iy][ix],lons[iy][ix],xlatf,xlonf,1,&xdist,&xangle);
      if((xdist>=(float)rin)&&(xdist<=(float)rout)) {
        ihfov[iy][ix]=temps[iy][ix]/10.0F;
        xarray[idx]=ihfov[iy][ix];    /* for total sample statistics */
        idx++;
      }
    }
  }

  idx--;
  /* compute total sample statistics */
  XAVG(xarray,idx,&xxrad[1][0],&xxrad[1][1],&xxrad[1][2],&xxrad[1][3]);

  /* call mnansd ot get mean/sd for 3x3 boxes */
  MNANSD(ihfov,numx,numy,lside,xm,xs,&np);

  /* create new/smaller dataset of individual
     3X3s which meet criteria in channel.
     this is the filtered sample */
  for(i=0;i<np;i++) {
    if(xs[i]<=valth) {
      xmf[kno]=xm[i];
      /* xsf[kno]=xs[i]; */
      kno++;
    }
  }

  kno--;
  /* compute filtered sample statistics */
  XAVG(xmf,kno,&xxrad[2][0],&xxrad[2][1],&xxrad[2][2],&xxrad[2][3]);

  /* do histogramming to find clusters */
  HWORK(xmf,kno,jslot,kslot);
  
  /* count warm/cold filtered samples */
  for(i=0;i<kno;i++) {
    if(kslot[i]==1) {
      xwf[kknum]=xmf[i];
      kknum++;  /* cold sample size */
    }
    if(jslot[i]==1) {
      xcf[jjnum]=xmf[i];
      jjnum++;  /* warm sample size */
    }
  }

  jjnum--;
  kknum--;
  /* compute cold filtered sample statistics */
  XAVG(xwf,kknum,&xxrad[0][0],&xxrad[0][1],&xxrad[0][2],&xxrad[0][3]);

  /* compute warm filtered sample statistics */
  XAVG(xcf,jjnum,&xxrad[3][0],&xxrad[3][1],&xxrad[3][2],&xxrad[3][3]);

  isav[0]=kknum;    /* number of 3x3 in cold sample */
  isav[1]=idx;      /* total number in sample */
  isav[2]=kno;      /* number of 3x3 in filtered sample */
  isav[3]=jjnum;    /* number of 3x3 in warm sample */
  isav[4]=np;       /* total number of 3x3 */

}

void MNANSD(float ihfov[maxd][maxd],int numx,int numy,int lside,float *xmean,float *xsd,int *np)
/* Calculate means and standard deviations of squares
   of side length lside pixels comprising a given fov 
    Inputs  : ihfov - input 2-d input array
              lside - length of side of square in pixels
    Outputs : xmean - means array
              xsd   - standard deviations array
              np    - number of values in xmean/xsd
*/
{
  int ial,ias,i,j,xnp=0;
  double x,xsumsq,xsum,side2;
  logical iok;
 
  side2= (double) (lside*lside);
  for(ial=0;ial<(numx-lside+1);ial++) { 
    for(ias=0;ias<(numy-lside+1);ias++) { 
      iok=TRUE;
      for(j=0;j<=(lside-1);j++) { 
        for(i=0;i<=(lside-1);i++) 
	{ if(ihfov[ias+i][ial+j]==0.0F) iok=FALSE; }
      }
      if(iok) 
      {
        xsum=0.0;
        xsumsq=0.0;
        for(j=0;j<=(lside-1);j++) { 
          for(i=0;i<=(lside-1);i++) { 
            x= (double)ihfov[ias+i][ial+j];
            xsum=xsum+x;
            xsumsq=xsumsq+(x*x);
          }
        }
        xmean[xnp]= (float) (xsum/side2);
        x=(xsumsq/side2)-(double)(xmean[xnp]*xmean[xnp]);
        if(x>0.) 
	{ xsd[xnp]= (float) sqrt(x); }
        else { xsd[xnp]=0.0F; }
        xnp++;
      }
    }
  }
  xnp--;
  *np=xnp;
  return;
}

void HWORK(float *data,int np,
           int *jslot,int *kslot)
/* Determine histogram bins within standard deviation thresholds
   and output array containing flag values for analysis.
    Inputs  : data  - array of data
              np    - number of values in array
    Outputs : jslot - array containing flag for use in warm histogram
              kslot - array containing flag for use in cold histogram
*/
{
  int iflag=0,ismo=0,iend=5,nslot;
  int i,iind,ind1,ind2,ind3,ind4;
  int ihist[slots],idist[slots];       /* maximum of nslot elements */
  int islot[bufsiz];   /* maximum of np elements */
  float sdevl,sdevr,xmin,xdif,fave,varl,varr;

  for(i=0;i<np;i++) {
    jslot[i]=0;
    kslot[i]=0;
  }

  nslot=np/20;
  HISTM(data,np,nslot,iflag,islot,ihist,&xmin,&xdif);

  /* WARM CLUSTER */
  FIT2(ihist,nslot,xmin,xdif,ismo,1,iend,&iind,&fave,&varl,&varr);
  /*rlo=xmin+ (float)(iind-1)*xdif;*/
  /*rpl=(rlo+(rlo+xdif))/2.0F;*/
  /* contrain variances to an upper limit of 25 */
  varr= MIN(25.0F,varr);
  varl= MIN(25.0F,varl);

  CURVE(nslot,xmin,xdif,iind,fave,varl,varr,idist);
  /* compute radiance at gaussian peak and 
     standard deviations of curves on each side of peak */
  /*rlo=xmin+(float) ((iind-1)*xdif);*/
  /*rave=(rlo+(rlo+xdif))/2.0F;*/
  sdevl = (float)sqrt( (double) varl)/xdif;
  sdevr = (float)sqrt( (double) varr)/xdif;

  /* compute indicies corresponding to point selection thresholds */
  ind1=iind-((int)sdevl*3);  /* left  - 3 STDV from center */
  ind2=iind-((int)sdevl*1);  /* left  - 1 STDV from center */
  ind3=iind+((int)sdevr*1);  /* right - 1 STDV from center */
  ind4=iind+((int)sdevr*3);  /* right - 3 STDV from center */

  /* select points for use in histogram */
  for(i=0;i<np;i++) {
    if((islot[i]>=ind2)&&(islot[i]<=ind3)) 
    { jslot[i]=1; }   /* all points w/in 1 STDV of peak */
    else if((islot[i]>=ind1)&&(islot[i]<ind2)) 
    {
      /* as many points w/in 1 and 3 STDV of peak
         (left side) as suggested by gaussian fit */
      if(idist[islot[i]]>0) 
      {
        jslot[i]=1;
        idist[islot[i]]--;
      }
    }
    else if((islot[i]<=ind4)&&(islot[i]>ind3)) 
    {
      /* as many points w/in 1 and 3 STDV of peak
         (right side) as suggested by gaussian fit */
      if(idist[islot[i]]>0) 
      {
        jslot[i]=1;
        idist[islot[i]]--;
      }
    }
    if((islot[i]==iind)&&(jslot[i]!=1)) 
    {
      /* make sure all points corresponding to peak are chosen */
      jslot[i]=1;
      idist[islot[i]]--;
    }
  }
 
  /* COLDEST CLUSTER */
  FIT2(ihist,nslot,xmin,xdif,ismo,2,iend,&iind,&fave,&varl,&varr);
  /*rlo=xmin+ (float) (iind-1)*xdif;*/
  /*rph=(rlo+(rlo+xdif))/2.0F;*/
  /* contrain variances to an upper limit of 25 */
  varr= MIN(25.0F,varr);
  varl= MIN(25.0F,varl);

  CURVE(nslot,xmin,xdif,iind,fave,varl,varr,idist);
  /* compute radiance at gaussian peak and 
     standard deviations of curves on each side of peak */
  /*rlo=xmin+((float)(iind-1)*xdif);*/
  /*rave=(rlo+(rlo+xdif))/2.0F;*/
  sdevl = (float)sqrt((double)varl)/xdif;
  sdevr = (float)sqrt((double)varr)/xdif;

  /* compute indicies corresponding to point selection thresholds */
  ind1=iind-((int)sdevl*3);  /* left  - 3 STDV from center */
  ind2=iind-((int)sdevl*1);  /* left  - 1 STDV from center */
  ind3=iind+((int)sdevr*1);  /* right - 1 STDV from center */
  ind4=iind+((int)sdevr*3);  /* right - 3 STDV from center */

  /* select points for use in histogram */
  for(i=0;i<np;i++) {
    if((islot[i]>=ind2)&&(islot[i]<=ind3)) 
    {
      kslot[i]=1;   /* all points w/in 1 STDV of peak */
    }
    else if((islot[i]>=ind1)&&(islot[i]<ind2)) 
    {
      /* as many points w/in 1 and 3 STDV of peak
         (left side) as suggested by gaussian fit */
      if(idist[islot[i]]>0) 
      {
        kslot[i]=1;
        idist[islot[i]]--;
      }
    }
    else if((islot[i]<=ind4)&&(islot[i]>ind3)) 
    {
      /* as many points w/in 1 and 3 STDV of peak
         (right side) as suggested by gaussian fit */
      if(idist[islot[i]]>0) 
      {
        kslot[i]=1;
        idist[islot[i]]--;
      }
    }
    if((islot[i]==iind)&&(kslot[i]!=1)) 
    { /* make sure all points corresponding to peak are chosen */
      kslot[i]=1;
      idist[islot[i]]--;
    }
  }
  return;
}

void FIT2(int *ihist,int nslot,float xmin,float xdif,int ismoo,int ijob,
   int nhot,int *iind,float *fave,float *varl,float *varr)
/* Apply a ismoo-point smoother to the histogram.  Find the peak
   frequency in the smoothed histogram (searching from the high
   data value end) and fit a gaussian distribution to each half
   of that peak.
    Inputs  : ihist - histogram frequencies
              nslot - number of data slots in histogram (150)
              xmin  - minimum data value in histogram
              xdif  - data value range of each histogram slot
              ismoo - flag for smooth/no-smooth
                      0 - do not apply smoother
                      # - apply #-point smoother
              ijob  - smoothed histogram or warmest cluster
              nhot  - warm end buffer limit (?)
    Outputs : iind  - histogram slot corresponding to peak
              fave  - frequency of peak
              varl  - variance defining left side of curve
              varr  - variance defining right side of curve
*/ 
{
  int i,jj,iloop,indx,istart=0,ia,ib,xiind;
  float xvar,var,xdist[slots],xave=0.0F,pts;
  logical incr=TRUE,foundstart=FALSE;

/* initialize xdist with 0 */
  for(i=1;i<nslot;i++) { xdist[i]=0.0F;  }
 
/* keep xdist 1-based for Fourier transform to match Fortran processing */
  for(i=1;i<=nslot;i++) xdist[i]=(float)(ihist[i-1]);

  /* apply ismoo-point smoother to histogram */
  pts= (float) ismoo;
  if(ismoo!=0) SMOOFT(xdist,nslot,pts);
 
  /* find max frequency in smoothed histogram */
  if(ijob==1) 
  {
    /* check from warm end first */
    for(i=nslot;i>0;i--) {
      if(xdist[i]>=1.0F) { istart++; foundstart=TRUE; }
      if(foundstart) {
        if(xdist[i]>xave) {
          xave=xdist[i];
          xiind=i;
        } else {
          /* this case is not steadily increasing */
          incr=FALSE;
        }
        if(istart>=nhot&&!incr) break;
      }
    }
  }
  else if(ijob==2) 
  {
    /* check from cold end first */
    for(i=1;i<=nslot;i++) {
      if(xdist[i]>=1.0F) { istart++; foundstart=TRUE; }
      if(foundstart) {
        if(xdist[i]>xave) {
          xave=xdist[i];
          xiind=i;
        } else {
          /* this case is not steadily increasing */
          incr=FALSE;
        }
      }
      if(istart>=nhot&&!incr) break;
    }
  }
  else 
  {
    sprintf(iout,"NOT A VALID JOB NUMBER FOR FIT2\n");
    xprintf(iout);
  }

  for(jj=1;jj<=2;jj++) {
    if(jj==1) {
      /* fit gaussian to left side of peak */
      ia=xiind-3;
      ib=xiind-1;
    } else {
      /* fit gaussian to right side of peak */
      ia=xiind+1;
      ib=xiind+3;
    }
    ia= MAX(1,ia);
    ib= MIN(nslot,ib);

    /* average gaussian curves three points each side of peak
       to obtain average variance of those curves */
    iloop=0;
    xvar=0.0F;
    for(indx=ia;indx<=ib;indx++) {
      TPGAUS(xiind,indx,xdist,xmin,xdif,&var);
      if(var>0.0F) {
        iloop++;
        xvar=xvar+var;
      }
    }
    if(iloop!=0) 
    { xvar=xvar/ (float)iloop; }
    else { xvar = 0.0F; }

    /* store avg variances for each side of curve */
    if(jj==1) {
      *varl=xvar;  /* left side variance */
    } else {
      *varr=xvar;  /* right side variance */
    } 
  }
  *iind=xiind;
  *fave=xdist[*iind];  /* store frequency of peak */
  return;
}

void TPGAUS(int icen,int ind,float *xdist,float xmin,float xdif,float *var)
/* Fit a gaussian (normal) distribution to a histogram using 
   a two-point method.
    Inputs  : icen  - center histogram point to fit curve to
              ind   - "other" histogram point
              xdist - histogram array
              xmin  - minimum data value of histogram
              xdif  - difference between histogram data values
    Outputs : var   - variance describing gaussian curve
*/
{
  float rlo,rave,fave,r,f,xnum,xden;

  rlo=xmin+( (float) (icen-1)*xdif);
  rave=(rlo+(rlo+xdif))/2.0F;
  fave=xdist[icen];

  rlo=xmin+((float)(ind-1)*xdif);
  r=(rlo+(rlo+xdif))/2.0F;
  f=xdist[ind];

  if(f<0.0F) f=0.0F;
  if(fave<0.0F) fave=0.0F;

  if((f!=0.0F)&&(fave!=0.0F)&&(f!=fave)) 
  {
    xnum=-1.0F*(r-rave)*(r-rave);
    xden=(float)(2.0*(log((double) (f/fave))));
    *var=xnum/xden;
  }
  else { *var=0.0F; }
  return;
}

void CURVE(int nslot,float xmin,float xdif,int iind,float fave,
           float varl,float varr,int *idist)
/* Calculate gaussian curve 
    Inputs  : nslot - histogram slots to fill with curve values
              xmin  - minimum data value in histogram
              xdif  - data value increment between histogram slots
              iind  - index of histogram slot with frequency peak
              fave  - frequency corresponding to peak
              varl  - variance defining left side of curve
              varr  - variance defining right side of curve
    Outputs : idist - gaussian curve
*/
{
  float rlo,rave,var,xdist,r;
  int i;


  for(i=0;i<nslot;i++) { idist[i]=0; }

  rlo=xmin+((float)(iind-1)*xdif);
  rave=(rlo+(rlo+xdif))/2.0F;
  for(i=0;i<=nslot;i++) {
    if(i<=iind) 
    {
      var=varl;
    }
    else { var=varr; }
    rlo=xmin+((float)(i-1)*xdif);
    r=(rlo+(rlo+xdif))/2.0F;
    xdist=fgauss(fave,r,rave,var);
    idist[i] = (int)xdist;
  }
  return;
}

float fgauss(float fave,float r,float rave,float var)
/* Compute frequency of gaussian function
    Inputs  : fave - frequency at peak of distribution
              r    - data value (position relative to peak)
              rave - data value corresponding to peak
              var  - variance describing curve
    Outputs : fgauss - frequency of gaussian function
*/
{
  float xvar=-9999.0F;

  if(var!=0.0F) xvar=(-1.0F*(r-rave)*(r-rave))/(2.0F*var);
  if(xvar<-10.0F) { return (0.0F); }
  else { return (fave*(float)exp((double)xvar)); }
}

void HISTM(float *data,int np,int nslot,int iflag,
           int *islot,int *ihist,float *xmin,float *xdif)
/* Contruct a histogram
    Inputs  : data  - array of data
              np    - number of values in array
              nslot - number of slots desired in histogram (150)
              iflag - radiance data (1) or other (0)
    Outputs : islot - array marking each data points' 
                      resultant histogram slot
              ihist - array of number of points in each
                      histogram slot (i.e. the histogram)
              xmin  - minimum data value
              xdif  - value range of each histogram slot
*/
{
  int i;  
  float xxmin,xxdif;

  HISTD(data,np,nslot,iflag,&xxmin,&xxdif);
  if(xxdif!=0.0F) 
  {
    for(i=0;i<nslot;i++) {
      ihist[i]=0;
    }
    for(i=0;i<np;i++) {
      islot[i]=ihistl(data[i],xxmin,xxdif);
      if((islot[i]<=nslot)&&(islot[i]>=0)) 
      {
        ihist[islot[i]]++;
      }
    }
  }
  *xdif=xxdif;
  *xmin=xxmin;
  return;
}

void HISTD(float *data,int np,int nslot,int iflag,
           float *xmin,float *xdif)
/* Define minimum data value and range of histogram value
   for each histogram slot.
    Inputs  : data  - array of data
              np    - number of values in array
              nslot - number of slots desired in histogram (150)
              iflag - radiance data (1) or other (0)
    Outputs : xmin  - minimum data value
              xdif  - value range of each histogram slot
*/
{
  int i;
  float xmax,xxmin;

  if(iflag==1) 
  {
    xxmin=0.0F;
    xmax= (float) (nslot-1);
  }
  else if (iflag==2) 
  {
    xxmin=0.0F;
    xmax=150.0F;
  }
  else if (iflag==3) 
  {
    xxmin=0.0F;
    xmax=15.0F;
  }
  else 
  {
    xxmin=99999.0F;
    xmax=0.0F;
    for(i=0;i<np;i++) {
      if(data[i]<xxmin) xxmin=data[i]; 
      if(data[i]>xmax) xmax=data[i]; 
    }
  }
  *xdif=(xmax-xxmin)/ (float) (nslot-1);
  *xmin=xxmin;
  return;
}

int ihistl(float data,float xmin,float xdif)
/* Compute histogram slot in which data value falls
    Inputs  : data - data value
              xmin - minimum value of data
              xdif - difference in value across one 
                     histogram slot
    Outputs : ihistl - histogram slot of data value
*/
{
  return ( (int )((data-xmin)/xdif));
}
 
void SMOOFT(float *y,int nslot,float pts)
{ 
  int m=2,nmin,j,k,mo2;
  float y1,yn,rn1,fac,const1;

  nmin=(int)((float)nslot+(2.0F*pts));
  while (m<nmin) 
  {
    m=2*m;
  }
  const1= (pts/(float)m)*(pts/(float)m);
  y1=y[1];
  yn=y[nslot]; 
  rn1=1.0F/((float)(nslot)-1.0F);
  for(j=1;j<nslot;j++) {
    y[j]=y[j]-(rn1*(y1*(float)(nslot-j)+(yn*(float)(j-1))));
  }
  if((nslot)<=m) 
  { 
    for(j=nslot;j<m;j++) {  
      y[j]=0.0F;
    }
  }
  mo2=m/2;
  REALFT(y,mo2,1); 
  y[1]=y[1]/(float)mo2;
  fac=1.0F;
  for(j=1;j<=(mo2-1);j++) {
    k=(2*j)+1;
    if(fac!=0.0F) 
    {
      fac=MAX(0.0F,(1.0F-const1*(float)(j*j))/(float)mo2);
      y[k]=fac*y[k];
      y[k+1]=fac*y[k+1];
    }
    else 
    {
      y[k]=0.0F;
      y[k+1]=0.0F;
    }
  }
  fac=MAX(0.0F,(1.0F-0.25F*(float)(pts*pts))/(float)mo2);
  y[2]=fac*y[2];
  REALFT(y,mo2,-1);
  for(j=1;j<=nslot;j++) {
    y[j]=(rn1*(y1*(float)(nslot-j)+(yn*(float)(j-1))))+y[j];
  }
  return;
}

void REALFT(float *data,int nslot,int isign)
{
  int i,n2p3=(2*nslot)+3,i1,i2,i3,i4;
  float c1=0.5F,c2;
  float wrs,wis,h1r,h1i,h2r,h2i;
  double wr,wi,wpr,wpi,wtemp;
  double theta=(6.28318530717959/(double)2.0)/(double)(nslot);

  if(isign==1) 
  {
    c2=-0.5F;
    FOUR1(data,nslot,1);
  }
  else 
  {
    c2=0.5F;
    theta=-theta;
  }

  wpr= (double)(-2.0)*sin ((double)((0.5*theta)*(0.5*theta)));
  wpi=sin(theta);
  wr=(double)(1.0)+wpr;
  wi=wpi;
  for(i=2;i<=(nslot/2+1);i++) {
    i1=(2*i)-1;
    i2=i1+1;
    i3=n2p3-i2;
    i4=i3+1;
    wrs=(float)(wr);
    wis=(float)(wi);
    h1r=c1*(data[i1]+data[i3]);
    h1i=c1*(data[i2]-data[i4]);
    h2r=-c2*(data[i2]+data[i4]);
    h2i=c2*(data[i1]-data[i3]);
    data[i1]=h1r+(wrs*h2r)-(wis*h2i);
    data[i2]=h1i+(wrs*h2i)+(wis*h2r);
    data[i3]=h1r-(wrs*h2r)+(wis*h2i);
    data[i4]=-h1i+(wrs*h2i)+(wis*h2r);
    wtemp=wr;
    wr=(wr*wpr)-(wi*wpi)+wr;
    wi=(wi*wpr)+(wtemp*wpi)+wi;
  }
  if(isign==1) 
  {
    h1r=data[1];
    data[1]=h1r+data[2];
    data[2]=h1r-data[2];
  }
  else 
  {
    h1r=data[1];
    data[1]=c1*(h1r+data[2]);
    data[2]=c1*(h1r-data[2]);
    FOUR1(data,nslot,-1);
  }
  return;
}

void FOUR1(float *data,int nslot,int isign)
{
  int i,j=1,m,n=nslot*2,mmax=2;
  int istep;
  float tempr,tempi;
  double wr,wi,wpr,wpi,wtemp,theta;
  double c1=6.28318530717959;

  for(i=1;i<=n;i=i+2) {
    if(j>i) 
    {
      tempr=data[j];
      tempi=data[j+1];
      data[j]=data[i];
      data[j+1]=data[i+1];
      data[i]=tempr;
      data[i+1]=tempi;
    }
    m=n/2;
    while((m>=2)&&(j>m)) 
    { /* are these indicies correct? */
      j=j-m;
      m=m/2;
    }
    j=j+m;
  }
  mmax=2;
  while(n>mmax) 
  {
    istep=2*mmax;
    theta=c1/(double)(isign*mmax);
    wpr=(double)(-2.0)*(sin((double)(0.5)*theta))*(sin((double)(0.5)*theta));
    wpi=sin(theta);
    wr=(double)(1.0);
    wi=(double)(0.0);
    for(m=1;m<=mmax;m=m+2) {
      for(i=m;i<=n;i=i+istep) {
        j=i+mmax;
        tempr=((float)(wr)*data[j])-((float)(wi)*data[j+1]);
        tempi=((float)(wr)*data[j+1])+((float)(wi)*data[j]);
        data[j]=data[i]-tempr;
        data[j+1]=data[i+1]-tempi;
        data[i]=data[i]+tempr;
        data[i+1]=data[i+1]+tempi;
      }
      wtemp=wr;
      wr=(wr*wpr)-(wi*wpi)+wr;
      wi=(wi*wpr)+(wtemp*wpi)+wi;
    }
    mmax=istep;
  }
  return;
}

void XAVG(float *data,int np,float *avg,float *sd,float *min,float *max)
{
  int ix;
  float xavg,xsd;
  float xsum=0.0F,xmin=999999.9F,xmax=-999999.9F;

  /* sum for mean */
  for(ix=0;ix<np;ix++) {
    xsum=xsum+data[ix];
    /* check for min/max */
    if(data[ix]<xmin) xmin=data[ix];
    if(data[ix]>xmax) xmax=data[ix];
  }
  xavg=xsum/(float)np;
  xsum=0.0F;

  /* sum perterbations for stdv */
  for(ix=0;ix<np;ix++) {
    xsum=xsum+((data[ix]-xavg)*(data[ix]-xavg));
  }
  xsd=(float)SQRT(xsum/(float)np);

  *avg=xavg;
  *sd=xsd;
  *min=xmin;
  *max=xmax;
}
