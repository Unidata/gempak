#include "inc/odt.h"

int odtscene( struct odtdata * );
void tofit( float *,float *,int,int,float * );
void classify( float,float,float,float,struct odtdata * );
int checkrapid( struct odtdata * );
extern int fft( float *, float *, int * );
extern void xprintf( char * );
extern double calctime( int, int );
extern void maxtnoval( struct odtdata *,int,int *,float * );

/* extern global variables */
extern int kstart,kend,keyer,kres;
extern struct odtdata *odthistoryfirst;
extern struct ringdata *tcircfirst;
extern char iout[200];

int odtscene(struct odtdata *odtcurrent)
/* Perform Fast Fourier Transform (FFT) analysis and determine 
   scene type via empirically defined threshold values.
    Inputs  : odtcurrent - structure containing current intensity values
    Outputs : various elements within structure odtcurrent
*/
{
  /*int   nxx=(6*nbin)+150;*/
  int   nbin=64,iok;
  int   ixx,iyy,iscene,idxcnt;
  float bd[534],cbd[534],tbd[534];
  float radb,rade,teye,dx2;
  float eyefft,eyecnt;
  /*float rngfft,rngcnt;*/
  /*float eyecg,rngwg,rngcg;*/
  float eyemg,eyewg,rngmg;
  float gmaxm,gmaxw,gmaxc;
  struct ringdata *tcirc;

  /* initialize temperature histogram bin values */
  for(iyy=0;iyy<nbin;iyy++) {
    bd[iyy]=26.0F-(float)iyy*2.0F;
  }

  /* set up arrays for FFT ananlysis.
     iscene=0 will perform FFT for cloud top region while
     iscene=1 will perform FFT for eye region */
  for(iscene=0;iscene<=1;iscene++) {
    for(iyy=0;iyy<nbin;iyy++) {
      cbd[iyy]=0.0F;
      tbd[iyy]=0.0F;
    }

    /* define start and end radii values */
    if(iscene==0) {
      /* CLOUD TOP */
      radb=(float)kstart;
      rade=(float)kend;
    } else {
      /* EYE REGION */
      radb=0.0F;
      rade=(float)keyer;
    }

    /* load arrays for FFT analysis */
    tcirc=tcircfirst;
    while(tcirc!=0) {
      if((tcirc->dist>=radb)&&(tcirc->dist<=rade)) {
        teye=(tcirc->temp - 273.16F);
        for(ixx=0;ixx<(nbin-1);ixx++) {
          if((teye<=bd[ixx])&&(teye>=bd[ixx+1])) {
            cbd[ixx]=cbd[ixx]+1.0F;
            tbd[ixx]=tbd[ixx]+teye;
          }
        }
      }
      tcirc=tcirc->nextrec;
    }

    /* perform FFT analysis */
    iok=fft(cbd,&dx2,&idxcnt);
    if(iok<0) {
      return -1;
    }
    
    /* calculate min and max histogram temperature values and maximum histogram frequency value */
    tofit(cbd,tbd,nbin,0,&gmaxm);    /* calculate max frequency temperature for eye/cloud */
    tofit(cbd,tbd,nbin,1,&gmaxc);    /* calculate min (coldest) temperature for eye/cloud */
    tofit(cbd,tbd,nbin,2,&gmaxw);    /* calculate max (warmest) temperature for eye/cloud */

    /* assign variables based upon region being analyzed */
    if(iscene==0) {
      /* CLOUD TOP */
      /*rngfft=dx2;*/
      /*rngcnt=idxcnt;*/
      rngmg=gmaxm;
      /*rngcg=gmaxc;*/
      /*rngwg=gmaxw;*/
    } else {
      /* EYE REGION */
      eyefft=dx2;
      eyecnt=(float)idxcnt;
      eyemg=gmaxm;
      /*eyecg=gmaxc;*/
      eyewg=gmaxw;
    }
  }

  /* assign rapid flag value in structure odtcurrent */
  odtcurrent->IR.rapid=checkrapid(odtcurrent);

  /* assign mean cloud top temperature in structure odtcurrent */
  odtcurrent->IR.meancloudt=rngmg;

  /* assign scenetype value in structure odtcurrent */
  classify(eyecnt,eyefft,eyewg,eyemg,odtcurrent);

  return 0;
}

void tofit(float *cbd, float *tbd, int nbin, int inflag, float *fave)
/* Determine maximum histogram bin, coldest histogram bin, or warmest
   histogram bin.  Coldest and warmest bins are assigned to be any
   local peak histogram bin at either end of the entire histogram
   which is greater than 4 bins from either edge.  
    Inputs  : cbd    - histogram bin count array
              tbd    - histogram temperature array
              nbin   - total number of bins in histogram
              inflag - flag to identify histogram value sought
                        (0-maximum,1-coldest,2-warmest)
    Outputs : fave   - histogram value for type defined by inflag
*/
{
  int ixx,jmax;
  float xmax=0.0F;

  if(inflag==0) {
    /* find maximum histogram bin */
    for(ixx=0;ixx<nbin;ixx++) {
      if(cbd[ixx]>xmax) {
        xmax=cbd[ixx];
        jmax=ixx;
      }
    }
  } else if(inflag==1) {
    /* find coldest histogram bin */
    for(ixx=nbin-2;ixx>0;ixx--) {
      if((cbd[ixx]>xmax)&&(cbd[ixx-1]<cbd[ixx])&&(cbd[ixx+1]<cbd[ixx])) {
        xmax=cbd[ixx];
        jmax=ixx;
        if(xmax>=4.0F) break;
      }
    }
  } else {
    /* find warmest histogram bin */
    for(ixx=1;ixx<(nbin-2);ixx++) {
      if((cbd[ixx]>xmax)&&(cbd[ixx-1]<cbd[ixx])&&(cbd[ixx+1]<cbd[ixx])) {
        xmax=cbd[ixx];
        jmax=ixx;
        if(xmax>=4.0F) break;
      }
    }
  }
  *fave=tbd[jmax]/cbd[jmax];
}

void classify(float eyecnt,float eyefft,
              float eyewg,float eyemg,
              struct odtdata *odtcurrent)
/* Classify scene type based on FFT analysis and histogram temperatures
   using empirically defined threshold values.
    Inputs  : eyecnt     - bin containing maximum FFT value (eye)
              eyefft     - value of maximum FFT bin (eye)
              eyewg      - warmest histogram bin temperature value
              eyemg      - maximum frequency histogram bin temperature value
              odtcurrent - structure containing current image analysis
    Outputs : scenetype in structure odtcurrent is modified
*/
{

  float eyetemp,cloudtemp,meantemp;

  eyetemp=odtcurrent->IR.eyet;
  cloudtemp=odtcurrent->IR.cloudt;
  meantemp=odtcurrent->IR.meancloudt;
  if(odtcurrent->IR.rapid>1) cloudtemp=MIN(meantemp,cloudtemp);

  if(eyecnt<=1.0F) {
    /* CDO */
    odtcurrent->IR.scenetype=6;
    if(((eyefft<12.0F)&&(eyewg-eyemg))>=10.0F) {
      /* EMBEDDED CENTER */
      odtcurrent->IR.scenetype=5;
    }
  } else if (eyecnt<=7.0F) {
    if((eyewg-eyemg)<=10.0F) {
      /* CDO */
      odtcurrent->IR.scenetype=6;
      if(eyetemp>=-10.0F) {
        /* SMALL EYE (PINHOLE) */
        odtcurrent->IR.scenetype=2;
      }
    } else if ((eyewg-eyemg)<=30.0F) {
      /* EMBEDDED CENTER */
      odtcurrent->IR.scenetype=5;
      if((eyewg>=-30.0F)||(eyetemp>=-10.0F)) {
        /* RAGGED EYE */
        odtcurrent->IR.scenetype=3;
      }
    } else if ((eyewg-eyemg)<=50.0F) {
      /* RAGGED EYE */
      odtcurrent->IR.scenetype=3;
    } else {
      /* CLEAR EYE */
      odtcurrent->IR.scenetype=0;
    }
  } else {
    /* CLEAR EYE */
    odtcurrent->IR.scenetype=0;
  }
  
  /* special cases with very warm center region */
  if((eyemg>-30.0F)&&(cloudtemp>-30.0F)) {
    /* VERY LARGE CLEAR EYE */
    odtcurrent->IR.scenetype=1;
    if(cloudtemp>-30.0F) {
      /* DRY SLOT/RAGGED EYE */
      odtcurrent->IR.scenetype=4;
      if(meantemp>-30.0F) {
        /* SHEAR */
        odtcurrent->IR.scenetype=9;
      }
    }
  }
}

int checkrapid(struct odtdata *odtcurrent)
/* Check for rapid deepening by comparing cloud top temperature
   value with maximum histogram cloud temperature value and previous
   rapid intensification flags.  If rapid intensification has been
   indicated for two or more continuous hours, the rapid flag
   will be set and modification to the Final T# time averaging
   scheme (number of hours over which time-weighted value will
   be calculated) and cloud top temperature (reassigned to 
   maximum histogram bin value) will take place.
    Inputs : odtcurrent - structure containing current image analysis
    Outputs: return value will be rapid flag
                  flag : 0=no rapid deepening incidated
                         1=onset of rapid deepening-watch for 2 hours
                         2=rapid deepening occuring-apply rules
                         3=rapid deepening concluded-holding
*/
{
  int icnt=0,rapid=0,crapid=0,iweak;
  float maxtno;
  double curtime,tlimit,xtime;
  struct odtdata *lasthistory,*odthistory;
  
  odthistory=odthistoryfirst;
  lasthistory=odthistoryfirst;  /* initialize lasthistory to first record in history file */

  if(odthistoryfirst==0) return crapid;

  /* determine current time in dddd.dddd format */
  curtime=calctime(odtcurrent->IR.date,odtcurrent->IR.time);
  
  /* determine time at two hours previous from current time */
  tlimit=curtime-1.0/12.0;
   
  /* set cloud temperature based on cloud top temperature
     and maximum histogram temperature */
  if((odtcurrent->IR.cloudt<=-70.0F)&&(odtcurrent->IR.meancloudt<=-75.0F)) {
    crapid=1;
  }

  /* check previous rapid flag values
     rapid cound number of rapid deepening flags, no matter value */
  while(odthistory!=0) {
    xtime=calctime(odthistory->IR.date,odthistory->IR.time);
    if(xtime>curtime) break;
    if((xtime>=tlimit)&&(xtime<curtime)) {
      icnt++;
      if(odthistory->IR.rapid>0) rapid++;
    }
    lasthistory=odthistory;
    odthistory=odthistory->nextrec;
  }

  if(icnt>0) {
    /* there are rapid deepening flags in last two hours */
    if(rapid==icnt) {
      /* there have been two continuous hours worth of rapid deepening */
      if(crapid==1) {
        /* rapid deepening flag indicated and can now be activated
           since cloud top temperature is still cold enough */
        crapid=2;
      } else {
        crapid=0;
        /* rapid deepeing flag indicated and activated, now 
           cloud top temperature is not cold enough, so flag as such */
        if(lasthistory->IR.rapid>=2) crapid=3;
        /* if Rule 9 is being applied, we want to hold the 6-hour time
           average in effect for 12 hours in order to prevent a discontinuity
           when the time averaging scheme flips from 6-hour to 12-hour.
           if Rule 9 is not being applied, let rapid deepening flag return to 0 */
        maxtnoval(odtcurrent,12,&iweak,&maxtno);
        if(iweak==0) crapid=0;
      }
    }
  } else {
    /* there is no data within last two hours... use the last rapid 
       deepening flag as a guide for the current value */ 
    if(crapid==1) {
      if(lasthistory->IR.rapid==2) crapid=2;
    } else {
      if(lasthistory->IR.rapid==2) crapid=3;
    }
  }

  return crapid;
}
