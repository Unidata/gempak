/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"
/* include file containing all AODT remapping functions */
#include "../inc/odtremap.h"

#include <time.h>
#include <sys/types.h>
#define IND(y,x) ((y-1)*remap_vars.ncl+x)
#define BTEST(word, bit) (word & (0x01 << ((bit)%8)))
#define BSET(word, bit)  (word | (0x01 << ((bit)%8)))
#define  A_LOG(x)     (float)( log( (double)(x) ) )
#define  A_ROUND(x)   (int)( (x+0.5) )

/* internal routines */
int  aodtv72_automode1( float *, float *, int * );
int  aodtv72_automode2( float,float,float *, float *, int * );
int  aodtv72_readforecast( FILE *, int * );
void aodtv72_pickfinallocation( float, float, float, float, int,
                   float, float, float, float, int,
                   float, float, float,
                   float, float, float,int,
                   float *, float *, float *,int * );
void aodtv72_logspiral( float, float, float, int, int *, float *, float * );
void aodtv72_laplacian( float ** );
int  aodtv72_polint( double *,float *,int,double,float *,float *);

/* Tony Wimmers center finding routines */
int  aodtv72_spiralCenterLowRes(float, float, float *, float *, float *,float **);
int  aodtv72_gradient(float [maxd][maxd], int, int, float, float, float **, float **);
int  aodtv72_ringFit(float, float, int **, float *, float *, int *, float **);
int  aodtv72_lalo2indsFloat(float, float, float [maxd][maxd], float [maxd][maxd], int, int, int *, int *);
int  aodtv72_inds2laloFloat(int, int, float [maxd][maxd], float [maxd][maxd], int, int, float *, float *);
int  aodtv72_circleFilt(int, int **, int **);
int  aodtv72_calcScores(float, float, float, float, float, float **, float, float, float, float **, float *, float *, float *, int *);
int  aodtv72_findRingScore(float, float, float **, float *);
int  aodtv72_moatMaskCalc( int ** );
int  aodtv72_meshgrid( float, int **);
int  aodtv72_bwimage( int **, int, int, int, int ** );
int  aodtv72_find( int *, int );

/* Dave Santek remapping routines */
int  aodtv72_remapdata( );
int  aodtv72_uinit( );
int  aodtv72_init( );
void aodtv72_determinedest( );
void aodtv72_corner(int, int, int, float [], float []);
int  aodtv72_umap(int, int, int *, int *);
int  aodtv72_domap(int , float [], float [], int, int);
int  aodtv72_findpoint (float, float, int *, int *);

static struct  datagrid *areadataRM;

int aodtv72_automode1( float *retlat,float *retlon,int *retpos ) 
/* Determine storm position at time curtime using NHC/JTWC 
   forecast discussion products.  Time and location information
   from these products are then interpolated to time in question
   to derive a estimated storm position.  If position estimation
   cannot be calculated, a lat/lon position of -99,99/-99.99 will
   be returned.
    Inputs  : none
    Outputs : retlat  - estimated latitude position
              retlon  - estimated longitude position
    Note    : autopos - method used to derive estimated storm location 0-error
    Return : -43 : Error w/ forecast file open and BAD extrapolation
             -44 : Invalid forecast file and BAD extrapolation
             -45 : Error w/ forecast file read and BAD extrapolation
             -46 : Error w/ forecast interpolation and BAD extrapolation
              42 : GOOD INTERPOLATION
              43 : Error w/ forecast file open but GOOD EXTRAPOLATION
              44 : Invalid forecast file but GOOD extrapolation
              45 : Error w/ forecast file read but GOOD extrapolation
              46 : Error w/ forecast interpolation but GOOD EXTRAPOLATION
*/
{
  FILE *fp;
  int iok,ioklat,ioklon,usethis,iret,fnum;
  float telat,telon,dtelat,dtelon;
  double curtime;
  logical getextrap=FALSE;

  curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);
  iret=0;
  /* get file information */
  fp=fopen(fixfile_v72,"r+");
  if(fp==0) {
    telat=-99.99;
    telon=-99.99;
    usethis=0;
    getextrap=TRUE;
    iret=43;
  } else {
    /* Get forecast positions from NHC (or other) forecast file, if avaialble.
       Otherwise, use linear extrapolation of previous locations.
       Will use as "first guess" for Laplacian */
    iok=aodtv72_readforecast(fp,&fnum);

    if(iok!=0) {
      telat=-99.99;
      telon=-99.99;
      usethis=0;
      getextrap=TRUE;
      iret=46+iok; /* -1 will givd 45=invalid index / -2 will give 44=invalid file/dates */
    } else {
      /* Call Interpolation procedure.  Routine is called 
         for both latitude and longitude interpolations. */
      ioklat=aodtv72_polint(fcsttime_v72,fcstlat_v72,fnum,curtime,&telat,&dtelat);
      ioklon=aodtv72_polint(fcsttime_v72,fcstlon_v72,fnum,curtime,&telon,&dtelon);
      if((ioklat==0)&&(ioklon==0)) {
        /* Good interpolated values... use 'em */
        usethis=1;
        getextrap=FALSE;
        iret=42;
      } else {
        /* Bad interpolated values... try extrapolated values */
        getextrap=TRUE;
        usethis=6;
        iret=46;
      }
    }
  } 

  /* try to extrapolate storm location from previous storm locations in history file */
  if(getextrap) {
    /* call slopecal to get y-intercept values for lat/lon values */
    telat=aodtv72_slopecal((double)12.0,3);
    telon=aodtv72_slopecal((double)12.0,4);
    if((A_ABS(telat)>90.0)||(A_ABS(telon)>180.0)) {
      usethis=0;
      iret=-iret;   /* invalid interp and extrap... negative error code returns */
    } else {
      usethis=6;
      iret=iret;   /* invalid interp but good extrap... positive error code returns */
    }
  }
  
  *retlat=telat;
  *retlon=telon;
  *retpos=usethis;

  return iret;
}

int aodtv72_automode2(float taclat,float taclon,
                   float *retlat, float *retlon, int *retpos )
/* Additional automatic positioning of storm center location 
   using official forecasts from NHC or JTWC as input.  Storm
   location will be estimated using Laplacian analysis (looking
   for large gradients in temperature field, i.e. the eye) along
   with new spiral fitting and ring fitting routines derived 
   by Tony Wimmers in his MatLab routines (which have been converted).
   The final position will be determined utilizing 
   empirically defined confidence factors for each method.
   The final storm position will be returned along with a 
   position determination flag.
    Inputs  : taclat  - input storm center latitude position (forecast interpolation)
              taclon  - input storm center longitude position (forecast interpolation)
    Outputs : retlat  - final storm center latitude position
              retlon  - final storm center longitude position
              autopos - method used to derive estimated storm location
                         0-error
                         1-interpolation of operational forecast
                         2-Laplacian analysis
                         3-Warm Spot location
                         4-10^ log spiral analysis 
                         5-Combo method of spiral and ring analyses
                         6-linear extrapolation from prior locations
*/
{ 
  int   iok,ixx,iyy,ix,iy,a,b,aptr,bptr,ic1=0,ic2=0;
  int   searchrad2=90,threshold1=12.0,threshold2=7.5,usethis;
  int   imagedate,imagetime,fnum,ringSize,xn,yn;
  int   maxRingSize,scoreMethod;
  int   **moatFlagField;
  float latdiff,londiff,pi=3.141592;
  float latfcst,lonfcst,tempfcst;
  float xdist,xangle,uselat,uselon,useconf;
  float avelat1,stdvlat1,skewlat1,avelon1,stdvlon1,skewlon1;
  float avelat2,stdvlat2,skewlat2,avelon2,stdvlon2,skewlon2;
  float **spGrid1,**ringScores;
  float **lapvals;
  float *templat1,*templon1;
  float *templat2,*templon2;
  float latFirstGuess,lonFirstGuess,lonNW,lonNE,lonSW,lonSE;
  float latSpiralCenter,lonSpiralCenter,maxSpiralCenter;
  float latRingCenter,lonRingCenter,scoreRingCenter;
  float latFinal,lonFinal,scoreFinal;
  logical odocross=TRUE,dateline=FALSE;
   time_t timestart,timeend; 

  /* allocate memory */
  a=sizeof(int);
  b=sizeof(float);
  aptr=sizeof(int*);
  bptr=sizeof(float*);
  moatFlagField=(int **)calloc((size_t)maxd,aptr);
  lapvals=(float **)calloc((size_t)maxd,bptr);
  for(iy=0;iy<maxd;iy++) {
    moatFlagField[iy]=(int *)calloc((size_t)maxd,a);
    lapvals[iy]=(float *)calloc((size_t)maxd,b);
  }
  templat1=(float *)calloc((size_t)(maxd*4),b);
  templon1=(float *)calloc((size_t)(maxd*4),b);
  templat2=(float *)calloc((size_t)(maxd*4),b);
  templon2=(float *)calloc((size_t)(maxd*4),b);

  if(odocross) {
    /* crosshatch stuff */
    areadataRM=(struct datagrid *)malloc(sizeof(struct datagrid)); 
    londiff=areadata_v72->lon[0][0]-areadata_v72->lon[0][1];
    latdiff=areadata_v72->lat[0][0]-areadata_v72->lat[1][0];
    if(A_ABS(londiff-latdiff)<0.001) {
      /* data is already remapped */
      /* crosses dateline check */
      xn=areadata_v72->numx-1;
      yn=areadata_v72->numy-1;
      lonNW=areadata_v72->lon[0][0];
      lonNE=areadata_v72->lon[0][xn];
      lonSW=areadata_v72->lon[yn][0];
      lonSE=areadata_v72->lon[yn][xn];
      if((lonNW<lonNE)||(lonSW<lonSE)) dateline=TRUE;
      lonNW=lonNW+360.0;
      for(iyy=0;iyy<areadata_v72->numy;iyy++) {
        for(ixx=0;ixx<areadata_v72->numx;ixx++) {
          areadataRM->lon[iyy][ixx]=areadata_v72->lon[iyy][ixx];
          /* check for dateline crossing */
          if (dateline&&(areadataRM->lon[iyy][ixx]<0.0)) areadataRM->lon[iyy][ixx]=areadataRM->lon[iyy][ixx]+360.0;
          areadataRM->lat[iyy][ixx]=areadata_v72->lat[iyy][ixx];
          areadataRM->temp[iyy][ixx]=areadata_v72->temp[iyy][ixx];
        }
      }
      areadataRM->numx=areadata_v72->numx;
      areadataRM->numy=areadata_v72->numy;
    } else {
      /* remap data to rectilinear projection */
      /* (void)time(&timestart); */
      aodtv72_remapdata( ); 
      /* (void)time(&timeend); */
      /* printf("remap time=%f\n",difftime(timeend,timestart)); */
    }
  
    /* allocate space for final analysis grids from Ring Fit and Spiral Analysis */
    spGrid1=(float **)calloc((size_t)maxd*maxd,bptr);
    for(iy=0;iy<(maxd*maxd);iy++) {
      spGrid1[iy]=(float *)calloc((size_t)3,b);
    }
    ringSize=(maxd*maxd*15)+1;
    ringScores=(float **)calloc((size_t)ringSize,bptr);
    for(iy=0;iy<ringSize;iy++) {
      ringScores[iy]=(float *)calloc((size_t)3,b);
    }

    /* perform spiral analysis to determine storm center location */
    /* (void)time(&timestart); */
    iok=aodtv72_spiralCenterLowRes(taclat,taclon,&latSpiralCenter,&lonSpiralCenter,&maxSpiralCenter,spGrid1);
    /* printf("Spiral Score : lat=%f  lon=%f  score=%f\n",latSpiralCenter,lonSpiralCenter,maxSpiralCenter); */
    /* (void)time(&timeend); */
    /* printf("spiral time=%f\n",difftime(timeend,timestart)); */

/*
    / redefine first guess for ring analysis as input storm location point /
    latFirstGuess=taclat;
    lonFirstGuess=taclon;
*/
    /* redefine first guess for ring analysis as input storm location point */
    latFirstGuess=latSpiralCenter;
    lonFirstGuess=lonSpiralCenter;
  
    /* calculate Moat Mask for false eye check */
    /* (void)time(&timestart); */
    iok=aodtv72_moatMaskCalc( moatFlagField );
     /* printf("moatMask time=%f\n",difftime(timeend,timestart)); */
    /* (void)time(&timeend); */

    /* perform ring analysis to determine storm center location */
    /* (void)time(&timestart); */
    iok=aodtv72_ringFit(latFirstGuess,lonFirstGuess,moatFlagField,&latRingCenter,&lonRingCenter,&maxRingSize,ringScores);
    /* printf("Ring Score   : lat=%f  lon=%f  score=%f\n",latRingCenter,lonRingCenter,maxRingSize,ringScores[maxRingSize]); */
    /* (void)time(&timeend); */
    /* printf("ringfit time=%f\n",difftime(timeend,timestart)); */
  
    /* printf("fglat=%f fglon=%f  splat=%f splon=%f  rglat=%f rglon=%f\n",taclat,taclon,latSpiralCenter,lonSpiralCenter,latRingCenter,lonRingCenter); */
    /* caluculate confidence factor for combined spiral/ring analyses */
    iok=aodtv72_calcScores(taclat,taclon,
                        latSpiralCenter,lonSpiralCenter,maxSpiralCenter,spGrid1,
                        latRingCenter,lonRingCenter,scoreRingCenter,ringScores,
                        &latFinal,&lonFinal,&scoreFinal,&scoreMethod);
    /* printf("SPIRAL CENTER  :  lat=%f  lon=%f  SCORE=%f  \n",latSpiralCenter,lonSpiralCenter,maxSpiralCenter,scoreMethod); */
    /* printf("RING   CENTER  :  lat=%f  lon=%f  SCORE=%f  \n",latRingCenter,lonRingCenter,scoreRingCenter); */
    /* printf("FINAL  CENTER  :  lat=%f  lon=%f  SCORE=%f  \n",latFinal,lonFinal,scoreFinal); */

    /* free memory */
    for(iy=0;iy<ringSize;iy++) {
      free(ringScores[iy]);
    }
    free(ringScores);
    for(iy=0;iy<(maxd*maxd);iy++) {
      free(spGrid1[iy]);
    }
    free(spGrid1);
    free(areadataRM);

  } else {
    latSpiralCenter=-99.9;
    lonSpiralCenter=-99.9;
    maxSpiralCenter=-1.0;
    latRingCenter=-99.9;
    lonRingCenter=-99.9;
    scoreRingCenter=-1.0;
    latFinal=-99.9;
    lonFinal=-99.9;
    scoreFinal=-99.9;
    scoreMethod=1;
  }

  /* compute Laplacian of scene */
  aodtv72_laplacian(lapvals);
  /* check Laplacian values within search radii and set up analysis arrays */
  for(iy=0;iy<areadata_v72->numy;iy++) {
    for(ix=0;ix<areadata_v72->numx;ix++) {
      aodtv72_distance(odtcurrent_v72->IR.warmlatitude,odtcurrent_v72->IR.warmlongitude,areadata_v72->lat[iy][ix],areadata_v72->lon[iy][ix],1,&xdist,&xangle);
      if(xdist<=(float)searchrad2) {
        if((A_ABS(lapvals[iy][ix])>=(float)threshold2)&&(areadata_v72->temp[iy][ix]>=ebd_v72[2])) {
          templat1[ic1]=areadata_v72->lat[iy][ix];
          templon1[ic1]=areadata_v72->lon[iy][ix];
          ic1++;
        }
        if((A_ABS(lapvals[iy][ix])>=(float)threshold1)&&(areadata_v72->temp[iy][ix]>=ebd_v72[2])) {
          templat2[ic2]=areadata_v72->lat[iy][ix];
          templon2[ic2]=areadata_v72->lon[iy][ix];
          ic2++;
        }
      }
    }
  }

  /* determine stdv and skew of Laplacian analysis for
     two different thresholds.  Will try and pinpoint
     eye/warm spot regions from these values */
  if(ic1>0) {
    aodtv72_calcskew(templat1,ic1,&avelat1,&stdvlat1,&skewlat1);
    aodtv72_calcskew(templon1,ic1,&avelon1,&stdvlon1,&skewlon1);
  } else {
    avelat1=-99.9;
    avelon1=-99.9;
  }
  if(ic2>0) {
    aodtv72_calcskew(templat2,ic2,&avelat2,&stdvlat2,&skewlat2);
    aodtv72_calcskew(templon2,ic2,&avelon2,&stdvlon2,&skewlon2);
  } else {
    avelat2=-99.9;
    avelon2=-99.9;
  }

  /* free memory */
  free(templon2);
  free(templat2);
  free(templon1);
  free(templat1);
  for(iy=0;iy<maxd;iy++) {
    free(lapvals[iy]);
    free(moatFlagField[iy]);
  }
  free(lapvals);
  free(moatFlagField);

  /* load forecast interpolation/extrapolation location and temperature
   * value into variables for location determination scheme */
  tempfcst=odtcurrent_v72->IR.eyet;
  usethis=odtcurrent_v72->IR.autopos;

  aodtv72_pickfinallocation(avelat1,avelon1,stdvlat1,stdvlon1,ic1,
               avelat2,avelon2,stdvlat2,stdvlon2,ic2,
               taclat,taclon,tempfcst,
               latFinal,lonFinal,scoreFinal,scoreMethod,
               &uselat,&uselon,&useconf,&usethis);

  if(uselon>180.0) {
    uselon=uselon-360.0;
  }
/*
  printf("TAC CENTER    :  lat=%f  lon=%f \n",taclat,taclon);
  printf("AUTO CENTER   :  lat=%f  lon=%f  SCORE=%f  METHOD=%d\n",latFinal,lonFinal,scoreFinal,scoreMethod);
  printf("FINAL LOCATION:  lat=%f  lon=%f  SCORE=%f  METHOD=%d\n",uselat,uselon,useconf,usethis);
*/

  /* store final location and determination method in structure */
  *retlat=uselat;
  *retlon=uselon;
  *retpos=usethis;

  return 0;
}

int aodtv72_calcScores(float latFG,float lonFG,
                    float latSP,float lonSP,float scoreSP,float **spGrid,
                    float latRG,float lonRG,float scoreRG,float **ringScores,
                    float *finalLat, float *finalLon,float *finalMax,int *finalType)
/* This routine will determine the confidence scores for the spiral fitting 
   and ring fitting routines and calculate the best possible position for the 
   storm center based upon a combination of the two methods, if available.  If the
   ring fitting routine does not determine a good candidate position, the spiral
   fitting routine will be used alone.  If the spiral fitting routine candidate point
   is not "good", the forecast point will be selected.
   Inputs  : latFG    - First Guess (forecast interpolation) latitude
             lonFG    - First Guess (forecast interpolation) longitude
             latSP    - Spiral Analysis latitude at maximum score location
             lonSP    - Spiral Analysis longitude at maximum score location
             scoreSP  - Spiral Analysis Score value
             spGrid   - Grid of Spiral Analysis scores in analysis region
             latRG    - Ring Analysis latitude at maximum score location
             lonRG    - Ring Analysis longitude at maximum score location
             scoreRG  - Ring Analysis Score value
             ringGrid - Grid of Ring Analysis scores in analysis region
   Outputs : finalLat - Latitude of final selected location
             finalLon - Longitude of final selected location
             finalMax - Confidence Score (value) of final selected location
             finalType- Method used to determine final selected location
                        1-First Guess,4-Enhanced Spiral Analysis,5-Combo Ring/Spiral Analysis
*/
{
  int   method,iy,spnum,rgnum,b,iok;
  float maxFxErr=1.0,expectedMaxFxErr=maxFxErr;
  float maxAllowableDisplacement=expectedMaxFxErr*1.15;
  float SPIRALWEIGHT=10.0,DISTPENALTYWEIGHT=(0.5/expectedMaxFxErr);   /* changed 20 to 10 */
  float PROXIMITYBONUS=4.5,PROXIMITYTHRESH=0.25,COMBOSCORETHRESH=15.0;  /* changed 5.0 to 4.5; changed 0.175 to 0.25 */
  float xdenom=111.0;  /* convert distance in km to degrees */
  float xdist,xangle,spiralPartScore;
  float spiralDistFromGuess,ringDistFromGuess;
  float latOfESPmax,lonOfESPmax,espDistFromGuess,ESPindexMax=-999.0;
  float latOfRingMax,lonOfRingMax,foundRingScore;
  float latMaxComboScore,lonMaxComboScore,comboScore,maxComboScore=-99.0;
  float *distPenalty,*spiralPart,*distFromSpCent,*distBonus,*enhancedSpiralPart;
  float *comboScoreSpiralPart;

  /* allocate memory */
  b=sizeof(float);
  spnum=(int)spGrid[0][0]+1;
  rgnum=(int)ringScores[0][0]+1;
  distPenalty=(float *)calloc((size_t)spnum,b);
  spiralPart=(float *)calloc((size_t)spnum,b);
  distFromSpCent=(float *)calloc((size_t)spnum,b);
  distBonus=(float *)calloc((size_t)spnum,b);
  enhancedSpiralPart=(float *)calloc((size_t)spnum,b);
  comboScoreSpiralPart=(float *)calloc((size_t)spnum,b);

  /* Spiral Score Calculations */
  aodtv72_distance(latFG,lonFG,latSP,lonSP,1,&xdist,&xangle);
  spiralDistFromGuess=xdist/xdenom;
  spiralPartScore=scoreSP;
  for(iy=1;iy<spnum;iy++) {
    aodtv72_distance(latFG,lonFG,spGrid[iy][1],spGrid[iy][2],1,&xdist,&xangle);
    distPenalty[iy]=-DISTPENALTYWEIGHT*(xdist/xdenom);
    spiralPart[iy]=SPIRALWEIGHT*(spGrid[iy][0]-spiralPartScore);
    aodtv72_distance(latSP,lonSP,spGrid[iy][1],spGrid[iy][2],1,&xdist,&xangle);
    distFromSpCent[iy]=xdist/xdenom;
    if(distFromSpCent[iy]<=PROXIMITYTHRESH) {
      distBonus[iy]=PROXIMITYBONUS;
    } else {
      distBonus[iy]=0.0;
    }
    enhancedSpiralPart[iy]=spiralPart[iy]+distPenalty[iy];
    /* printf("spGrid :   iy: %d  Lat: %f  Lon: %f  Value: %f ",iy,spGrid[iy][1],spGrid[iy][2],spGrid[iy][0]); */
    /* printf(" distPentaly=%f  distFromSpCent=%f  distBonus=%f   max=%f\n", distPenalty[iy],distFromSpCent[iy],distBonus[iy],ESPindexMax); */
    if(enhancedSpiralPart[iy]>ESPindexMax) {
      ESPindexMax=enhancedSpiralPart[iy];
      latOfESPmax=spGrid[iy][1];
      lonOfESPmax=spGrid[iy][2];
    }
    comboScoreSpiralPart[iy]=spiralPart[iy]+distPenalty[iy]+distBonus[iy];
  }
  /* printf("latFG=%f  lonFG=%f   latOfESPmax=%f  lonOfESPmax=%f\n",latFG,lonFG,latOfESPmax,lonOfESPmax); */
  aodtv72_distance(latFG,lonFG,latOfESPmax,lonOfESPmax,1,&xdist,&xangle);
  espDistFromGuess=xdist/xdenom;
  
  /* printf("espDistFromGuess=%f  maxAllowableDisplacement=%f\n",espDistFromGuess,maxAllowableDisplacement); */
  if(espDistFromGuess<=maxAllowableDisplacement) {
    /* Ring Score Calculations */
    aodtv72_distance(latFG,lonFG,latRG,lonRG,1,&xdist,&xangle);
    ringDistFromGuess=xdist/xdenom;
  
    latOfRingMax=latRG;
    lonOfRingMax=latRG;
    
    for(iy=1;iy<spnum;iy++) {
      iok=aodtv72_findRingScore(spGrid[iy][1],spGrid[iy][2],ringScores,&foundRingScore);
      if(iok==1) {
        comboScore=comboScoreSpiralPart[iy]+foundRingScore;
        /* printf("    FOUND  spnum=%d  lat=%f  lon=%f  ringScore=%f  comboScore=%f  maxComboScore=%f\n",iy,spGrid[iy][1],spGrid[iy][2],foundRingScore,comboScore,maxComboScore); */
        if(comboScore>maxComboScore) {
          maxComboScore=comboScore;
          latMaxComboScore=spGrid[iy][1];
          lonMaxComboScore=spGrid[iy][2];
        }
      }
    }
    /* printf("maxComboScore=%f   RingPart=%f   ESPindexMax=%f\n",maxComboScore,foundRingScore,ESPindexMax); */
    if(maxComboScore>=COMBOSCORETHRESH) {
      /* use Combo Method */
      *finalMax=maxComboScore;
      *finalLat=latMaxComboScore;
      *finalLon=lonMaxComboScore;
      *finalType=5;
    } else {
      /* use ESP method */
      *finalMax=1.0+ESPindexMax;
      *finalLat=latOfESPmax;
      *finalLon=lonOfESPmax;
      *finalType=4;
    }
  } else {
    /* use Forecast Position */
    *finalMax=0.0;
    *finalLat=latFG;
    *finalLon=lonFG;
    *finalType=1;
  }

  /* free memory */
  free(distPenalty);
  free(spiralPart);
  free(distFromSpCent);
  free(distBonus);
  free(enhancedSpiralPart);
  free(comboScoreSpiralPart);

  return 0;
}

int aodtv72_findRingScore(float lat,float lon,float **ringScores,float *value)
/* Find Ring score at selected location (spiral analysis location)
   Inputs  : lat        - latitude of search location
             lon        - longitude of search location
             ringScores - Array/Grid of Ring Analysis scores
   Outputs : value      - Value at search location in Ring Analysis Grid (if found)
   Return  : -1 if not found, 1 if found
*/
{
  int iyy=1,iret=-1,a;
  float incy,incx;
  logical found=FALSE;

  a=ringScores[0][0];
  *value=-99.9;
  incy=A_ABS(areadataRM->lat[0][0]-areadataRM->lat[1][0])/2.0;
  incx=A_ABS(areadataRM->lon[0][1]-areadataRM->lon[0][0])/2.0;
  while(iyy<=a) {
    if((A_ABS(ringScores[iyy][1]-lat)<=incy)&&(A_ABS(ringScores[iyy][2]-lon)<=incx)) {
      if(ringScores[iyy][0]>*value) *value=ringScores[iyy][0];
      iret=1;
    }
    iyy++;
  }
 
  return iret;
}

int aodtv72_readforecast( FILE *fp,int *fnum )
/* Read the forecast file and obtain the forecast positions
   for interpolation of the storm position
    Inputs  : fp      - file pointer
    Outputs : fnum    - number of points used in forecast interpolation
              return value : -4 for error reading forecast file
                             -5 for invalid forecast file
                              0 for good read of forecast file
              global variables : fcsttime_v72, fcstlat_v72, fcstlon_v72
                                 arrays containing previous, current,
                                 and forecast times/positions for use
                                 in quadratic interpolation scheme
                                 [0] - 12 hour old time/postion (if available)
                                 [1] - 6 hour old time/postion (if available)
                                 [2] - current time/postion
                                 [3] - forecast time/postion
                                 [4] - forecast time/postion
*/
{
  char line[200];
  char l1[30],l2[30],l3[30],l4[30],l5[30],l6[30],l7[30],l8[30],l1a[30];
  int maxline=200,ixx,iyy,izz1,izz2,iy,iz,maxfcst;
  int imon,imonx,iyear,iyearx,idate,idatex,julday,juldayx;
  int ftime,ftimelast,itime;
  float lat[6],lon[6];
  double curtime,time[6],xtime,ttime;
  logical getmonth=FALSE,findyear=TRUE;

  curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);
  ixx=0;
  iyy=0;
  if(ifixtype_v72==1) {  /* NHC */
    /* read NHC forecast file, quit at EOF */
    while(((fgets(line,maxline,fp))!=NULL)&&(ixx<6)) {
      strcpy(l6," ");
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
        idatex=(int)aodtv72_atoif(l6,1,2);
        iyearx=(int)aodtv72_atoif(l7,1,4);
        while((strncmp(l5,cmon_v72[imonx],3)!=0)&&(imonx<12)) { imonx++; }
        juldayx=aodtv72_idmyyd(idatex,imonx+1,iyearx);
        getmonth=TRUE;
      }
      imon=imonx;
      iyear=iyearx;
      if(!strncmp(l6,"KT",2)) {
        if((!strncmp(l1,"INITIAL",7))||(!strncmp(l1a,"VT",2))) {
          idate=(int)aodtv72_atoif(l2,1,7)/10000;
          itime=((int)aodtv72_atoif(l2,1,7)%10000)*100;
          if(idate<idatex) {
            imon++;
            if(imon==12) {
              iyear++;
              imon=0;
            }
            julday=aodtv72_idmyyd(idate,imon+1,iyear);
          } else {
            julday=juldayx+(idate-idatex);
          }
          time[ixx]=aodtv72_calctime(julday,itime);
          lat[ixx]=aodtv72_atoif(l3,1,strlen(l3));
          lon[ixx]=aodtv72_atoif(l4,1,strlen(l4));
          ixx++;
        }
      }
    }
  } else if(ifixtype_v72==2) {
    /* read JTWC forecast file, quit at EOF */
    /* o.k... since JTWC does not put the month and year
       on their bulletins, they will need to be "made up".
       We will assume that the forecast is current and the
       month and year from the image is equal to the month
       and year of the forecast file.  */
    (void)aodtv72_yddmy((int)curtime,&idatex,&imonx,&iyearx);
    while(((fgets(line,maxline,fp))!=NULL)&&(ixx<6)) {
      (void)sscanf(line,"%s %s %s %s %s",l1,l2,l3,l4,l5);
      if(!strncmp(l2,"---",3)) {
        if(ixx==0) {
          idate=(int)aodtv72_atoif(l1,1,6)/10000;
          itime=((int)aodtv72_atoif(l1,1,6)%10000)*100;
          if(idate==idatex) {
            /* dates are the same... probably o.k.... should check times */
            julday=(int)curtime;
          } else if((idatex-idate)<=2) {
            /* this is probably o.k too... forecast date is before image by one day */
            julday=(int)curtime+(idate-idatex);
          } else {
            /* dates are invalid.  Either image date is before forecast or
               is well beyond forecast availability */
            return -2;
          } 
          time[ixx]=aodtv72_calctime(julday,itime);
          lat[ixx]=aodtv72_atoif(l4,1,strlen(l4));
          lon[ixx]=aodtv72_atoif(l5,1,strlen(l5));
        } else {
          idate=(int)aodtv72_atoif(l1,1,6)/10000;
          itime=((int)aodtv72_atoif(l1,1,6)%10000)*100;
          julday=(int)curtime+(idate-idatex);
          time[ixx]=aodtv72_calctime(julday,itime);
          if (time[ixx]<time[ixx-1]) {
            imon=imonx+1;
            iyear=iyearx;
            if(imon==13) {
              iyear=iyearx+1;
              imon=1;
            }
            julday=aodtv72_idmyyd(idate,imon,iyear);
            time[ixx]=aodtv72_calctime(julday,itime);
          }
          lat[ixx]=aodtv72_atoif(l3,1,strlen(l3));
          lon[ixx]=aodtv72_atoif(l4,1,strlen(l4));
        }
      }
      if((!strncmp(l1,"MAX",3))&&(!strncmp(l4,"-",1))) {
        ixx++;
      }
    }
  } else if(ifixtype_v72==3) {
    /* generic forecast file input */
    while(((fgets(line,maxline,fp))!=NULL)&&(ixx<6)) {
      (void)sscanf(line,"%s %s %s %s %s %s",l1,l2,l3,l4,l5,l6);
      idate=(int)aodtv72_atoif(l1,1,2);
      imon=(int)aodtv72_atoif(l2,1,2);
      iyear=(int)aodtv72_atoif(l3,1,4);
      itime=(int)aodtv72_atoif(l4,1,4)*100;
      julday=aodtv72_idmyyd(idate,imon,iyear);
      time[ixx]=aodtv72_calctime(julday,itime);
      lat[ixx]=aodtv72_atoif(l5,1,strlen(l5));
      lon[ixx]=aodtv72_atoif(l6,1,strlen(l6));
      ixx++;
    }
  } else if(ifixtype_v72==0) {
    /* ATCF forecast file input */
    while(((fgets(line,maxline,fp))!=NULL)) {
      (void)sscanf(line,"%s %s %s %s %s %s %s %s",l1,l2,l3,l4,l5,l6,l7,l8);
      iyear=(int)aodtv72_atoif(l3,1,4);
      imon=(int)aodtv72_atoif(l3,5,6);
      idate=(int)aodtv72_atoif(l3,7,8);
      itime=(int)aodtv72_atoif(l3,9,10)*10000;
      ftime=(int)aodtv72_atoif(l6,1,strlen(l6))*10000;
      julday=aodtv72_idmyyd(idate,imon,iyear);
      ttime=aodtv72_calctime(julday,itime);
      if((ttime<curtime)&&(ftimelast!=ftime)&&(iyy<6)) {
        izz1=(int)(ftime+itime)/240000;
        izz2=((int)(ftime+itime)%240000);
        time[iyy]=aodtv72_calctime(julday+izz1,izz2);
        lat[iyy]=aodtv72_atoif(l7,1,strlen(l7))/10.0;
        lon[iyy]=aodtv72_atoif(l8,1,strlen(l8))/10.0;
        iyy++;
        ftimelast=ftime;
        ixx=iyy;
      }
    }
  } else if(ifixtype_v72==9) {
    /* best track file input */
    while(((fgets(line,maxline,fp))!=NULL)&&(ixx<6)) {
      if(findyear) {
        (void)sscanf(line,"%s %s",l1,l2);
        iyear=(int)aodtv72_atoif(l2,1,4);
        findyear=FALSE;
        /* printf("year=%d\n",iyear); */
      } else {
        (void)sscanf(line,"%s %s %s %s %s %s",l1,l2,l3,l4,l5,l6);
        imon=(int)aodtv72_atoif(l1,1,2);
        idate=(int)aodtv72_atoif(l1,4,5);
        itime=(int)aodtv72_atoif(l1,7,8)*10000+(int)aodtv72_atoif(l1,10,11)*100;
        julday=aodtv72_idmyyd(idate,imon,iyear);
        xtime=aodtv72_calctime(julday,itime);
	if(xtime<curtime) {
          time[0]=xtime;
          lat[0]=aodtv72_atoif(l2,1,strlen(l2));
          lon[0]=aodtv72_atoif(l3,1,strlen(l3));
        } else {
	  if((xtime>=curtime)&&(ixx<4)) { 
            ixx++;
            time[ixx]=xtime;
            lat[ixx]=aodtv72_atoif(l2,1,strlen(l2));
            lon[ixx]=aodtv72_atoif(l3,1,strlen(l3));
	  }
        }
      }
    }
  } else {
    return -1;
  }
  ixx--;

  /* determine number of valid forecast times for given image time */
  iy=0;
  maxfcst=A_MIN(ixx,3);
  while((time[iy]<=curtime)&&(iy<maxfcst)) {
    iy++;
  }

  if((iy==0)||(iy==maxfcst)) {
    /* image before/after forecast time */
    return -2;
  }

  /* initialize forecast info array */
  for(iz=0;iz<5;iz++) {
    fcsttime_v72[iz]=9999999.9;
    fcstlat_v72[iz]=999.0;
    fcstlon_v72[iz]=999.0;
  }

  for(iz=0;iz<maxfcst;iz++) {
    /* load forecast times into return arrays */
    fcsttime_v72[iz]=time[iz];
    fcstlat_v72[iz]=lat[iz];
    if((lon[0]>0.0)&&(lon[iz]<0.0)) {
      /* dateline cross */
      fcstlon_v72[iz]=lon[iz]+360.0;
    } else {
      fcstlon_v72[iz]=lon[iz];
    }
  }
  *fnum=maxfcst;
  return 0;
}

void aodtv72_pickfinallocation(float lplat1,float lplon1,float lplats1,float lplons1,int lpnum1,
                  float lplat2,float lplon2,float lplats2,float lplons2,int lpnum2,
                  float nhclat,float nhclon,float nhctemp,
                  float splat,float splon,float spscore,int spmethod,
                  float *uselat,float *uselon,float *useconf,int *usethis)
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
              splat   - Ring/Spiral Analysis latitude position
              splon   - Ring/Spiral Analysis longitude position
              spscore - Ring/Spiral Analysis confidence factor score
              spmethod- Ring/Spiral Analysis position derivation method
              nhclat  - NHC/JTWC interpolated latitude position
              nhclon  - NHC/JTWC interpolated longitude position
              nhctemp - NHC/JTWC interpolated temperature value
    Outputs : uselat  - latitude value to be used
              uselon  - longitude value to be used
              usethis - method utilized in determination of storm postion values
*/
{
  int   warmcat,nhccat,diffcat,ixx,origpos,count;
  int   lastescene,lastcscene;
  float xdist,xangle,cescene,ccscene;
  float lastci,lasttno,lastrawT,maxci;
  float laplsd1,laplsd2,laplss1,laplss2;
  float conflapl,conf1,conf2,confdist;
  float laplat,laplon;
  struct odtdata *odthistory;
  double curtime,xtime;
  logical landcheck,foundeye=FALSE,forceauto=FALSE;

  maxci=0.0;
  curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);
  cescene=odtcurrent_v72->IR.eyescene;
  ccscene=odtcurrent_v72->IR.cloudscene;
  odthistory=odthistoryfirst_v72;
  /* if(hfile==(char *)NULL) {*/
  /* if((strlen(hfile)==0)) { */
  if((strlen(hfile_v72)==0)||(forceauto)) {
     lasttno=9.0; 
     maxci=9.0; 
     lastrawT=9.0; 
  } else if(odthistoryfirst_v72==0) {
     lasttno=osstr_v72;
     maxci=osstr_v72; 
     lastrawT=osstr_v72; 
  } else {
    count=0;
    lasttno=odtcurrent_v72->IR.Traw; 
    lastrawT=odtcurrent_v72->IR.Traw; 
    maxci=odtcurrent_v72->IR.Traw; 
    while(odthistory!=0) {
      xtime=aodtv72_calctime(odthistory->IR.date,odthistory->IR.time);
      landcheck=TRUE;
      if(((oland_v72)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) landcheck=FALSE;
      if((xtime<curtime)&&(landcheck)) {
        lastci=odthistory->IR.CI;
        lasttno=odthistory->IR.Tfinal;
        lastrawT=odthistory->IR.Traw;
        lastescene=odthistory->IR.eyescene;
        lastcscene=odthistory->IR.cloudscene;
        if (lastci>maxci) maxci=lastci;
        if((lastescene<3)||((lastcscene==1)&&(lastescene==3))) {
          count=count+1;
          /* checking for eye or embedded center scene types */
          if(count==3) foundeye=TRUE;
        } else {
          count=0;
        }
      }
      odthistory=odthistory->nextrec;
    }
    if(((cescene<3)||((ccscene==1)&&(cescene==3)))&&(count==2)) foundeye=TRUE;
  }
  origpos=*usethis;
  /* printf("%d %d : ",odtcurrent_v72->IR.date,odtcurrent_v72->IR.time); */
  /* printf("spscore=%f lasttno=%f ",spscore,lasttno); */
  /* check score for developing systems (maxci<5.0) starting at 3.0 or
     check score for weakeining systems (maxci>=5.0) only above 3.5 */
  if(((lastrawT>=3.0)&&(maxci<5.0))||
     ((lastrawT>=3.5)&&(maxci>=5.0))) {

    /* if((lasttno<=4.5)&&(!foundeye)) spscore=-99.0; */
    if((lasttno<=4.5)&&(spscore<1.0)&&(!foundeye)) spscore=-99.0;
    /* printf(" FINALspscore=%f ",spscore); */

    /* compare laplacian score with Spiral/Ring Analysis confidence factor/score */
    if(spscore>0.0) {
    /* if(spscore>conflapl) { */
      /* use Spiral/Ring methodology for center point */
      *uselat=splat;
      *uselon=splon;
      *useconf=spscore;
      *usethis=spmethod;
    } else {
      /* CDO... can't find anything to focus on */
      *usethis=origpos;
      *uselat=nhclat;
      *uselon=nhclon;
      *useconf=0.0;
    }
  } else {
    /* current Tfinal is less than 3.5 or current scene is not an eye or embedded center
     * WILL USE FORECAST POSITION FOR AODT ANALYSIS */
    *usethis=origpos;
    *uselat=nhclat;
    *uselon=nhclon;
    *useconf=0.0;
  }

}

void aodtv72_logspiral(float inlat,float inlon,float searchtemp,int searchtype,
                    int *bestspiral,float *bestlat,float *bestlon)
/* Determine storm location using 10^ Log-spiral analysis.
   Algorithm will attempt to match the spiral with the image
   pixels at or below the threshold temperature based on 
   BD-enhancement curve values 
    Inputs  : inlat      - center latitude of analysis grid
              inlon      - center longitude of analysis grid
              searchtemp - temperature threshold value
              searchtype - 1=search at single point;2=search over 2^box
    Outputs : bestlat    - best latitude location from analysis
              bestlon    - best longitude location from analysis
              bestspiral - number of consecutive arcs through which spiral passes
*/
{
  int ixx,iyy,izz,iskip,rotfac,theta,b;
  int imaxx,iminx,imaxy,iminy,ycount;
  int spiralconsec,maxconsec,spiralbest,spiralbestrot,bestrot;
  float xrad=57.29578,A=25.0,B=10.0/xrad;
  float maxx,minx,maxy,miny,xmindist;
  float glat,glon,xlat,xlon,xdist,xangle;
  float ylatdiff,ylondiff,xres=6.0;
  float thetax,r,thetaval,thetaval2;
  /* float zlat[bufsiz],zlon[bufsiz];*/
  float *zlat,*zlon,lres;
  int np,ipt;

  if(odtcurrent_v72->IR.sattype==5) xres=12.0;
  /* allocate memory */
  b=sizeof(float);
  zlat=(float *)calloc((size_t)bufsiz,b);
  zlon=(float *)calloc((size_t)bufsiz,b);
  if(searchtype==2) {
    /* search over 2.0 degree box */
    maxx=inlat+1.0;
    minx=inlat-1.0;
    maxy=inlon+1.0;
    miny=inlon-1.0;
    imaxx=(int)(maxx*100.0);
    iminx=(int)(minx*100.0);
    imaxy=(int)(maxy*100.0);
    iminy=(int)(miny*100.0);
  } else {
    /* search at a single point */
    imaxx=(int)(inlat*100.0);
    iminx=(int)(inlat*100.0);
    imaxy=(int)(inlon*100.0);
    iminy=(int)(inlon*100.0);
  }

  *bestspiral=0;

  /* initialize arrays */
  np=0;
  for(ixx=0;ixx<areadata_v72->numx;ixx++) {
    for(iyy=0;iyy<areadata_v72->numy;iyy++) {
      if(areadata_v72->temp[iyy][ixx]<=searchtemp) {
        zlat[np]=areadata_v72->lat[iyy][ixx];
        zlon[np]=areadata_v72->lon[iyy][ixx];
        np++;
      }
    }
  }

  /* loop through x-axis/elements of analysis grid box */
  for(ixx=iminx;ixx<=imaxx;ixx=ixx+20) {
    xlat=(float)ixx/100.0;
    /* loop through y-axis/lines of analysis grid box */
    for(iyy=iminy;iyy<=imaxy;iyy=iyy+20) {
      xlon=(float)iyy/100.0;
      iskip=0;
      /* determine distance from each point in box to current location */
      if(searchtype==2) {
        xmindist=12.0;
        for(izz=0;izz<np;izz++) {
          aodtv72_distance(xlat,xlon,zlat[izz],zlon[izz],1,&xdist,&xangle);
          if(xdist<=xmindist) { 
            /* if the lat/lon point is too close to cold cloud tops, do
               not calculate log spiral at this point.  Trying to eliminate
               "false" arc locations by forcing the system to use some
               of the arc points on the spiral away from the start of
               the spiral (were getting "false echos" without this". */
            iskip=1;
            break;
          }
        }
      }

      spiralbest=0;
      spiralbestrot=0;
      /* if arc location passes analysis above, proceed with placement of spiral */
      if(iskip==0) { 
        /* rotate the arc spiral thru entire revolution at 30 degree intervals */
        for(rotfac=0;rotfac<=330;rotfac=rotfac+30) {
          spiralconsec=0;
          maxconsec=0;

          /* calculate position of each point on spiral from 0 to 540^ */
          for(theta=0;theta<=540;theta=theta+15) {
            thetax=(float)theta/xrad;
            r=A*A_EXP((B*thetax));
            thetaval=(float)theta+(float)rotfac;
            if(xlat<0.0) thetaval=(float)(-1*theta)+(float)rotfac;
            thetaval2=thetaval+180.0;
            aodtv72_distance2(xlat,xlon,r,thetaval2,&glat,&glon);
            ycount=0;
            for(izz=0;izz<np;izz++) {
              ylatdiff=A_ABS(glat-zlat[izz]);
              ylondiff=A_ABS(glon-zlon[izz]);
              /* if a point is within 0.1^ latitude/longitude determine distance */
              if((ylatdiff<=0.1)&&(ylondiff<=0.1)) {
                aodtv72_distance(glat,glon,zlat[izz],zlon[izz],1,&xdist,&xangle);
                /* if distance from spiral point is within 6km from an accepted
                   temperature threshold point, count it */
                if(xdist<=xres) ycount++;
              }
            }
            /* if there are 4 or more threshold points associated with each 
               spiral point, count within consecutive spiral point counter */
            if(ycount>=4) {
              spiralconsec++;
              /* save spiral that has the maximum consecutive spiral counts
                 for each rotation though 360^ at each center location */
              if(spiralconsec>maxconsec) maxconsec=spiralconsec;
            } else {
              spiralconsec=0;
            }
            /* if this spiral has the greatest number of consecutive spiral
               points, save the location and number of points */
            if(maxconsec>spiralbest) {
              spiralbest=maxconsec;
              spiralbestrot=rotfac;
            }
          }
        } /* rotfac loop */
        if(spiralbest>*bestspiral) {
          *bestspiral=spiralbest;
          *bestlat=xlat;
          *bestlon=xlon;
          bestrot=spiralbestrot;
        }
      } /* iskip if */
    } /* iyy loop */
  } /* ixx loop */

  /* free memory */
  free(zlon);
  free(zlat);

  /* load array for best spiral band */
  ipt=0;
  for(theta=0;theta<=540;theta=theta+15) {
    thetax=(float)theta/xrad;
    r=A*A_EXP((B*thetax));
    thetaval=(float)theta+(float)bestrot;
    if(xlat<0.0) thetaval=(float)(-1*theta)+(float)rotfac;
    thetaval2=thetaval+180.0;
    aodtv72_distance2(*bestlat,*bestlon,r,thetaval2,&glat,&glon);
    /* load array for external plotting of spiral band */
    spiralband_v72[0][ipt]=glat;
    spiralband_v72[1][ipt]=glon;
    ipt++;
  }
}

void aodtv72_laplacian(float **data2)
/* Compute Laplacian values for scene.
   Derived from program laplacian.c by John Gauch and Edu Metz
   of the University of Kansas (1994).
    Inputs  : none 
    Outputs : data2 - array containing Laplacian values for each point
*/
{
  int x,y;
  float dxx,dyy;

  for(y=1;y<areadata_v72->numy-1;y++) {
    for(x=1;x<areadata_v72->numx-1;x++) {
/*  dx and dy not used 
      dx = (areadata_v72->temp[y+1][x+1] + (2.0*areadata_v72->temp[y][x+1]) +
            areadata_v72->temp[y-1][x+1] - areadata_v72->temp[y+1][x-1] +
            (2.0*areadata_v72->temp[y][x-1]) - areadata_v72->temp[y-1][x-1])/8.0;
      dy = (areadata_v72->temp[y+1][x+1] + (2.0*areadata_v72->temp[y+1][x]) +
            areadata_v72->temp[y+1][x-1] - areadata_v72->temp[y-1][x+1] +
            (2.0*areadata_v72->temp[y-1][x]) - areadata_v72->temp[y-1][x-1])/8.0;
*/
      dxx = (areadata_v72->temp[y+1][x+1] + (4.0*areadata_v72->temp[y][x+1]) +
             areadata_v72->temp[y-1][x+1] - (2.0*areadata_v72->temp[y+1][x]) -
             (8.0*areadata_v72->temp[y][x]) - (2.0*areadata_v72->temp[y-1][x]) +
             areadata_v72->temp[y+1][x-1] + (4.0*areadata_v72->temp[y][x-1]) +
             areadata_v72->temp[y-1][x-1])/6.0;
      dyy = (areadata_v72->temp[y+1][x+1] - (2.0*areadata_v72->temp[y][x+1]) +
             areadata_v72->temp[y-1][x+1] + (4.0*areadata_v72->temp[y+1][x]) -
             (8.0*areadata_v72->temp[y][x]) + (4.0*areadata_v72->temp[y-1][x]) +
             areadata_v72->temp[y+1][x-1] - (2.0*areadata_v72->temp[y][x-1]) +
             areadata_v72->temp[y-1][x-1])/6.0;
      /* calculate directional derivative */
      /* data3[y][x]=(int)(A_SQRT((dx*dx)+(dy*dy))); */
      data2[y][x]=dxx+dyy;
    }
  }

  /* handle boundary rows and columns */
  for(x=0;x<areadata_v72->numx;x++) {
    data2[0][x]=data2[1][x];
    data2[areadata_v72->numy-1][x]=data2[areadata_v72->numx-2][x];
  }
  for(y=0;y<areadata_v72->numy;y++) {
    data2[y][0]=data2[y][1];
    data2[y][areadata_v72->numx-1]=data2[y][areadata_v72->numx-2];
  }

}

int aodtv72_polint(double *xa,float *ya,int n,double x,float *y,float *dy)
/* Polynomial interpolation scheme program derived from FORTRAN 
   program POLINT in : Numerical Recipies - The Art of Scientific 
   Computing, 1986, Press, Flannery, Teukolsky, and Vetterling, Cambridge Press
*/
{
  float c[5],d[5],dif,dift,hp,ho,w,den,dyx,yx;
  int   ns,i,m;

  ns=1;
  dif=A_ABS(x-xa[0]);
  for(i=1;i<=n;i++) {
    dift=A_ABS(x-xa[i-1]);
    if(dift<dif) {
      ns=i;
      dif=dift;
    }
    c[i-1]=ya[i-1];
    d[i-1]=ya[i-1];
  }
  yx=ya[ns-1];
  ns--;
  for(m=1;m<=n-1;m++) {
    for(i=1;i<=n-m;i++) {
      ho=xa[i-1]-x;
      hp=xa[(i-1)+m]-x;
      w=c[i]-d[i-1];
      den=ho-hp;
      if(den==0.0) return -1;
      den=w/den;
      d[i-1]=hp*den;
      c[i-1]=ho*den;
    }
    if((2*ns)<(n-m)) {
      dyx=c[ns];
    } else {
      dyx=d[ns-1];
      ns--;
    }
    yx=yx+dyx;
  }
  *dy=dyx;
  *y=yx;

  return 0;
}

int aodtv72_spiralCenterLowRes(float nhcLat,float nhcLon,
                           float *spiralLatCenter,float *spiralLonCenter,float *spiralScore,float **spGrid)
/* Perform Tony Wimmers' Spiral Analysis.  Analysis is a 5-degree Log Spiral (not sure why he does not
   use a 10-degree method.
   Inputs  : nhcLat          - First Guess latitude position (from interpolated forecast)
             nhcLon          - First Guess longitude position (from interpolated forecast)
   Outputs : spiralLatCenter - Spiral Analysis latitude position at analysis grid maxiumum score value
             spiralLonCenter - Spiral Analysis longitude position at array/grid maxiumum score value
             spiralScore     - Spiral Analysis maxiumum score value
             spGrid          - Array containting Spiral Analysis grid field
*/
{
  float pi=3.141592;
  float alpha=5.0*pi/180.0;
  float alphapowp1;
  float outSideFactor=0.62;

  int   iok,ixx,iyy,b,bptr,yx=1;
  int   numPoints;
  float xOff,yOff,asign,lonMult;
  float inFilterDisc,gradOrigMag,gradLogMag,gradLogReduction,denom;
  float maxCenterMeanCross,allCenterMeanCross,maxCenterMeanCrossX,maxCenterMeanCrossY;
  float proxyX,proxyY,spiralX,spiralY,rawCrossScore,crossScoreClean,crossScoreSum,searchRad,searchRadMax;
  float latInc,lonInc,distX,distY;
  float **gradN,**gradE;
  float filterRadiusDeg=4.5;
  float searchRadiusDeg,spacingDegC,spacingDegF;
  float xFGmin,xFGmax,yFGmin,yFGmax;
  struct   datagrid *areadata_v72NC;
      time_t timestart,timeend; 
      int aa=0,aa1=0,bb=0,bb1=0;

  searchRadiusDeg=1.75;
  spacingDegC=0.2;
  spacingDegF=0.1;
  inFilterDisc=A_POW(searchRadiusDeg+(2.0*spacingDegF),2);
  alphapowp1=1.0+A_POW(alpha,2);

  asign=A_ABS(nhcLat)/nhcLat;

  iyy=(areadataRM->numy)-1;
  if((nhcLon<0.0)&&((areadataRM->lon[0][0]>180.0)||(areadataRM->lon[iyy][0]>180.0))) {
    /* printf("DATELINE CROSS... changing nhcLon from %f",nhcLon); */
    nhcLon=nhcLon+360.0;
    /* printf(" to %f\n",nhcLon); */
  }
  /* define data grid around first guess */
  areadata_v72NC=(struct datagrid *)malloc(sizeof(struct datagrid));
  for(iyy=0;iyy<areadataRM->numy;iyy++) {
    for(ixx=0;ixx<areadataRM->numx;ixx++) {
      /* compute normalized coordinate system arrays */
      areadata_v72NC->lon[iyy][ixx]=(areadataRM->lon[iyy][ixx]-nhcLon)*(A_COS(pi*nhcLat/180.0));
      areadata_v72NC->lat[iyy][ixx]=areadataRM->lat[iyy][ixx]-nhcLat;
      areadata_v72NC->temp[iyy][ixx]=areadataRM->temp[iyy][ixx];
      /* printf("iyy=%d ixx=%d  lat=%f lon=%f  lat=%f lon=%f  temp=%f\n",iyy,ixx,areadataRM->lat[iyy][ixx],areadataRM->lon[iyy][ixx],areadata_v72NC->lat[iyy][ixx],areadata_v72NC->lon[iyy][ixx],areadataRM->temp[iyy][ixx]); */
    }
  }
  areadata_v72NC->numx=areadataRM->numx;
  areadata_v72NC->numy=areadataRM->numy;

  /* determine lat/lon grid increment */
  lonInc=A_ABS(areadata_v72NC->lon[0][0]-areadata_v72NC->lon[0][1]);    /* W to E gradient */
  latInc=A_ABS(areadata_v72NC->lat[0][0]-areadata_v72NC->lat[1][0]);    /* N to S gradient */
  /* This is to determine longitude multiplier factor... original routines were developed 
     using negative Western Hemisphere, but McIDAS is positive in WH.  So if lonMult is negative,
     we know (assume) we are using non-McIDAS input imagery/values, otherwise make lonMult positive.
     This all assumes that image is loaded from NW to SE */
  lonMult=areadata_v72NC->lon[0][0]-areadata_v72NC->lon[0][1];
  lonMult=(lonMult<0.0) ? 1.0 : -1.0;
  /* printf("latInc=%f  lonInc=%f  lonMult=%f\n",latInc,lonInc,lonMult); */

  /* allocate memory */
  b=sizeof(float);
  bptr=sizeof(float*);
  gradN=(float **)calloc((size_t)maxd,bptr);
  gradE=(float **)calloc((size_t)maxd,bptr);
  for(iyy=0;iyy<maxd;iyy++) {
    gradN[iyy]=(float *)calloc((size_t)maxd,b);
    gradE[iyy]=(float *)calloc((size_t)maxd,b);
  }

  /* calculate gradient field */
  iok=aodtv72_gradient(areadata_v72NC->temp,areadata_v72NC->numx,areadata_v72NC->numy,lonInc,latInc,gradN,gradE);
  for(ixx=0;ixx<areadata_v72NC->numx;ixx++) {
    for(iyy=0;iyy<areadata_v72NC->numy;iyy++) {
       /* printf("iyy=%d ixx=%d  lat=%f lon=%f  lat=%f lon=%f  gradN=%f gradY=%f\n",iyy,ixx,areadataRM->lat[iyy][ixx],areadataRM->lon[iyy][ixx],areadata_v72NC->lat[iyy][ixx],areadata_v72NC->lon[iyy][ixx],gradN[iyy][ixx],gradE[iyy][ixx]); */
      gradOrigMag=A_SQRT(A_POW(gradN[iyy][ixx],2)+A_POW(gradE[iyy][ixx],2));
      gradLogMag=A_LOG(1.0+gradOrigMag);
      if(gradLogMag==0.0) {
        gradLogReduction=0.0;
      } else {
        gradLogReduction=gradLogMag/gradOrigMag;
      }
      gradN[iyy][ixx]=gradLogReduction*gradN[iyy][ixx];
      gradE[iyy][ixx]=gradLogReduction*gradE[iyy][ixx];
    }
  }

  /* COURSE GRID */
  /* calculate cross product score at each grid point */
  /* ixx/iyy are "starting point" coordinates */
 (void)time(&timestart);
  maxCenterMeanCross=-99.0;
  searchRadMax=A_POW(searchRadiusDeg+((2.0*spacingDegC)/3.0),2);
  for(xOff=-searchRadiusDeg;xOff<=searchRadiusDeg;xOff=xOff+spacingDegC) {
    for(yOff=-searchRadiusDeg;yOff<=searchRadiusDeg;yOff=yOff+spacingDegC) {
      /* xOff/yOff are offset coordinates from "starting point" */
      searchRad=A_POW(xOff,2)+A_POW(yOff,2);
      if(searchRad<=searchRadMax) {
        crossScoreSum=0.0;
	numPoints=0;
        for(iyy=1;iyy<areadata_v72NC->numy-1;iyy++) {
          for(ixx=1;ixx<areadata_v72NC->numx-1;ixx++) {
            searchRad=A_POW(areadata_v72NC->lon[iyy][ixx],2)+A_POW(areadata_v72NC->lat[iyy][ixx],2);
            if(searchRad<inFilterDisc) {
              proxyX=lonMult*areadata_v72NC->lon[iyy][ixx]-xOff;
              proxyY=areadata_v72NC->lat[iyy][ixx]-yOff;
              denom=A_SQRT((alphapowp1)*(A_POW(proxyX,2)+A_POW(proxyY,2)));
              spiralX=(alpha*proxyX+(asign*proxyY))/denom;
              spiralY=(alpha*proxyY-(asign*proxyX))/denom;
              rawCrossScore=(spiralX*gradN[iyy][ixx])-(spiralY*gradE[iyy][ixx]);
              crossScoreClean=A_MAX(0.0,-rawCrossScore)+(outSideFactor*A_MAX(0.0,rawCrossScore));
	      crossScoreSum=crossScoreSum+crossScoreClean;
   /* printf("lat=%f lon=%f  xoff=%f yoff=%f  proxyX=%f proxyY=%f  spiralX=%f spiralY=%f  rawScore=%f clean=%f\n",areadataRM->lat[iyy][ixx],areadataRM->lon[iyy][ixx],xOff,yOff,proxyX,proxyY,spiralX,spiralY,rawCrossScore,crossScoreClean); */
	      numPoints++; 
            }
          }
        }
        allCenterMeanCross=crossScoreSum/(float)numPoints;   /* calculate mean of all values in crossScore array */
	/* store location of maximum score position */
	if(allCenterMeanCross>maxCenterMeanCross) {
          maxCenterMeanCross=allCenterMeanCross;
          maxCenterMeanCrossY=yOff;
          maxCenterMeanCrossX=xOff;
        }
      }
    }
  }

  /* printf("course grid : y=%f x=%f max=%f\n",maxCenterMeanCrossY,maxCenterMeanCrossX,maxCenterMeanCross); */
  (void)time(&timeend);
/*  printf("course grid time=%f\n",difftime(timeend,timestart)); */

  xFGmin=maxCenterMeanCrossX-spacingDegC;
  xFGmax=maxCenterMeanCrossX+spacingDegC;
  yFGmin=maxCenterMeanCrossY-spacingDegC;
  yFGmax=maxCenterMeanCrossY+spacingDegC;
  /* FINE GRID */
  (void)time(&timestart); 
  maxCenterMeanCross=-99.0;
  for(xOff=xFGmin;xOff<=xFGmax;xOff=xOff+spacingDegF) {
    for(yOff=yFGmin;yOff<=yFGmax;yOff=yOff+spacingDegF) {
      /* xOff/yOff are offset coordinates from "starting point" */
        crossScoreSum=0.0;
	numPoints=0;
        for(iyy=1;iyy<areadata_v72NC->numy-1;iyy++) {
          for(ixx=1;ixx<areadata_v72NC->numx-1;ixx++) {
            searchRad=A_POW(areadata_v72NC->lon[iyy][ixx],2)+A_POW(areadata_v72NC->lat[iyy][ixx],2);
            if(searchRad<inFilterDisc) {
              proxyX=lonMult*areadata_v72NC->lon[iyy][ixx]-xOff;
              proxyY=areadata_v72NC->lat[iyy][ixx]-yOff;
              denom=A_SQRT((alphapowp1)*(A_POW(proxyX,2)+A_POW(proxyY,2)));
              spiralX=(alpha*proxyX+(asign*proxyY))/denom;
              spiralY=(alpha*proxyY-(asign*proxyX))/denom;
              rawCrossScore=(spiralX*gradN[iyy][ixx])-(spiralY*gradE[iyy][ixx]);
              crossScoreClean=A_MAX(0.0,-rawCrossScore)+(outSideFactor*A_MAX(0.0,rawCrossScore));
	      crossScoreSum=crossScoreSum+crossScoreClean;
	      numPoints++; 
            }
          }
        }
        allCenterMeanCross=crossScoreSum/(float)numPoints;   /* calculate mean of all values in crossScore array */
	/* store location of maximum score position */
	if(allCenterMeanCross>maxCenterMeanCross) {
          maxCenterMeanCross=allCenterMeanCross;
          maxCenterMeanCrossY=yOff;
          maxCenterMeanCrossX=xOff;
        }
        spGrid[yx][0]=allCenterMeanCross;
        spGrid[yx][1]=(float)yOff;
        spGrid[yx][2]=(float)xOff;
        spGrid[0][0]=(float)yx;
        yx++;
    }
  }
/*
  printf("fine grid : y=%f x=%f max=%f\n",maxCenterMeanCrossY,maxCenterMeanCrossX,maxCenterMeanCross);
  (void)time(&timeend);
  printf("fine grid time=%f\n",difftime(timeend,timestart));
*/

  spGrid[0][1]=0.0;
  spGrid[0][2]=0.0;

  /* free memory */
  free(areadata_v72NC);
  for(iyy=0;iyy<maxd;iyy++) {
    free(gradN[iyy]);
    free(gradE[iyy]);
  }
  free(gradN);
  free(gradE);

  /* determine lat/lon point from x/y coordinates */
  *spiralLatCenter=maxCenterMeanCrossY+nhcLat;
  *spiralLonCenter=((lonMult*maxCenterMeanCrossX)/(A_COS(pi*nhcLat/180.0)))+nhcLon;
  *spiralScore=maxCenterMeanCross;
  for(yx=1;yx<=(int)spGrid[0][0];yx++) {
    spGrid[yx][1]=spGrid[yx][1]+nhcLat;
    spGrid[yx][2]=((lonMult*spGrid[yx][2])/(A_COS(pi*nhcLat/180.0)))+nhcLon;
  }

  return 0;
}

int aodtv72_gradient(float tG[maxd][maxd],int neles,int nlines,float lonInc,float latInc,float **gradN,float **gradE)
/* Calculate gradient field of input temperature value array.
   Inputs  : tG     - Temperature array
             neles  - Number of elements in Temperature array
             nlines - Number of lines in Temperature array
             lonInc - Longitudinal increment (elements)
             latInc - Latitudinal increment (lines)
   Outputs : gradN  - Gradient field of temperatures in N-S direction (along elements or Latitudinally)
             gradE  - Gradient field of temperatures in E-W direction (along lines or Longitudunally)
*/
{
  int   ixx,iyy;

  /* initialize arrays */
  for(iyy=0;iyy<nlines;iyy++) {
    for(ixx=0;ixx<neles;ixx++) {
      gradN[iyy][ixx]=0.0;
      gradE[iyy][ixx]=0.0;
    }
  }
  for(iyy=1;iyy<nlines-1;iyy++) {
    for(ixx=1;ixx<neles-1;ixx++) {
      /* determine N-S gradient at point */
      gradN[iyy][ixx]=(tG[iyy-1][ixx]-tG[iyy+1][ixx])/(2.0*latInc);
      /* printf("iyy=%d ixx=%d  latinc=%f  tG1=%f  tG2=%f\n",iyy,ixx,latInc,tG[iyy-1][ixx],tG[iyy+1][ixx]); */
      /* determine W-E gradient at point */
      /* gradE[iyy][ixx]=(tG[iyy][ixx-1]-tG[iyy][ixx+1])/(2.0*lonInc);  */
      /* determine E-W gradient at point */
      gradE[iyy][ixx]=(tG[iyy][ixx+1]-tG[iyy][ixx-1])/(2.0*lonInc);   
    }
  }
}

int aodtv72_ringFit(float latFirstGuess,float lonFirstGuess,int **moatMaskField,
                 float *latRing1,float *lonRing1,int *maxRing1,float **ringScores)
/* Perform Tony Wimmers' Ring Analysis.  
   Inputs  : nhcLat     - First Guess latitude position (from interpolated forecast)
             nhcLon     - First Guess longitude position (from interpolated forecast)
   Outputs : latRing1   - Ring Analysis latitude position at analysis grid maxiumum score value
             lonRing1   - Ring Analysis longitude position at array/grid maxiumum score value
             maxRing1   - Ring Analysis maxiumum score value
             ringScores - Array containting Ring Analysis grid field
*/
{

  float  **gx,**gy;
  float  dotScoreMax,dotScoreFinal;
  float  degPerPix,dotScore,inDisk,finalLat,finalLon;
  float  dotScoreSum,dotProductI,dotProductJ,dotProducts,asign;
  float  latInc,lonInc,lonMult;
  float  xlatx,xlonx,NanAdj,maxvalue;
  float  ringSearchRadiusDeg=0.75;   /* original was 1.0 */
  float  minRadiusDeg=0.06;
  float  maxRadiusDeg=0.40;
  int    maxRad,locI,locJ;
  int    iok,iFirstGuess,jFirstGuess;
  int    ixx,izz,iyy,a,aptr,b,bptr,circleFilterI,circleFilterJ,radi,ipts,numNans=0;
  int    searchRadiusPix,minRadiusPix,maxRadiusPix,numCirclePts;
  int    **filtRows,**filtCols;
  logical foundMoat;

  *latRing1=0.0;
  *lonRing1=0.0;

  /* derive values */
  degPerPix=A_ABS(areadataRM->lat[0][0]-areadataRM->lat[1][0]);
  searchRadiusPix=(int)A_ROUND(ringSearchRadiusDeg/degPerPix);
  minRadiusPix=A_MAX(2,A_ROUND(minRadiusDeg/degPerPix));
  maxRadiusPix=A_ROUND(maxRadiusDeg/degPerPix);

  lonInc=1.0; 
  /* latInc would be 1.0, but Tony does not flip this gradient, unlike with spiral routine */
  latInc=-1.0;

  /* allocate memory */
  a=sizeof(int);
  aptr=sizeof(int*);
  filtRows=(int **)calloc((size_t)50,aptr);
  filtCols=(int **)calloc((size_t)50,aptr);
  for(iyy=0;iyy<50;iyy++) {
    filtRows[iyy]=(int *)calloc((size_t)maxd,a);
    filtCols[iyy]=(int *)calloc((size_t)maxd,a);
  }
  b=sizeof(float);
  bptr=sizeof(float*);
  gx=(float **)calloc((size_t)maxd,bptr);
  gy=(float **)calloc((size_t)maxd,bptr);
  for(iyy=0;iyy<maxd;iyy++) {
    gx[iyy]=(float *)calloc((size_t)maxd,b);
    gy[iyy]=(float *)calloc((size_t)maxd,b);
  }

  /* calculate gradient field */
  iok=aodtv72_gradient(areadataRM->temp,areadataRM->numx,areadataRM->numy,lonInc,latInc,gy,gx);

  /* printf("RingFit : latFirstGuess=%f lonFirstGuess=%f\n",latFirstGuess,lonFirstGuess); */
  /* make matricies of row and column numbers */
  iok=aodtv72_lalo2indsFloat(latFirstGuess,lonFirstGuess,areadataRM->lat,areadataRM->lon,areadataRM->numx,areadataRM->numy,&iFirstGuess,&jFirstGuess);

  /* initialize circle/ring filter arrays */
  for(radi=0;radi<50;radi++) {  /* radius in pixels */
    for(ipts=0;ipts<500;ipts++) {   /* number of points on circle at radius */
      filtRows[radi][ipts]=0;
      filtCols[radi][ipts]=0;
    }
  }

  /* determine digital pixel coordinates for ring analysis for different radii sizes */
  for(radi=minRadiusPix;radi<=maxRadiusPix;radi++) {   /* this should be less than 100 total radius pixels, I hope */
    iok=aodtv72_circleFilt(radi,filtRows,filtCols);
  }

  /* search image box */
  dotScoreMax=-99999.0;
  izz=1;
  /* develop the accumulator */
  for(radi=minRadiusPix;radi<maxRadiusPix;radi++) { 
    numCirclePts=filtRows[radi][0];
    /* determine each main point in analysis disc */
    for(ixx=0;ixx<areadataRM->numx;ixx++) {
      for(iyy=0;iyy<areadataRM->numy;iyy++) {
        inDisk=((ixx-iFirstGuess)*(ixx-iFirstGuess))+((iyy-jFirstGuess)*(iyy-jFirstGuess));
        if(inDisk<=(searchRadiusPix*searchRadiusPix)) {
          /* if main point (iyy,ixx) is in disc, calculate dotproduct for each subpoint on ring around main point */
          dotScoreSum=0.0;
          numNans=0;
          foundMoat=FALSE;
          /* printf("ixx=%d iyy=%d  numCirclePts=%d\n",ixx,iyy,numCirclePts); */
          for(ipts=1;ipts<=numCirclePts;ipts++) {
            circleFilterI=ixx+filtCols[radi][ipts];
            circleFilterJ=iyy+filtRows[radi][ipts];
   /* insert Moat Mask here */
            if(moatMaskField[circleFilterJ][circleFilterI]==1) foundMoat=TRUE;
            /* printf("%d %d mask=%d  foundMoat=%d\n",circleFilterJ,circleFilterI,moatMaskField[circleFilterJ][circleFilterI],foundMoat); */
            if((foundMoat)||(circleFilterI<0)||(circleFilterJ<0)||(circleFilterI>=areadataRM->numx)||(circleFilterJ>=areadataRM->numy)) {
              dotProductI=-999.9;
              dotProductJ=-999.9;
            } else {
              dotProductI=((float)filtRows[radi][ipts]/(float)radi)*gy[circleFilterJ][circleFilterI];
              /* printf(" | %d %d j=%d i=%d     j=%d i=%d  gy=%f gx=%f | \n",ipts,radi,filtRows[radi][ipts],filtCols[radi][ipts],
                 circleFilterJ,circleFilterI,gy[circleFilterJ][circleFilterI],gx[circleFilterJ][circleFilterI]); */
              dotProductJ=((float)filtCols[radi][ipts]/(float)radi)*gx[circleFilterJ][circleFilterI];
            }
            if((dotProductI<-999.0)||(dotProductJ<-999.0)) {
              numNans++;
            } else {
              dotProducts=dotProductI+dotProductJ;
              if(dotProducts==0.0) {
                asign=0.0;
              } else {
                asign=A_ABS(dotProducts)/dotProducts;  /* return -1/+1 for -/+ value */
                }
              dotScore=asign*(A_LOG(1.0+A_ABS(dotProducts)));
              dotScoreSum=dotScoreSum+dotScore;
            }
          } /* if indisk */
          /* check for missing data and adjust dotScoreFinal accordingly */
          if(foundMoat||((float)numNans)>(0.575*(float)numCirclePts)) {
            dotScoreFinal=0.0;
          } else {
            NanAdj=(float)numCirclePts/(float)(numCirclePts-numNans);
            dotScoreFinal=-NanAdj*dotScoreSum/A_SQRT((float)numCirclePts);
          }
          if(dotScoreFinal>dotScoreMax) {
            dotScoreMax=dotScoreFinal;
            locI=ixx;
            locJ=iyy;
            maxRad=radi;
          }
          ringScores[izz][0]=dotScoreFinal;
          xlatx=areadataRM->lat[iyy][ixx];
          xlonx=areadataRM->lon[iyy][ixx];
          ringScores[izz][1]=xlatx;
          ringScores[izz][2]=xlonx;
          ringScores[0][0]=(float)izz;
          izz++;
          /* printf("ixx=%d iyy=%d lat=%f lon=%f radi=%d  dotScoreFinal=%f  dotScoreMax=%f \n",ixx,iyy,xlatx,xlonx,radi,dotScoreFinal,dotScoreMax); */
        } /* ixx */
      } /* iyy */
    } /* radi */
  }

  /* free memory */
  for(iyy=0;iyy<maxd;iyy++) {
    free(gx[iyy]);
    free(gy[iyy]);
  }
  free(gx);
  free(gy);
  for(iyy=0;iyy<50;iyy++) {
    free(filtRows[iyy]);
    free(filtCols[iyy]);
  }
  free(filtRows);
  free(filtCols);
  ringScores[0][1]=0.0;
  ringScores[0][2]=0.0;

  /* make matricies of row and column numbers */
  
  iok=aodtv72_inds2laloFloat(locI,locJ,areadataRM->lat,areadataRM->lon,areadataRM->numx,areadataRM->numy,&finalLat,&finalLon);
  *latRing1=finalLat;
  *lonRing1=finalLon;
  *maxRing1=maxRad;

  return 0;
}

int aodtv72_lalo2indsFloat(float lat,float lon,float latGrid[maxd][maxd],float lonGrid[maxd][maxd],int neles,int nlines,int *i,int *j)
/* Determine array index given latitude/longitude position 
   Inputs  : lat     - latitude position
             lon     - longitude position
             latGrid - latitude position grid at i/j points
             lonGrid - longitude position grid at i/j points
             neles   - number of elements in lat/lonGrid
             nlines  - number of lines in lat/lonGrid
   Outputs : i       - i (x-axis) position
             j       - j (y-axis) position
*/
{
  float  latLo,latHi,lonLo,lonHi;

  latLo=latGrid[nlines-1][0];
  latHi=latGrid[0][0];
  lonLo=lonGrid[0][neles-1];
  lonHi=lonGrid[0][0];

  *j=(int)(((float)(nlines-1.0)/(latHi-latLo))*(latHi-lat));
  *i=(int)(((float)(neles-1.0)/(lonHi-lonLo))*(lonHi-lon));

  return 0;
}

int aodtv72_inds2laloFloat(int i,int j,float latGrid[maxd][maxd],float lonGrid[maxd][maxd],int neles,int nlines,float *lat,float *lon)
/* Determine latitude/longitude position from array index 
   Inputs  : i       - i (x-axis) position
             j       - j (y-axis) position
             latGrid - latitude position grid at i/j points
             lonGrid - longitude position grid at i/j points
             neles   - number of elements in lat/lonGrid
             nlines  - number of lines in lat/lonGrid
   Outputs : lat     - latitude position
             lon     - longitude position
*/
{
  float  latLo,latHi,lonLo,lonHi;
  
  latLo=latGrid[nlines-1][0];
  latHi=latGrid[0][0];
  lonLo=lonGrid[0][neles-1];
  lonHi=lonGrid[0][0];

  *lon=lonHi-(((float)(i)/((float)neles-1.0))*(lonHi-lonLo));
  *lat=latHi-(((float)(j)/((float)nlines-1.0))*(latHi-latLo));

  return 0;
}

int aodtv72_circleFilt(int radius,int **fR,int **fC)
/* Determine index positions from i/j centerpoint on array for given radius
   Inputs  : radius - radius of ring to be determined (in number of pixels)
   Outputs : fR     - ring position (in row/y-axis direction)
             fC     - ring position (in column/x-axis direction)
   Note    : Array position [#][0] will be number of points on given radius (#=radius size)
*/
{
  int   ixx,iyy,icnt,radp1;
  float diff,inThresh;

  icnt=1;
  inThresh=0.5*(float)(((radius+1)*(radius+1))-(radius*radius));
  radp1=radius+1;
  for(ixx=-radp1;ixx<=radp1;ixx++) {
    for(iyy=-radp1;iyy<=radp1;iyy++) {
      diff=(float)((iyy*iyy)+(ixx*ixx)-(radius*radius));
      if(A_ABS(diff)<=inThresh) {
        fR[radius][icnt]=iyy;
        fC[radius][icnt]=ixx;
	icnt++;
      }
    }
  }
  /* number of points on given radius size */
  fR[radius][0]=icnt-1;
  fC[radius][0]=icnt-1;

  return 0;
}

/* the following routines were originally developed by Dave Santek of UW/SSEC and
   were added to the AODT under permission.
   If executed, an array of latitude and longitude position arrays will be remapped
   to a rectilinear projection for Tony Wimmers routines
*/
int aodtv72_remapdata( )
{

	/* Calls routines to setup transformation, transform, data move */
	
	int rc=0;				/* Return code */
	int nc=0;				/* Number of corners */
        int b;
	float *zlin;		/* Line coords */
	float *zele;		/* Elem coords */

        /* int lspline=1;          / spline function for line */
        int lspline=3;          /* spline function for line */
        int espline=lspline;    /* spline function for element */

        int ixx,iyy;

        /* printf("SPLINE=%d\n",lspline); */
        tiff_vars.in_elems=areadata_v72->numx;
        tiff_vars.in_lines=areadata_v72->numy;

        rc = aodtv72_uinit( );

        (void) aodtv72_determinedest( );

        rc = aodtv72_init(lspline, espline, &nc);

        b=sizeof(float);
        zlin=(float *)calloc((size_t)nc,b);
        zele=(float *)calloc((size_t)nc,b);
/*
	zlin = malloc(nc * sizeof(zlin));
	zele = malloc(nc * sizeof(zele));
*/

	(void) aodtv72_corner(nc, lspline, espline, zlin, zele);

        if((espline>1)||(lspline>1)) {
	  rc = aodtv72_domap(nc, zlin, zele, lspline, espline);
        }

	free(zlin);
	free(zele);

	return rc;
}

int aodtv72_uinit( )
{

	/* Setup output file size and global variables needed */
	/* This module is for no changes for checking distortion */

	dis_vars.xrectl = (double) tiff_vars.in_lines;
	dis_vars.xrecte = (double) tiff_vars.in_elems;

	return 0;
}

int aodtv72_init(int lspline, int espline, int *nc) 
{
	/* Compute number of corners for transformation & block sizes */

	int mod_sne;	/* Modify number of source bits (byte boundary) */

        remap_vars.nspl = (tiff_vars.out_elems + espline -1)/espline;
	remap_vars.nspe = (tiff_vars.out_lines + lspline -1)/lspline;

        remap_vars.ncl = remap_vars.nspl + 1;
	remap_vars.nce = remap_vars.nspe + 1;

        if((tiff_vars.out_elems + espline - 1) % espline == 0) 
		remap_vars.ncl = remap_vars.nspl;
        if((tiff_vars.out_lines + lspline - 1) % lspline == 0) 
		remap_vars.nce = remap_vars.nspe;

        *nc = remap_vars.ncl * remap_vars.nce;

	remap_vars.in_bfw = A_MAX( MINBFW, MINBLKSIZ * tiff_vars.in_elems );
	remap_vars.out_bfw = A_MAX( MINBFW, A_MAX( lspline, MINBLKSIZ) * tiff_vars.out_elems );

	remap_vars.slb = remap_vars.in_bfw/tiff_vars.in_elems;
	remap_vars.dlb = ((remap_vars.out_bfw/tiff_vars.out_elems)/lspline) * lspline;

    return 0;
}

void  aodtv72_determinedest( )
{
        int xn,yn,il,ie;
        float latNW,lonNW,latNE,lonNE,latSW,lonSW,latSE,lonSE,lat,lon;
        float dmaxlat,dmaxlon,dminlat,dminlon,dinclat,dinclon,inclatlon;
        int iyy,ixx;
        float xval;

        xn=areadata_v72->numx-1;
        yn=areadata_v72->numy-1;

        /* printf("xn=%d yn=%d\n",xn,yn); */
        latNW=areadata_v72->lat[0][0];
        lonNW=areadata_v72->lon[0][0];
        latNE=areadata_v72->lat[0][xn];
        lonNE=areadata_v72->lon[0][xn];
        latSW=areadata_v72->lat[yn][0];
        lonSW=areadata_v72->lon[yn][0];
        latSE=areadata_v72->lat[yn][xn];
        lonSE=areadata_v72->lon[yn][xn];
  
        /* crosses dateline check */
        if((lonNW<lonNE)||(lonSW<lonSE)) {
          lonNW=lonNW+360.0;
          if(lonSW<lonSE) lonSW=lonSW+360.0;
          printf("DATELINE CROSS\n");
          for(ixx=0;ixx<=xn;ixx++) {
            for(iyy=0;iyy<=yn;iyy++) {
              xval=areadata_v72->lon[iyy][ixx];
              if(xval<0.0) areadata_v72->lon[iyy][ixx]=xval+360.0;
            }
          }
          latNW=areadata_v72->lat[0][0];
          lonNW=areadata_v72->lon[0][0];
          latNE=areadata_v72->lat[0][xn];
          lonNE=areadata_v72->lon[0][xn];
          latSW=areadata_v72->lat[yn][0];
          lonSW=areadata_v72->lon[yn][0];
          latSE=areadata_v72->lat[yn][xn];
          lonSE=areadata_v72->lon[yn][xn];
        }

        dmaxlat=A_MIN(latNW,latNE);
        dminlat=A_MAX(latSW,latSE);
        dmaxlon=A_MIN(lonNW,lonSW);
        dminlon=A_MAX(lonNE,lonSE);

        dinclat=(dmaxlat-dminlat)/(float)areadata_v72->numy;
        dinclon=(dmaxlon-dminlon)/(float)areadata_v72->numx;

        inclatlon=A_MAX(dinclat,dinclon);

/*
        printf("Source Array Bounds\n");
        printf("     NW Corner : %7.2f/%7.2f\n",latNW,lonNW);
        printf("     NE Corner : %7.2f/%7.2f\n",latNE,lonNE);
        printf("     SW Corner : %7.2f/%7.2f\n",latSW,lonSW);
        printf("     SE Corner : %7.2f/%7.2f\n",latSE,lonSE);
        printf("Destination Array Bounds\n");
        printf("    Max Lat/Lon: %7.2f/%7.2f\n",dmaxlat,dmaxlon);
        printf("    Min Lat/Lon: %7.2f/%7.2f\n",dminlat,dminlon);
        printf("    Inc Lat/Lon:   %5.3f/  %5.3f\n",inclatlon,inclatlon);
*/

        tiff_vars.out_lines=(int)A_ABS((dmaxlat-dminlat)/inclatlon);
        tiff_vars.out_elems=(int)A_ABS((dmaxlon-dminlon)/inclatlon); /* dmaxlon-dminlon may be negative. Comment by M. Li */
	for (il = 0; il < tiff_vars.out_lines; il++) {
          lat=dmaxlat-(il*inclatlon);
	  for (ie = 0; ie < tiff_vars.out_elems; ie++) {
            lon=dmaxlon-(ie*inclatlon);
            areadataRM->lat[il][ie]=lat;
            areadataRM->lon[il][ie]=lon;
          }
        }
        areadataRM->numx=tiff_vars.out_elems;
        areadataRM->numy=tiff_vars.out_lines;
}
void  aodtv72_corner(int nc, int lspline, int espline, float lines[], float elems[])
{
	/* Compute transformations at corners */

	int countL;
	int countE;
	int i;
	int index;
	int num_lines;
	int num_elems;
	int rc;

	int Dline;
	int Delem;
	int Sline;
	int Selem;
	
        /* printf("nc=%d\n",nc); */;
	/* initialize array of corners */
	for (i = 0; i< nc; i++) 
	{
		lines[i] = (float) -99.9;
		elems[i] = (float) -99.9;
	}

	/* loop through destination file and record source coords */

	index = -1;
	num_lines = tiff_vars.out_lines + lspline - 1;
	num_elems = tiff_vars.out_elems + espline - 1;
        /* printf("lines=%d elems=%d\n",tiff_vars.out_lines,tiff_vars.out_elems); */

	for (countL = 0; countL < num_lines; countL = countL + lspline) {
	  Dline = countL;

	  for (countE = 0; countE < num_elems; countE = countE + espline )
	  {
	    Delem = countE;
            /* printf("dline=%d delem=%d  numlines=%d numelems=%d\n",Dline,Delem,num_lines,num_elems); */
	    rc = aodtv72_umap(Dline, Delem, &Sline, &Selem);
            if((espline==1)&&(lspline==1)) {
              areadataRM->temp[Dline][Delem]=areadata_v72->temp[Sline][Selem];
            } else {
              ++index;
 	      if( rc == 0) {
	        lines[index] = Sline;
	        elems[index] = Selem;
	      }
	    }
	  }
	}
        /* printf("index=%d\n",index); */
}

int aodtv72_umap(int yl, int ye, int *zl, int *ze)
{
        int iok,srcline,srcele;
        float destlat,destlon;

	/* Convert destination yl, ye to source coords zl, ze */

        yl=A_MIN(yl,tiff_vars.out_lines-1);
        ye=A_MIN(ye,tiff_vars.out_elems-1);
        destlat=areadataRM->lat[yl][ye];
        destlon=areadataRM->lon[yl][ye];

        iok=aodtv72_findpoint(destlat,destlon,&srcline,&srcele);
	*zl = srcline;
	*ze = srcele;
/*
	*zl = yl;
	*ze = ye;
*/

	return 0;
}

int aodtv72_domap(int nc, float zlin[], float zele[], int lspline, int espline)
{
	/* Move data according to tranformation */

    int acclin,block,dloc,dne0,doff,doff_word,doff_bit,doff0,doff1,doff2,doffe,doffl;
    int ie,ie1,ie2,ifxedg,il,il1,il2,ind1,ind2,ipt,isps;
    int k,kmaxe,kmaxl,kmine,kminl,maxsl,mod_sne,mxpt,npt,opoint;
    int point,psps,sblk,se,sl,sloc,soff,soff_word,soff_bit,sppix,read;
    float ze11,ze12,ze21,ze22,zea,zeac,zeac0,zeb,zebb,zec,zecc;
    float zl11,zl12,zl21,zl22,zla,zlac,zlac0,zlb,zlbb,zlc,zlcc;
    float zminl,zmaxl,zmine,zmaxe,zsume,zsuml;

    char *source;			/* Source buffer */
    char *dest;				/* Destination buffer */
    char *temp;				/* Temporary buffer */

/* 
    long *source4;
    long *dest4;
*/
    float *source4;
    float *dest4;

    int buf_length;

    int nrad= 8;
    static int jl1[8] = {-2,-2, 0, 2, 2, 2, 0,-2};
    static int jl2[8] = {-1,-1, 0, 1, 1, 1, 0,-1};
    static int je1[8] = { 0, 2, 2, 2, 0,-2,-2,-2};
    static int je2[8] = { 0, 1, 1, 1, 0,-1,-1,-1};

    int indx=0;

	/* 
		To smooth edged, extrapolate out one corner
		Use mean value from all radially extrapolated calculations
	*/

	source4 = malloc(remap_vars.in_bfw * 4);
	dest4 = malloc(remap_vars.out_bfw * 4);

    	source = (char *) source4;
	dest = (char *) dest4;

	buf_length = remap_vars.in_bfw * 4;
	

	temp = malloc(nc);

	if(source == NULL || dest == NULL || temp == NULL)
	{
	  /* printf("Error allocating memory in domap\n"); */
	  /* printf("Sizes: %d %d %d\n", remap_vars.in_bfw,remap_vars.out_bfw, nc); */
	  return -1;
	}

	sppix = lspline * espline;

        (void) memset(temp, 0, nc);
 
	for (il = 0; il < tiff_vars.in_lines; il++) {
	  for (ie = 0; ie < tiff_vars.in_elems; ie++) {
            indx=(il*tiff_vars.in_elems)+ie;
            source4[indx]=areadata_v72->temp[il][ie];
          }
        }


	for (il = 1; il < remap_vars.nce+1; il++) {
	for (ie = 1; ie < remap_vars.ncl+1; ie++) {
	  ipt = IND(il, ie);
	  if(zlin[ipt-1] == (float) -99.9) {
	    zsuml = (float) 0.0;
	    zsume = (float) 0.0;
	    npt = 0;
	    for (k = 0; k < nrad; k++) {
	      il1=il+jl1[k];
	      il2=il+jl2[k];
	      if( il1 <   1 || il2 <   1) goto skip;
	      if( il1 > remap_vars.nce || il2 > remap_vars.nce) goto skip;

	      ie1 = ie + je1[k];
	      ie2 = ie + je2[k];
	      if( ie1 <   1 || ie2 <   1) goto skip;
	      if( ie1 > remap_vars.ncl || ie2 > remap_vars.ncl) goto skip;

	      ind1 = IND(il1, ie1);
	      ind2 = IND(il2, ie2);

	      if(zlin[ind1-1] == (float) -99.9 || zlin[ind2-1] ==(float) -99.9) goto skip;
	      if(temp[ind1-1] != 0) goto skip;
	      if(temp[ind2-1] != 0) goto skip;
	      npt = npt + 1;
	      zsuml = zsuml + (float) 2. * zlin[ind2-1] - zlin[ind1-1];
	      zsume = zsume + (float) 2. * zele[ind2-1] - zele[ind1-1];
	      skip:;
	    }

	    if( npt > 0) {
	      zlin[ipt-1] = zsuml/npt;
	      zele[ipt-1] = zsume/npt;
	      temp[ipt-1] = 1;
	    }
	  }
	}
	}

	free(temp);

	/* Loop through by destination blocks */
	
	block = 0;

	for (dloc = 1; dloc < tiff_vars.out_lines +1; dloc = dloc + remap_vars.dlb) {
	  /* Accumulated lines/block */
	  acclin = block * remap_vars.dlb;
  
	  /* Pointer to first corner of splines for this dest block */
	  point = block * remap_vars.ncl * remap_vars.dlb /lspline;
	  opoint = point;

	  /* Pointer to last corner for this dest block */
	  mxpt = ((block + 1) * remap_vars.ncl * remap_vars.dlb / lspline) - 1;
	  mxpt = A_MIN(mxpt, nc - remap_vars.ncl);
  
	  (void) memset(dest, 0, buf_length);

	  /* For each destination block loop through entire source */

	  for (sloc = 1; sloc < tiff_vars.in_lines +1; sloc = sloc + remap_vars.slb) {
	    maxsl = A_MIN(tiff_vars.in_lines, sloc + remap_vars.slb-1);
	    read = FALSE;

	    /* Loop through splines and move any data */

	    point = opoint;
	    sblk = 0;
	    while (point < mxpt) {

	      for (isps = 1; isps < remap_vars.nspl+1; isps++) {
	        doff0 = sblk/remap_vars.nspl*lspline*tiff_vars.out_elems;
	        doff1 = (sblk % remap_vars.nspl) * espline;
	        doff2 = doff1 + doff0+1;
	        psps = point + isps - 1;

	        /* Get 4 corners in line space and check for out of bounds */

	        zl11=zlin[psps];
	        zl12=zlin[psps + 1];
	        zl21=zlin[psps + remap_vars.ncl];
	        zl22=zlin[psps + 1 + remap_vars.ncl];
	        zminl = A_MIN(zl11,zl12);
	        zminl = A_MIN(zminl, zl21);
	        zminl = A_MIN(zminl, zl22);

	        /* Test for the presence of a limb in the spline box */

	        if( zminl == (float) -99.9) goto label30;

	        kminl = (int) (zminl + (float) 0.5);
	        if(kminl > maxsl) goto label30;
	        zmaxl = A_MAX(zl11, zl12);
	        zmaxl = A_MAX(zmaxl, zl21);
	        zmaxl = A_MAX(zmaxl, zl22);
	        kmaxl = (int) (zmaxl + (float) 0.5);
	        if(kmaxl < sloc) goto label30;

	        /* Get 4 corners in elem space & check for out of bound */
        
	        ze11=zele[psps];
	        ze12=zele[psps + 1];
	        ze21=zele[psps + remap_vars.ncl];
	        ze22=zele[psps + 1 + remap_vars.ncl];
        
	        zmaxe = A_MAX(ze11, ze12);
	        zmaxe = A_MAX(zmaxe, ze21);
	        zmaxe = A_MAX(zmaxe, ze22);
	        kmaxe = (int) (zmaxe + (float) 0.5);
	        if(kmaxe < 1) goto label30;

	        zmine = A_MIN(ze11, ze12);
	        zmine = A_MIN(zmine, ze21);
	        zmine = A_MIN(zmine, ze22);
	        kmine = (int) (zmine + (float) 0.5);

	        if(kmine > tiff_vars.in_elems) goto label30;
	        ifxedg = 0;

	        /* If the max & min element fall off the image...pitch it */

	        if( kmaxe > tiff_vars.in_elems && kmine < 1) goto label30;

	        /* Fix if left & right edge should be continuous */

	        if(kmaxe - kmine > (int) (.75 * tiff_vars.in_elems) ) {
	          if(ze11 < tiff_vars.in_elems/2) ze11 = ze11 + tiff_vars.in_elems;
	          if(ze12 < tiff_vars.in_elems/2) ze12 = ze12 + tiff_vars.in_elems;
	          if(ze21 < tiff_vars.in_elems/2) ze21 = ze21 + tiff_vars.in_elems;
	          if(ze22 < tiff_vars.in_elems/2) ze22 = ze22 + tiff_vars.in_elems;
	          ifxedg = 1;
	        }

	        zla=(zl12-zl11)/espline;
	        zlb=(zl21-zl11)/lspline;
	        zlc=(zl22+zl11-zl12-zl21)/sppix;
	        zea=(ze12-ze11)/espline;
	        zeb=(ze21-ze11)/lspline;
	        zec=(ze22+ze11-ze21-ze12)/sppix;

	        dne0 = 0;
                zlbb=zl11+ (float) 0.5;
	        zlcc= (float) 0.0;
	        zebb=ze11+ (float) 0.5;
	        zecc= (float) 0.0;

	        if(read == FALSE) {
	          read = TRUE;
	        }

	        if( (isps == remap_vars.nspl ) || (kmine < 1 || kmaxe > tiff_vars.in_elems) ||
	            (kminl < sloc || kmaxl > maxsl) || (point+2*remap_vars.ncl-1 > mxpt)) {
	          for (il = 1; il < lspline+1; il++) {
	            zlac=zlcc+zla;
	            zeac=zecc+zea;
	            zlac0= (float) 0.0;
	            zeac0= (float) 0.0;
	            for (ie =1; ie < espline+1; ie++) {
	              sl = (int) (zlbb+zlac0);
	              if( sl < sloc) goto label38;
	              if( sl > maxsl) goto label38;
	              se= (int) (zebb+zeac0);
	              if( se < 1) goto label38;
	              if( se > tiff_vars.in_elems && ifxedg == 0) goto label38;
	              if (se > tiff_vars.in_elems) se = se - tiff_vars.in_elems;
	              soff=(sl-sloc)*tiff_vars.in_elems+se;
	              doffe=doff1+ie;
	              if(doffe >  tiff_vars.out_elems) goto label38;
	              doffl=doff0+dne0;
	              if(doffl/tiff_vars.out_elems+acclin-1 > tiff_vars.out_lines) goto label38;
	              doff=doffl+doffe;
	              dest4[doff-1]=source4[soff-1];
	              label38:;
	              zlac0=zlac0+zlac;
	              zeac0=zeac0+zeac;
	            }
	            zlbb=zlbb+zlb;
	            zlcc=zlcc+zlc;
	            zebb=zebb+zeb;
	            zecc=zecc+zec;
	            dne0=dne0+tiff_vars.out_elems;
	          }
	        } else {
	          if( ifxedg == 0) {
	            for (il = 1; il < lspline+1; il++) {
	              zlac=zlcc+zla;
	              zeac=zecc+zea;
	              zlac0= (float) 0.0;
	              zeac0= (float) 0.0;
	              doff = doff2 + dne0;
	              for (ie =1; ie < espline+1; ie++) {
		        sl=(int) (zlbb+zlac0);
		        se=(int) (zebb+zeac0);
		        soff=(sl-sloc)*tiff_vars.in_elems+se;
		        dest4[doff-1]=source4[soff-1];
		        doff = doff + 1;
		        zlac0=zlac0+zlac;
		        zeac0=zeac0+zeac;
	              }
	              zlbb=zlbb+zlb;
	              zlcc=zlcc+zlc;
	              zebb=zebb+zeb;
	              zecc=zecc+zec;
	              dne0=dne0+tiff_vars.out_elems;
	            }
	          } else if( ifxedg == 1) {
	            for (il = 1; il < lspline+1; il++) {
	              zlac=zlcc+zla;
	              zeac=zecc+zea;
	              zlac0= (float) 0.0;
	              zeac0= (float) 0.0;
	              doff = doff2 + dne0;
	              for (ie =1; ie < espline+1; ie++) {
		        sl=(int) (zlbb+zlac0);
		        se=(int) (zebb+zeac0);
	                if (se > tiff_vars.in_elems) se = se - tiff_vars.in_elems;
	                soff=(sl-sloc)*tiff_vars.in_elems+se;
	                dest4[doff-1]=source4[soff-1];
	                doff = doff + 1;
	                zlac0=zlac0+zlac;
	                zeac0=zeac0+zeac;
	              }
	              zlbb=zlbb+zlb;
	              zlcc=zlcc+zlc;
	              zebb=zebb+zeb;
	              zecc=zecc+zec;
	              dne0=dne0+tiff_vars.out_elems;
	            }
	          }
	        }


	        label30:;
	        sblk = sblk + 1;
	      }	
	      point = point + remap_vars.ncl;
	    }
	  }

	  block = block + 1;
	}

	for (il = 0; il < tiff_vars.out_lines; il++) {
	  for (ie = 0; ie < tiff_vars.out_elems; ie++) {
            indx=(il*tiff_vars.out_elems)+ie;
            if(dest4[indx]==0.0) {
              dest4[indx]=dest4[indx-1];
            } 
            areadataRM->temp[il][ie]=dest4[indx];
          }
        }

	free(dest);
	free(source);
/*
	free(dest4);
	free(source4);
*/

	return 0;
}

int aodtv72_findpoint (float slat,float slon,int *outy,int *outx)
{

  int ixx,x,y,numx,numy,lastsearch,value;
  float latxy[4],lonxy[4],xdist[4];
  float lastdist,xdistx,xangle;
  logical found=FALSE,oob=FALSE;
  logical inlat=FALSE,inlon=FALSE;
 
  numx=tiff_vars.in_elems;
  numy=tiff_vars.in_lines;
  /* printf("numx=%d numy=%d\n",numx,numy); */
  x=0;
  y=0;
  lastsearch=0;
  lastdist=9999.9;
  xdistx=0.0;
  for(ixx=0;ixx<4;ixx++) {
    xdist[ixx]=0.0;
    latxy[ixx]=0.0;
    lonxy[ixx]=0.0;
  }
  while(!found&&!oob) {
    latxy[0]=areadata_v72->lat[y][x];
    lonxy[0]=areadata_v72->lon[y][x];
    latxy[3]=areadata_v72->lat[y+1][x+1];
    lonxy[3]=areadata_v72->lon[y+1][x+1];
    /* printf("x=%d  y=%d  : latxy0=%f lonxy0=%f latxy3=%f lonxy3=%f\n",x,y,latxy[0],lonxy[0],latxy[3],lonxy[3]); */
    if((slon>lonxy[0])||(slon<lonxy[3])) {
      inlon=FALSE;
      if(slon<lonxy[3]) {
        x++;
      } else {
        if(slon>lonxy[0]) x--;
      }
    } else {
      inlon=TRUE;
    }
    if((slat>latxy[0])||(slat<latxy[3])) {
      inlat=FALSE;
      if(slat<latxy[3]) {
        y++;
      } else {
        if(slat>latxy[0]) y--;
      }
    } else {
      inlat=TRUE;
    }
    aodtv72_distance(slat,slon,latxy[0],lonxy[0],1,&xdistx,&xangle);
    /* printf("distance : slat=%f slon=%f latxy0=%f lonxy0=%f dist=%f angle=%f\n",slat,slon,latxy[0],lonxy[0],xdistx,xangle); */
    if(inlat&&inlon) found=TRUE;
    if(lastdist<=xdistx) found=TRUE; 
    if((x<0)||(x>numx-2)) oob=TRUE;
    if((y<0)||(y>numy-2)) oob=TRUE;
    lastdist=xdistx;
  }
  if(found) {
    latxy[1]=areadata_v72->lat[y][x+1];
    lonxy[1]=areadata_v72->lon[y][x+1];
    latxy[2]=areadata_v72->lat[y+1][x];
    lonxy[2]=areadata_v72->lon[y+1][x];
    aodtv72_distance(slat,slon,latxy[0],lonxy[0],1,&xdist[0],&xangle);
    aodtv72_distance(slat,slon,latxy[1],lonxy[1],1,&xdist[1],&xangle);
    value=(xdist[0]<xdist[1]) ? 0 : 1;
    aodtv72_distance(slat,slon,latxy[2],lonxy[2],1,&xdist[2],&xangle);
    value=(xdist[value]<xdist[2]) ? value : 2;
    aodtv72_distance(slat,slon,latxy[3],lonxy[3],1,&xdist[3],&xangle);
    value=(xdist[value]<xdist[3]) ? value : 3;
    *outx=((value==0)||(value==2)) ? x : x+1;
    *outy=((value==0)||(value==1)) ? y : y+1;
    return 0;
  } else {
    *outx=-1;
    *outy=-1;
    return -1;
  }
}

int  aodtv72_moatMaskCalc( int **objArray )
{
  int i,nobjs,bptr,iok;
  int nr,nc,r,c,ival;
  int **bwField;
  float pi=3.141592;
  float tempThreshold=237.0;
  float maxRadiusDeg=0.50;
  float xlat,xlon,maxLat,maxLon,minLat,minLon,avgLat,featureLength;

  nc=areadataRM->numx;
  nr=areadataRM->numy;

  /* printf("nc=%d nr=%d\n",nc,nr); */

  for( r = 0; r < nr; r++ ) {
    for( c = 0; c < nc; c++ ) {            
      objArray[r][c]=0;
    }
  }

  bptr=sizeof(int*);
  bwField=(int **)calloc((size_t)nr,bptr);
  for(i=0;i<nr;i++) {
    bwField[i]=(int *)calloc((size_t)nc,bptr);
  }

  iok=aodtv72_meshgrid(tempThreshold,bwField);
/*
  printf("BW:\n");
  for( r = 0; r < nr; r++ ) {
      printf("r=%d :",r);
      for( c = 0; c < nc; c++ ) {            
          printf("%d  ",bwField[r][c]);
      }
      printf("\n");
  }
*/

  nobjs=aodtv72_bwimage(bwField,8,nr,nc,objArray);
/*
  printf("nobjs=%d\n",nobjs);
  printf("objArray (init):\n");
  for( r = 0; r < nr; r++ ) {
      printf("r=%d :",r);
      for( c = 0; c < nc; c++ ) {            
          printf("%d  ",objArray[r][c]);
      }
      printf("\n");
  }
*/

  for(i=1;i<=nobjs;i++) {
    maxLat=-90.0;
    minLat=90.0;
    maxLon=-180.0;
    minLon=180.0;
    for( r = 0; r < nr; r++ ) {
      for( c = 0; c < nc; c++ ) {            
        if(objArray[r][c]==i) {
          xlat=areadataRM->lat[r][c];
          xlon=areadataRM->lon[r][c];
          if(xlat>maxLat) maxLat=xlat;
          if(xlon>maxLon) maxLon=xlon;
          if(xlat<minLat) minLat=xlat;
          if(xlon<minLon) minLon=xlon;
        }
      }
    }
    avgLat=.5*(minLat+maxLat);
    featureLength=A_SQRT(A_POW((maxLat-minLat),2)+A_POW((maxLon-minLon)/A_COS((pi*avgLat)/180.0),2));
    /* printf("i=%d  featureLength=%f  value=%f\n",i,featureLength,A_POW(maxRadiusDeg,2)); */
    ival=(featureLength>(maxRadiusDeg*2)) ? 1 : 0;
    for( r = 0; r < nr; r++ ) {
      for( c = 0; c < nc; c++ ) {            
        if(objArray[r][c]==i) {
          objArray[r][c]=ival;
          /* printf("reassigning i=%d at r=%d c=%d to %d\n",i,r,c,ival); */
        }
      }
    }
  }
/*
  printf("objArray (final):\n");
  for( r = 0; r < nr; r++ ) {
    printf("r=%d :",r);
    for( c = 0; c < nc; c++ ) {            
      printf("%d  ",objArray[r][c]);
    }
    printf("\n");
  }
*/

  for(i=0;i<nr;i++) {
    free(bwField[i]);
  }
  free(bwField);
  return 0;
}

int aodtv72_meshgrid( float tempThresh, int **bwField)
{

  int ixx,iyy;
  for(ixx=0;ixx<areadataRM->numx;ixx++) {
    for(iyy=0;iyy<areadataRM->numy;iyy++) {
      bwField[iyy][ixx]=(areadataRM->temp[iyy][ixx]>tempThresh) ? 1 : 0;
    }
  }

  return 0;
}

int aodtv72_bwimage(int **BW,int n,int nr,int nc,int **L)
/* ---------------------------------------------------------------------

    bwimage.cc - octave module to label componenets of a binary image

    copyright 2002 Jeffrey E. Boyd

    - uses 4, 6, or 8 connectedness
    - See BKP Horn, Robot Vision, MIT Press, 1986, p 66 - 71 

    labeling scheme

        +-+-+-+
        |D|C|E|
        +-+-+-+
        |B|A| |
        +-+-+-+
        | | | |
        +-+-+-+
                
    A is the center pixel of a neighborhood.  In the 3 versions of
    connectedness:
    
    4:  A connects to B and C
    6:  A connects to B, C, and D
    8:  A connects to B, C, D, and E
    
[l,num] - bwlabel( bw, n ) - label foreground components of boolean image
    bw  -   boolean image array
    n   -   neighborhood connectedness (4, 6,or 8)
    l   -   label image array
    num -   number of components labeled
    The algorithm is derived from  BKP Horn, Robot Vision, MIT Press,
    1986, p 65 - 89 " )
--------------------------------------------------------------------- */
{
    int nobj;               /* number of objects found in image */
    int ntable;             /* number of elements in the component table/tree */
    
    /* other variables */
    int bptr,nptr,i;
    int B, C, D, E;
    int r,c;
    int tlabel;
    int *lset;
    
    ntable = 0;
    bptr=sizeof(int*);
    lset=(int *)calloc((size_t)nr*nc,bptr);
    lset[0] = 0;
    
    for( r = 0; r < nr; r++ ) {
        for( c = 0; c < nc; c++ ) {            
            if ( BW[r][c] ) {               /* if A is an object */
                /* get the neighboring pixels B, C, D, and E */
                B = 0;
                C = 0;
                D = 0;
                E = 0;
                if ( c > 0 ) B = aodtv72_find( lset, L[r][c-1] );
                if ( r > 0 ) C = aodtv72_find( lset, L[r-1][c] );
                if ( r > 0 && c > 0 ) D = aodtv72_find( lset, L[r-1][c-1] );
                if ( r > 0 && c > (nc - 1) ) E = aodtv72_find( lset, L[r-1][c+1] );
                    
                if ( n == 4 ) {
                    /* apply 4 connectedness */
                    if ( B && C ) {        /* B and C are labeled */
                        if ( B == C ) {
                            L[r][c] = B;
                        } else {
                            lset[C] = B;
                            L[r][c] = B;
                        }
                    } else if ( B ) {           /* B is object but C is not */
                        L[r][c] = B;
                    } else if ( C ) {           /* C is object but B is not */
                        L[r][c] = C;
                    } else {                    /* B, C, D not object - new object */
                        /*   label and put into table */
                        ntable++;
                        lset[ ntable ] = ntable;
                        L[r][c] = lset[ ntable ];
                    }
                } else if ( n == 6 ) {
                    /* apply 6 connected ness */
                    if ( D ) {                  /* D object, copy label and move on */
                        L[r][c] = D;
                    } else if ( B && C ) {      /* B and C are labeled */
                        if ( B == C ) {
                            L[r][c] = B;
                        } else {
                            tlabel = A_MIN(B,C);
                            lset[B] = tlabel;
                            lset[C] = tlabel;
                            L[r][c] = tlabel;
                        }
                    } else if ( B ) {           /* B is object but C is not */
                        L[r][c] = B;
                    } else if ( C ) {           /* C is object but B is not */
                        L[r][c] = C;
                    } else {                    /* B, C, D not object - new object */
                        /*   label and put into table */
                        ntable++;
                        lset[ ntable ] = ntable;
                        L[r][c] = lset[ ntable ];
                    }
                } else if ( n == 8 ) {
                    /* apply 8 connectedness */
                    if ( B || C || D || E ) {
                        tlabel = B;
                        if ( B ) { tlabel = B; }
                        else if ( C ) { tlabel = C; }
                        else if ( D ) { tlabel = D; }
                        else { tlabel = E; }
                        L[r][c] = tlabel;
                        if ( B && B != tlabel ) lset[B] = tlabel;
                        if ( C && C != tlabel ) lset[C] = tlabel;
                        if ( D && D != tlabel ) lset[D] = tlabel;
                        if ( E && E != tlabel ) lset[E] = tlabel;
                    } else {
                        /*   label and put into table */
                        ntable++;
                        lset[ ntable ] = ntable;
                        L[r][c] = lset[ ntable ];
                    }
                }
            } else {
                L[r][c] = 0;      /* A is not an object so leave it */
            }
        }
    }
    
    /* consolidate component table */
    for( i = 0; i <= ntable; i++ ) {
        lset[i] = aodtv72_find( lset, i );
    }

    /* run image through the look-up table */
    for( r = 0; r < nr; r++ ) {
        for( c = 0; c < nc; c++ ) {
            L[r][c] = lset[ L[r][c] ];
        }
    }
    
    /* count up the objects in the image */
    for( i = 0; i <= ntable; i++ ) {
        lset[i] = 0;
    }

    for( r = 0; r < nr; r++ ) {
        for( c = 0; c < nc; c++ ) {
            lset[ L[r][c] ]++;
        }
    }

    /* number the objects from 1 through n objects */
    nobj = 0;
    lset[0] = 0;
    for( i = 1; i <= ntable; i++ ) {
        if ( lset[i] > 0 ) {
            lset[i] = ++nobj;
        }
    }

    /* run through the look-up table again */
    for( r = 0; r < nr; r++ ) {
        for( c = 0; c < nc; c++ ) {
            L[r][c] = lset[ L[r][c] ];
        }
    }

    free(lset);
    return nobj;
}

int aodtv72_find( int set[], int x )
{
    int r = x;
    while ( set[r] != r ) {
        r = set[r];
    }
    return r;
}
