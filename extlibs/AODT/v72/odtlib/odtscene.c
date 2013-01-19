/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"
#define maxsec 24
#define maxsecA 2000

int aodtv72_calcscene(void);
void aodtv72_classifyredo(void);
void aodtv72_classify(void);
void aodtv72_cdoshearcalc( float,float,float,int,float *,float * );
void aodtv72_xxsort( float *, float *, float *, int );
void aodtv72_fminx( float *, int, int *, float * );
void aodtv72_fmaxx( float *, int, int *, float * );

int aodtv72_calcscene(void)
/* Perform Fast Fourier Transform (FFT) analysis and determine 
   scene type via empirically defined threshold values.
    Inputs  : global structure odtcurrent_v72 containing current intensity values
    Outputs : various elements within structure odtcurrent_v72
    Return  : -41 : error with FFT routine
              -51 : cloud temperature value <-100C or >+40C
	        0 : o.k.
*/
{
  int   nbin=64,iok,a,b,bptr;
  int   ixx,iyy,izz,iscene,idxcnt,maxsecd2,sxscnt;
  int   *sectorcnt,cnteye;
  float bd[534],cbd[534],tbd[534];
  float radb,rade,radeye,teye,dx2,eyecnt,rngcnt;
  float xangle,xdist,xtemp,slice;
  float *sectordiffa,*sectormin;
  float **sector,*eyearr,*sxs,sectang1,sectang2;
  float *avex,*stdvx,*skewx,*xs,*i2;
  float Aaveext,Astdvext,Askewext,Aavesym,Astdvsym,Askewsym;
  float Eaveeye,Estdveye,Eskeweye;
  float alsdist,alsradb,alsrade,alssum,alst,alscnt;
  struct ringdata *tcirc;

  float eyetemp;
  float eyecdosize,rmw;
  int eyecat;
    char  cdate[13],*hfilex;
    FILE *fp;

  /* initialize temperature histogram bin values */
  for(iyy=0;iyy<nbin;iyy++) {
    bd[iyy]=26.0-(float)iyy*2.0;
  }
  /* set up arrays for FFT ananlysis.
     iscene=0 will perform FFT for cloud top region while
     iscene=1 will perform FFT for eye region */
  for(iscene=0;iscene<=1;iscene++) {
    for(iyy=0;iyy<nbin;iyy++) {
      cbd[iyy]=0.0;
      tbd[iyy]=0.0;
    }

    /* define start and end radii values */
    if(iscene==0) {
      /* CLOUD TOP */
      radb=(float)kstart_v72;
      rade=(float)kend_v72;
    } else {
      /* EYE REGION */
      radb=0.0;
      rade=(float)kstart_v72;
    }

    /* load arrays for FFT analysis */
    tcirc=tcircfirst_v72;
    while(tcirc!=0) {
      if((tcirc->dist>=radb)&&(tcirc->dist<=rade)) {
        teye=(tcirc->temp - 273.16);
        for(ixx=0;ixx<(nbin-1);ixx++) {
          if((teye<=bd[ixx])&&(teye>=bd[ixx+1])) {
            cbd[ixx]=cbd[ixx]+1.0;
            tbd[ixx]=tbd[ixx]+teye;
          }
        }
      }
      tcirc=tcirc->nextrec;
    }

    /* perform FFT analysis */
    iok=aodtv72_fft(cbd,&dx2,&idxcnt);
    if(iok<0) return -41;
    
    /* assign variables based upon region being analyzed */
    if(iscene==0) {
      rngcnt=idxcnt; /* CLOUD TOP */
    } else {
      eyecnt=idxcnt; /* EYE REGION */
    }
  }

  /* compute various cloud and eye region parameters for classification scheme */

  /* allocate memory for arrays */
  a=sizeof(int);
  sectorcnt=(int *)calloc((size_t)maxsec,a);
  b=sizeof(float);
  bptr=sizeof(float*);
  sectormin=(float *)calloc((size_t)maxsec,b);
  sector=(float **)calloc((size_t)maxsec,bptr);
  for(ixx=0;ixx<maxsec;ixx++) {
    sector[ixx]=(float *)calloc((size_t)maxsecA,b);
  }
  eyearr=(float *)calloc((size_t)maxsecA,b);

  for(ixx=0;ixx<maxsec;ixx++) {
    sectorcnt[ixx]=0;
    sectormin[ixx]=999.0;
    for(iyy=0;iyy<maxsecA;iyy++) {
      sector[ixx][iyy]=-999.99;
    }
  }

  /* load array for analysis */
  radb=(float)kstart_v72;
  rade=(float)kend_v72;
  radeye=(float)kstart_v72;
  tcirc=tcircfirst_v72;
  slice=360.0/(float)maxsec;
  cnteye=0;
  while(tcirc!=0) {
    xangle=tcirc->angle;
    xdist=tcirc->dist;
    xtemp=tcirc->temp-273.16;
    if(xangle==360.0) xangle=0.0;
    ixx=0;
    if((xdist>=radb)&&(xdist<=rade)) {
      while(ixx<maxsec) {
        sectang1=A_MAX(0.0,(float)ixx*slice);
        sectang2=A_MIN(360.0,(float)(ixx+1)*slice);
        if((xangle>=sectang1)&&(xangle<sectang2)) {
          sector[ixx][sectorcnt[ixx]]=xtemp;
          sectorcnt[ixx]++;
          ixx=maxsec;
        } else {
          ixx++;   
        }
      }
    }
    if((xdist>=0)&&(xdist<=radeye)) {
      eyearr[cnteye]=xtemp;
      cnteye++;
    }
    /* the following will count all values w/in the different BD curve
       ranges for the whole cloud region so we can examine the structure
       of the cloud region later */
    tcirc=tcirc->nextrec;
  }

  /* position annulus at CW max temp distance and 
     determine mean temp w/in +/- 40km from this distance.  If dist
     is less than 68km from center, annulus will start at 28km */
  alscnt=0;
  alssum=0.0;
  alsdist=(float)odtcurrent_v72->IR.cwring;
  alsradb=A_MAX(28.0,alsdist-40.0);
  alsrade=A_MAX(108.0,alsdist+40.0);
  tcirc=tcircfirst_v72;
  while(tcirc!=0) {
    xdist=tcirc->dist;
    xtemp=tcirc->temp-273.16;
    if((xdist>=alsradb)&&(xdist<=alsrade)) {
      alssum=alssum+xtemp;
      alscnt++;
    }
    tcirc=tcirc->nextrec;
  }
  alst=alssum/(float)alscnt;
  odtcurrent_v72->IR.cloudt=alst;
  if((odtcurrent_v72->IR.cloudt<-100.0)||(odtcurrent_v72->IR.cloudt>40.0)) return -51;

  /* determine "minimum" temperature for each sector.  This is not the actual
     lowest temperature, but instead is the temperature value of the coldest
     90% of the values within the sector.  This will, hopefully, eliminate some
     outliers */

  /* allocate memory */
  xs=(float *)calloc((size_t)maxsecA,b);
  i2=(float *)calloc((size_t)maxsecA,b);
  sxs=(float *)calloc((size_t)maxsecA,b);

  for(ixx=0;ixx<maxsec;ixx++) {
    sxscnt=sectorcnt[ixx];
    for(iyy=0;iyy<sxscnt;iyy++) {
      sxs[iyy]=sector[ixx][iyy];
    }
    aodtv72_xxsort(sxs,xs,i2,sxscnt);
    izz=sxscnt-(sxscnt*.1);
    sectormin[ixx]=xs[izz];
  }

  /* free memory */
  free(sxs);
  free(i2);
  free(xs);

  /* determine averages, standard deviations and skews for each sector */
  /* allocate memory */
  avex=(float *)calloc((size_t)maxsec,b);
  stdvx=(float *)calloc((size_t)maxsec,b);
  skewx=(float *)calloc((size_t)maxsec,b);
  sectordiffa=(float *)calloc((size_t)maxsec,b);
  for (ixx=0;ixx<maxsec;ixx++) {
    aodtv72_calcskew(sector[ixx],sectorcnt[ixx],&avex[ixx],&stdvx[ixx],&skewx[ixx]);
  }

  aodtv72_calcskew(avex,maxsec,&Aaveext,&Astdvext,&Askewext);
  odtcurrent_v72->IR.cloudt2=Aaveext;

  /* these are used to examine symmetry of convection */
  maxsecd2=maxsec/2;
  for (ixx=0;ixx<maxsecd2;ixx++) {
    sectordiffa[ixx]=A_ABS(avex[ixx]-avex[maxsecd2+ixx]);
  }
  aodtv72_calcskew(sectordiffa,maxsecd2,&Aavesym,&Astdvsym,&Askewsym);
  /* these are used to examine properties of the eye region */
  aodtv72_calcskew(eyearr,cnteye,&Eaveeye,&Estdveye,&Eskeweye);
  odtcurrent_v72->IR.eyestdv=Estdveye;
  odtcurrent_v72->IR.cloudsymave=Aavesym;
  odtcurrent_v72->IR.eyefft=(int)eyecnt;
  odtcurrent_v72->IR.cloudfft=(int)rngcnt;

  /* free memory */
  free(sectordiffa);
  free(skewx);
  free(stdvx);
  free(avex);
  for(ixx=0;ixx<maxsec;ixx++) {
    free(sector[ixx]);
  }
  free(sector);
  free(eyearr);
  free(sectormin);
  free(sectorcnt);

  /* assign scenetype value in structure odtcurrent_v72 */
  aodtv72_classify();

  return 0;
}

void aodtv72_classify(void)
/* Classify scene type based on FFT analysis and histogram temperatures
   using empirically defined threshold values.
    Inputs  : global structure odtcurrent_v72 containing current image analysis
    Outputs : scenetype in structure odtcurrent_v72 is modified

    SCENE TYPES : EYE REGION         CLOUD REGION
                  0 - clear          0 - uniform
                  1 - pinhole        1 - embedded center
                  2 - large          2 - irregular cdo
                  3 - none           3 - curved band
                                     4 - shear
*/
{
  int   iok,ixx,iyy,cloudfft,cloudcat,eyefft,eyecat,cwtcat,diffcat;
  int   cbring,cbringval,diffcloudcat;
  int   sceneeye,scenecloud,spiral,spiralmax;
  int   lastcscene,lastescene,cbringvalmax;
  float xlat,xlon,tempval,lasttno,lasttno12;
  float eyetemp,cloudtemp,cloudcwt,eyestdv,cloudsyma;
  float essizeDG=0.0,esdiffDG=0.0;
  float lat1,lon1,lat2,lon2;
  float eyecdosize,diffcloudt,cdosize,cdosizex,rmw;
  float cbringlatmax,cbringlonmax;
  double curtime,curtimem12,xtime,lastvalidtime;
  struct odtdata *odthistory;
  logical cbfound,cbscene,cbgray,shear,irrcdo,landcheck;
    float cdodiam[6] = { 0.0,85.0,140.0,195.0,250.0,999.0 };
    float xpart,ddvor;
    int   cdocat;
    char  cdate[13],*hfilex;
    FILE *fp;
    logical embdd=FALSE,checkembc=FALSE;
  float Ea,Eb,Ec,Ed,Ee,eyeval,Ca,Cb,Cc,Cd,Ce,cloudval;
  float diffeyecloudt,eyecloudcatdiff,eyecwtcatdiff,cloudcatdiff;
  float eyepart,cloudpart,cwtpart,diffcat2,lastvalidtno,lastr9,maxtno;
  float cdomax[7]= { 0.0,0.4,0.4,0.5,0.5,0.6,0.6 };
  int   diffeyecloudcat;
  logical foundeye,foundm12;


  eyetemp=odtcurrent_v72->IR.eyet;
  eyefft=odtcurrent_v72->IR.eyefft;
  eyestdv=odtcurrent_v72->IR.eyestdv;
  cloudtemp=odtcurrent_v72->IR.cloudt;
  cloudcwt=odtcurrent_v72->IR.cwcloudt;
  cloudfft=odtcurrent_v72->IR.cloudfft;
  cloudsyma=odtcurrent_v72->IR.cloudsymave;
  xlat=odtcurrent_v72->IR.latitude;
  xlon=odtcurrent_v72->IR.longitude;

  for(ixx=0;ixx<10;ixx++) {
    /* compute cloud category */
    if((cloudtemp<=ebd_v72[ixx])&&(cloudtemp>ebd_v72[ixx+1])) {
      cloudcat=ixx;
      xpart=(cloudtemp-ebd_v72[cloudcat])/(ebd_v72[cloudcat+1]-ebd_v72[cloudcat]);
      if(cloudcat==0) xpart=0.0;
      cloudpart=cloudcat+xpart;
    }
    /* compute eye category */
    if((eyetemp<=ebd_v72[ixx])&&(eyetemp>ebd_v72[ixx+1])) {
      eyecat=ixx;
      xpart=(eyetemp-ebd_v72[eyecat])/(ebd_v72[eyecat+1]-ebd_v72[eyecat]);
      if(eyecat==0) xpart=0.0;
      eyepart=eyecat+xpart;
    }
    /* compute C-W eye category */
    if((cloudcwt<=ebd_v72[ixx])&&(cloudcwt>ebd_v72[ixx+1])) {
      cwtcat=ixx;
      xpart=(cloudcwt-ebd_v72[cwtcat])/(ebd_v72[cwtcat+1]-ebd_v72[cwtcat]);
      if(cwtcat==0) xpart=0.0;
      cwtpart=cwtcat+xpart;
    }
  }

  /* printf("EYE = temp=%f cat=%d part=%f \n",eyetemp,eyecat,eyepart); */
  /* printf("CLD = temp=%f cat=%d part=%f \n",cloudtemp,cloudcat,cloudpart); */
  /* printf("CWT = temp=%f cat=%d part=%f \n",cloudcwt,cwtcat,cwtpart); */
  diffcat=A_MAX(0,A_MAX(cloudcat,cwtcat)-eyecat);
  diffcloudt=cloudtemp-cloudcwt;
  diffeyecloudt=eyetemp-cloudtemp;
  eyecwtcatdiff=cwtpart-eyepart;
  eyecloudcatdiff=cloudpart-eyepart;
  cloudcatdiff=cloudpart-cwtpart;
  diffcloudcat=cloudcat-cwtcat;
  diffeyecloudcat=cloudcat-eyecat;
  diffcat2=eyetemp-(A_MIN(cloudtemp,cloudcwt));

  curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);
  curtimem12=curtime-0.5;

  /* determine last Final T# value for curved band/other scene check */
  foundeye=FALSE;
  maxtno=0.0;
  lastr9=0;
  lasttno=maxtno;
  lastvalidtno=lasttno;
  if((odthistoryfirst_v72==0)||(strlen(hfile_v72)==0)) {
    foundm12=TRUE;
    lastescene=3;
    lastr9=1;
    if((cwtpart<3.5)&&(osstr_v72<3.5)) {
      lastcscene=3;
      lasttno12=osstr_v72;
    } else {
      lastcscene=0;
      lasttno12=A_MAX(osstr_v72,4.0);
    }
  } else {
    odthistory=odthistoryfirst_v72;
    foundm12=FALSE;
    lastcscene=3;
    while(odthistory!=0) {
      xtime=aodtv72_calctime(odthistory->IR.date,odthistory->IR.time);
      landcheck=TRUE;
      if(((oland_v72)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) landcheck=FALSE;
      if((xtime<curtime)&&(landcheck)) {
        lastvalidtime=xtime;
        if((xtime>=curtimem12)&&(!foundm12)) {
          lasttno12=odthistory->IR.Tfinal;
          foundm12=TRUE;
        }
        lasttno=odthistory->IR.Tfinal;
        lastcscene=odthistory->IR.cloudscene;
        lastescene=odthistory->IR.eyescene;
	if(lastescene<=2) foundeye=TRUE;
	if((lastcscene==4)&&(lastescene==3)) foundeye=FALSE;
        lastvalidtno=lasttno;
        lastr9=odthistory->IR.rule9;
        if(lasttno>maxtno) maxtno=lasttno;
      } else {
        if(!landcheck) {
          /* if over land for more than 12 hours, turn off foundeye */
          if((xtime-lastvalidtime)>0.5) {
            foundeye=FALSE;
            lasttno=lastvalidtno-(1.0*(xtime-lastvalidtime));
            /* printf("lastvalidtno=%f  deltatime=%f lasttno=%f\n",lastvalidtno,xtime-lastvalidtime,lasttno); */
          }
        }
      }
      odthistory=odthistory->nextrec;
    }
    /* check for large break in history file */
    if(!foundm12) lasttno12=lasttno;
  }
  /* printf("foundm12=%d\n",foundm12); */
  /* printf("lasttno12=%f\n",lasttno12); */

  /* NEW SCENE IDENTIFICATION SCHEME */
  /* NEW EYE SCENE THRESHOLD DETERMINATION */
  Ec=0.0;
  Ee=0.0;
  Ea=1.0-((eyefft-2)*0.1);
  Eb=-(eyepart*0.5);
  if(eyestdv>10.0) Ec=0.50;
  /* Ed=eyecloudcatdiff*0.75; */
  Ed=(eyecloudcatdiff*0.25)+(eyecwtcatdiff*0.50); /* new */
  /* printf("Ed=%f\n",Ed); */
  /* printf("maxtno=%f Ee=%f\n",maxtno,Ee); */
  if((foundm12)&&(lastescene<3)&&(maxtno>5.0)) Ee=Ee+0.25;
  /* printf("Ee=%f\n",Ee); */
  /* if(lasttno12<3.5) Ee=Ee-1.0; */
  if(lasttno12<=4.5) Ee=A_MAX(-1.0,lasttno12-4.5);  /* new */
  /* printf("lasttno12=%f  Ee=%f\n",lasttno12,Ee); */
  if((lastr9>0)&&(lasttno<4.0)) Ee=Ee-0.5;  /* new */
  /* printf("Ee=%f\n",Ee); */
  eyeval=Ea+Eb+Ec+Ed+Ee;
  sceneeye=3;                    /* NO EYE */
  if(eyeval>=0.50) sceneeye=0;   /* EYE */
/*
  if(eyeval>=0.00) sceneeye=5;   / OBSCURED EYE /
  if(eyeval>=1.50) sceneeye=4;   / RAGGED EYE /
  if(eyeval>=3.50) sceneeye=0;   / CLEAR EYE /
*/

  /* printf("eyeval= %f  sceneeye=%d \n",eyeval,sceneeye); */
  eyecdosize=0.0;
  if(rmwsizeman_v72>0) {
    /* printf("RMW SIZE=%d\n",rmwsizeman_v72); */
    odtcurrent_v72->IR.rmw=(float)rmwsizeman_v72;
    eyecdosize=(float)rmwsizeman_v72-1.0;             /* manually input eye size */
  } else {
    iok=aodtv72_rmw(&rmw,&eyecdosize);
    odtcurrent_v72->IR.rmw=rmw;
  }

  /* LARGE EYE CHECKS */
  if((sceneeye==0)&&(eyecdosize>=45.0)) sceneeye=2;   /* large eye */

  /* NEW CLOUD SCENE THRESHOLD DETERMINATION */
  shear=FALSE;
  irrcdo=FALSE;
  cbscene=TRUE;
  cbgray=TRUE;

  Cc=0.0;
  Cd=0.5;   /* changed to 0.5 */
  Ce=0.0;
  Ca=cwtpart*0.25;
  Cb=cloudpart*0.25;
  if(cloudfft<=2) Cc=A_MIN(1.50,cwtpart*0.25);
  if(lastcscene>=3) Cd=-0.50;
  /* printf("cwtpart=%f lasttno12=%f\n",cwtpart,lasttno12); */
  if(cwtpart>2.0) {  /* new */
    if(lasttno12>=2.5) {
      if(sceneeye==0) Ce=A_MIN(1.00,lasttno12-2.5);
      if(lasttno12>=3.5) Ce=Ce+1.00;
    }
    if((foundm12)&&(foundeye)) Ce=Ce+1.25;
  }
  cloudval=Ca+Cb+Cc+Cd+Ce;
  if(cloudval<0.0) shear=TRUE;                                 /* SHEAR */
  if(cloudval>=0.00) cbscene=TRUE;                             /* CURVED BAND (gray) */
  if(cloudval>=1.00) {
    cbscene=TRUE;                                              /* CURVED BAND (gray) */
    /* check for irregular CDO */
    if((diffcat2<0.0)&&(cloudsyma>40.0)) {
      irrcdo=TRUE;                                             /* IRREGULAR CDO */
    }
  }
  if((cloudval>=2.00)&&(cloudval<3.00)) {
    cbscene=TRUE;                                              /* CURVED BAND (gray) */
    /* check for irregular CDO */
    /* if((diffcloudcat<0)&&(diffcloudt>8.0)&&(cloudsyma>30.0)) { */
    if((diffcat2<0.0)&&(cloudsyma>30.0)) {
      irrcdo=TRUE;                                             /* IRREGULAR CDO */
    }
    if(cwtcat>=3) {
      /* if xcwt>3.0 try black/white CB check */
      if((diffcloudcat>0)&&(diffcloudt<-8.0)) cbgray=FALSE;      /* CURVED BAND (black/white) */
      /* check for large/ragged eye */
      if((sceneeye==0)||((eyepart>1.00)&&(diffeyecloudcat>=2.00))) cbscene=FALSE; /* EYE */
      /* check for CDO */
      if((cloudcatdiff<=0.0)&&(eyecwtcatdiff<1.00)) cbscene=FALSE; /* CDO */
    }
  }
  if(cloudval>=3.00) {
    cbscene=FALSE;                                             /* CDO */
    /* check for irregular CDO */
    if((diffcloudcat<0)&&(diffcloudt>8.0)&&(cloudsyma>30.0)) {
      irrcdo=TRUE;                                             /* IRREGULAR CDO */
      cbscene=TRUE;
    }
  }
  /* EMBEDDED CENTER CHECK */
  if((cloudtemp<cloudcwt)&&(cloudcwt<eyetemp)) checkembc=TRUE;
  if((!cbscene)&&(checkembc)) {
    tempval=ebd_v72[cwtcat+1]+273.16;
    aodtv72_logspiral(xlat,xlon,tempval,1,&spiral,&lat1,&lon1);
    if((spiral>=8)&&(spiral<20)) embdd=TRUE;
    /* printf(" EMBDD : cwtcat=%d spiral=%d \n",cwtcat,spiral); */
  }

  /* printf("cloudval= %f  shear=%d cbscene=%d cbgray=%d irrcdo=%d \n",cloudval,shear,cbscene,cbgray,irrcdo); */
  (void)aodtv72_julian2cmonth(odtcurrent_v72->IR.date,cdate);
   /* printf("%9s %6d
         %4.1f %4.1f %2d %2d %5.1f %5.1f  %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f
         %2d %2d %4.1f %4.1f  %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f   %6.2f %7.2f  %3.1f  \n",cdate,odtcurrent_v72->IR.time,
         cloudpart,cwtpart,cloudfft,diffcloudcat,diffcloudt,cloudsyma,Ca,Cb,0.0,Cc,Cd,Ce,cloudval,
         eyefft,diffeyecloudcat,eyepart,eyestdv,Ea,Eb,Ec,Ed,Ee,eyeval,xlat,xlon,lasttno); */

  /* CLASSIFY CLOUD REGION */
  cbring=0;
  cbringval=0;
  cbringvalmax=0;
  cbringlatmax=xlat;
  cbringlonmax=xlon;

L100:
  if(cbscene) {
    if(shear) {
      sceneeye=3;                   /* NO EYE */
      scenecloud=4;                 /* SHEAR */
      tempval=ebd_v72[3]+273.16;
      aodtv72_cdoshearcalc(xlat,xlon,tempval,3,&essizeDG,&esdiffDG);
      eyecdosize=A_MAX(4.0,essizeDG);
    } else if(irrcdo) {
      sceneeye=3;            /* NO EYE */
      scenecloud=2;          /* IRREGULAR CDO */
    } else {
      cbfound=FALSE;
L200:
      if(cbgray) {
        /* perform Curved Band analysis */
        ixx=4;  /* start with LIGHT GRAY */
        while((ixx>=2)&&(!cbfound)) {
          tempval=ebd_v72[ixx]+273.16;
          aodtv72_logspiral(xlat,xlon,tempval,1,&spiral,&lat1,&lon1);
          /* printf("BD level=%d  spiral=%d\n",ixx,spiral); */
          if((spiral>=8)||(ixx==2)) {   /* 10 = .375% -- 10 ==> 9 arcs of 15 degrees */
            if(spiral>25) {
              if(ixx==4) {
                cbgray=FALSE;
                goto L200;
              } else {
                ixx=0;
              }
            } else {
              if((ixx==2)&&(spiral<7)) {  /* 7 = .25% -- 10 ==> 6 arcs of 15 degrees */
                /* probably shear */
                cbfound=FALSE;
                shear=TRUE;
                goto L100;
              } else {
                cbfound=TRUE;
              }
            }
          } else {
            ixx--;
          }
        }
      } else {
        /* try BLACK and WHITE rings */
        cbscene=FALSE;
        ixx=6;
        while((ixx>4)&&(!cbfound)) {
          tempval=ebd_v72[ixx]+273.16;
          aodtv72_logspiral(xlat,xlon,tempval,1,&spiral,&lat1,&lon1);
          if((spiral>=9)&&(spiral<=25)) {
            cbfound=TRUE;
          } else {
            ixx--;
          }
        }
      }
      if(cbfound) {
        /* found curved band scenes */
        cbring=ixx;
        cbringval=spiral;
        sceneeye=3;                   /* NO EYE */
        scenecloud=3;                 /* CURVED BAND */
        /* search for maximum curved band analysis location within 1-degree box */
        tempval=ebd_v72[cbring]+273.16;
        if(osearch_v72) {
          aodtv72_logspiral(xlat,xlon,tempval,2,&spiralmax,&lat2,&lon2);
          cbringvalmax=spiralmax;
          cbringlatmax=lat2;
          cbringlonmax=lon2;
        } 
      } else {
        /* did not find curved band scenes, mark as non-eye/eye scene */
        scenecloud=0;
        cbscene=FALSE;
        embdd=FALSE;  /* redundant declaration */
        goto L100;
      }
    }
  } else {
    scenecloud=0;            /* UNIFORM */
    if(embdd) scenecloud=1;  /* EMBEDDED CENTER */
    /* PINHOLE EYE TEST */
    /* printf("sceneeye=%d diffeyecloudcat=%d eyefft=%d cwtpart=%f scenecloud=%d cloudfft=%d lasttno12=%f\n",
    sceneeye,diffeyecloudcat,eyefft,cwtpart,scenecloud,cloudfft,lasttno12); */
    if((rmwsizeman_v72>0)&&(rmwsizeman_v72<5.0)) sceneeye=1;
    if((eyeval>-0.25)&&(eyeval<1.50)&&(diffeyecloudcat>=2)&&(eyefft<=2)&&
       (cwtpart>6.0)&&(scenecloud<=1)&&(cloudfft<=4)&&(lasttno12>=3.5)) {
      sceneeye=1;            /* PINHOLE EYE CHECK */
    }
  }
  if((scenecloud<=2)&&(sceneeye==3)) { 
    /* for CDO TESTS */
    for(ixx=2;ixx<=6;ixx++) {  /* DG,MG,LG,B,W */
      tempval=ebd_v72[ixx]+273.16;
      /* printf("xlat=%f xlon=%f tempval=%f\n",xlat,xlon,tempval); */
      aodtv72_cdoshearcalc(xlat,xlon,tempval,1,&cdosize,&cdosizex);
      /* printf("CDO : ixx=%d  cdosize=%f  cdosize/111=%f  \n",ixx,cdosize,cdosize/111.0); */
      if(ixx==2) eyecdosize=cdosize;
    }
  }

  odtcurrent_v72->IR.eyescene=sceneeye;
  odtcurrent_v72->IR.cloudscene=scenecloud;
  odtcurrent_v72->IR.eyesceneold=-1;
  odtcurrent_v72->IR.cloudsceneold=-1;
  odtcurrent_v72->IR.eyecdosize=eyecdosize;
  odtcurrent_v72->IR.ringcb=cbring;
  odtcurrent_v72->IR.ringcbval=cbringval;
  odtcurrent_v72->IR.ringcbvalmax=cbringvalmax;
  odtcurrent_v72->IR.ringcblatmax=cbringlatmax;
  odtcurrent_v72->IR.ringcblonmax=cbringlonmax;

}

void aodtv72_classifyredo(void)
{
  int   ixx,scene,cbring,cbringval,spiral,spiralmax;
  int   cbringvalmax;
  float xlat,xlon,tempval;
  float lat1,lon1,lat2,lon2,essizeDG,esdiffDG;
  float eyecdosize,cbringlatmax,cbringlonmax,cdosize,cdosizex;
  logical cbfound=FALSE;

  xlat=odtcurrent_v72->IR.latitude;
  xlon=odtcurrent_v72->IR.longitude;
  scene=odtcurrent_v72->IR.cloudscene;
  cbringvalmax=0;
  cbringlatmax=xlat;
  cbringlonmax=xlon;
  if(scene==3) {
    /* perform curved band analysis -- find length of spiral arc */
    ixx=2;
    while((ixx<=4)&&(!cbfound)) {
      tempval=ebd_v72[ixx]+273.16;
      /* determine curved band analysis at user selected center */
      aodtv72_logspiral(xlat,xlon,tempval,1,&spiral,&lat1,&lon1);
      cbring=ixx;
      cbringval=spiral;
      /* search for maximum curved band analysis location within 1-degree box */
      if(osearch_v72) {
        aodtv72_logspiral(xlat,xlon,tempval,2,&spiralmax,&lat2,&lon2);
        cbringvalmax=spiralmax;
        cbringlatmax=lat2;
        cbringlonmax=lon2;
      }
      if(spiral<=25) {
        cbfound=TRUE;
      }
      ixx++;
    }
    if(cbring==4) {
      cbringval=A_MIN(25,cbringval);
      cbringvalmax=A_MIN(25,cbringvalmax);
      cbringlatmax=xlat;
      cbringlonmax=xlon;
    } 
    eyecdosize=0.0;
  } else {
    tempval=ebd_v72[2]+273.16;
    /* printf("xlat=%f xlon=%f tempval=%f\n",xlat,xlon,tempval); */
    aodtv72_cdoshearcalc(xlat,xlon,tempval,1,&cdosize,&cdosizex);
    /* printf("CDO : ixx=%d  cdosize=%f  cdosize/111=%f \n ",ixx,cdosize,cdosize/111.0); */
    eyecdosize=cdosize;
    cbring=0;
    cbringval=0;
  }
  odtcurrent_v72->IR.eyecdosize=eyecdosize;
  odtcurrent_v72->IR.ringcb=cbring;
  odtcurrent_v72->IR.ringcbval=cbringval;
  odtcurrent_v72->IR.ringcbvalmax=cbringvalmax;
  odtcurrent_v72->IR.ringcblatmax=cbringlatmax;
  odtcurrent_v72->IR.ringcblonmax=cbringlonmax;

}

void aodtv72_cdoshearcalc( float xlat,float xlon,float tempval,int atype,float *valb,float *valc)
/* Determine eye size or shear distance for a given scene.
    Inputs  : xlat    - center latitude of analysis grid
              xlon    - center longitude of analysis grid
              tempval - temperature threshold value to be used
              atype   - analysis type (1-cdo size,2-eye size,3-shear distance)
    Outputs : valb    - eye/cdo radius or shear distance
              valc    - eye/cdo symmetry value or 0
*/
{
  int ixx,iyy,np,b,numvalid;
  float xdist,xangle,smalldist,vc,maxdist;
  float *zlat,*zlon;
  float a1,a2,a3,a4,v1,v2,v3;

  /* allocate memory */
  b=sizeof(float);
  zlat=(float *)calloc((size_t)bufsiz,b);
  zlon=(float *)calloc((size_t)bufsiz,b);
  np=0;

  /* CDO size determination - RETURNS RADIUS */
  if(atype==1) {
    /* a1=999.9;a2=999.9;a3=999.9;a4=999.9; */
    a1=300.0;a2=300.0;a3=300.0;a4=300.0;
    for(ixx=0;ixx<areadata_v72->numx;ixx++) {
      for(iyy=0;iyy<areadata_v72->numy;iyy++) {
        if(areadata_v72->temp[iyy][ixx]>tempval) {
          zlat[np]=areadata_v72->lat[iyy][ixx];
          zlon[np]=areadata_v72->lon[iyy][ixx];
          np++;
        }
      }
    }
    /* printf("np=%d  numx*numy=%d\n",np,areadata_v72->numx*areadata_v72->numy); */
    maxdist=0.0;
    if(np<(areadata_v72->numx*areadata_v72->numy)) {
      for(ixx=0;ixx<np;ixx++) {
        aodtv72_distance(xlat,xlon,zlat[ixx],zlon[ixx],1,&xdist,&xangle);
        if(xdist>maxdist) maxdist=xdist;
        /* determine size of CDO */
        if(xdist>keyerM_v72){ 
          if((A_ABS(xangle-45.0)<=15.0)&&(xdist<a1)) a1=xdist;
          if((A_ABS(xangle-135.0)<=15.0)&&(xdist<a2)) a2=xdist;
          if((A_ABS(xangle-225.0)<=15.0)&&(xdist<a3)) a3=xdist;
          if((A_ABS(xangle-315.0)<=15.0)&&(xdist<a4)) a4=xdist;
        }
      }
      /* printf("a1=%f a2=%f a3=%f a4=%f\n",a1,a2,a3,a4); */
      numvalid=4;
      v3=keyerM_v72+kres_v72;
      if(a1<v3) numvalid--;
      if(a2<v3) numvalid--;
      if(a3<v3) numvalid--;
      if(a4<v3) numvalid--;
    } else {
      a1=0.0;
      a2=0.0;
      a3=0.0;
      a4=0.0;
    }
    if(numvalid<3) {
      *valb=0.0;
      *valc=0.0;
    } else {
      a1=A_MIN(a1,maxdist);
      a2=A_MIN(a2,maxdist);
      a3=A_MIN(a3,maxdist);
      a4=A_MIN(a4,maxdist);
      *valb=(a1+a2+a3+a4)/4.0;
      /* *valc=A_ABS(((a1+a3)/2.0)-((a2+a4)/2.0))/2.0; */
      v1=a1+a3;
      v2=a2+a4;
      vc=v1/v2;
      vc=A_MAX(vc,1.0/vc);
      *valc=vc;
    }
    /* printf("\nnp=%5.5d  a1=%5.1f a2=%5.1f a3=%5.1f a4=%5.1f  ",np,a1,a2,a3,a4); */
  }

  /* shear distance determination */
  if(atype==3) {
    a1=999.9;a2=999.9;a3=999.9;a4=999.9;
    for(ixx=0;ixx<areadata_v72->numx;ixx++) {
      for(iyy=0;iyy<areadata_v72->numy;iyy++) {
        if(areadata_v72->temp[iyy][ixx]<=tempval) {
          zlat[np]=areadata_v72->lat[iyy][ixx];
          zlon[np]=areadata_v72->lon[iyy][ixx];
          np++;
        }
      }
    }
    smalldist=999.9;
    for(ixx=0;ixx<np;ixx++) {
      aodtv72_distance(xlat,xlon,zlat[ixx],zlon[ixx],1,&xdist,&xangle);
      if(xdist<smalldist) smalldist=xdist;
    }
    *valb=smalldist;
    *valc=0.0;
  }

  /* free memory */
  free(zlon);
  free(zlat);

}

void aodtv72_xxsort(float *x1,float *x2,float *i2,int ndim)
{
  int   ih,i;
  float x,top;

  aodtv72_fminx(x1,ndim,&ih,&x);
  top=x-1.0;
  for(i=0;i<ndim;i++) {
    aodtv72_fmaxx(x1,ndim,&ih,&x);
    i2[i]=ih;
    x2[i]=x1[ih];
    x1[ih]=top;
  }
}

void aodtv72_fminx(float *f,int ndim,int *im,float *x)
{
  int i;

  *im=0;
  *x=f[0];
  for(i=1;i<ndim;i++) {
    if(f[i]<*x) {
      *x=f[i];
      *im=i;
    }
  }
}

void aodtv72_fmaxx(float *f,int ndim,int *im,float *x)
{
  int i;

  *im=0;
  *x=f[0];
  for(i=1;i<ndim;i++) {
    if(f[i]>*x) {
      *x=f[i];
      *im=i;
    }
  }
}
