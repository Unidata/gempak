/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int   aodtv72_calcintensity(void);
float aodtv72_Tnoraw(void); 
float aodtv72_Tnofinal( int ); 
float aodtv72_CIno( int * ); 
float aodtv72_TIEmodel(void);
void  aodtv72_maxtnoval( int,int *,float * );
float aodtv72_latbias( float,float,float );
/*int   aodtv72_tieflag(void);*/

int aodtv72_scenesearch( int,float * );
float pinhole(float,float,float);
float aodtv72_xlza(double,double,double,double);

int aodtv72_calcintensity(void)
/* Compute intensity values CI, Final T#, and Raw T#.
    Inputs  : global structure odtcurrent_v72 containing current analysis
    Outputs : none
    Return : 71 : storm is over land
              0 : o.k.
*/
{
  int   iok,iret,strength;
   

  if((odtcurrent_v72->IR.land==1)&&(oland_v72)) {
    iok=aodtv72_initcurrent(1);
    iret=71;
  } else {
    /* calculate current Raw T# value */
    odtcurrent_v72->IR.Traw=aodtv72_Tnoraw();

    /* check for spot analysis or full analysis using history file */
    /* if(hfile_v72==(char *)NULL) { */
    if((strlen(hfile_v72)==0)) {
      /* perform spot analysis (only Traw) */
      odtcurrent_v72->IR.Tfinal = odtcurrent_v72->IR.Traw;
      odtcurrent_v72->IR.Tfinal3 = odtcurrent_v72->IR.Traw;
      odtcurrent_v72->IR.CI = odtcurrent_v72->IR.Traw;
      odtcurrent_v72->IR.CIadjp=aodtv72_latbias(odtcurrent_v72->IR.CI,odtcurrent_v72->IR.latitude,odtcurrent_v72->IR.longitude);
      /* printf("%f %f %f   %f\n",odtcurrent_v72->IR.CI,odtcurrent_v72->IR.latitude,odtcurrent_v72->IR.longitude,odtcurrent_v72->IR.CIadjp); */
      odtcurrent_v72->IR.rule9 = 0;
      /*odtcurrent_v72->IR.TIEraw=aodtv72_TIEmodel();*/
      /*odtcurrent_v72->IR.TIEavg=odtcurrent_v72->IR.TIEraw;*/
      /*odtcurrent_v72->IR.TIEflag=aodtv72_tieflag(); */
    } else {
      if(((odthistoryfirst_v72==0)&&(ostartstr_v72==TRUE))&&((strlen(hfile_v72)!=0))) {
        odtcurrent_v72->IR.Tfinal = odtcurrent_v72->IR.Traw;
        odtcurrent_v72->IR.Tfinal3 = odtcurrent_v72->IR.Traw;
        odtcurrent_v72->IR.CI = odtcurrent_v72->IR.Traw;
      } else {
        /* perform full analysis (Tfinal and CI) */
        odtcurrent_v72->IR.Tfinal = aodtv72_Tnofinal(0);
        odtcurrent_v72->IR.Tfinal3 = aodtv72_Tnofinal(1);
        odtcurrent_v72->IR.CI = aodtv72_CIno(&strength);
        odtcurrent_v72->IR.rule9=strength;
        /*odtcurrent_v72->IR.TIEraw=aodtv72_TIEmodel();*/
        /*odtcurrent_v72->IR.TIEavg=aodtv72_Tnofinal(2);*/
        /*odtcurrent_v72->IR.TIEflag=aodtv72_tieflag();*/
      }
    }
    iret=0;
  }

  return iret;
}

float aodtv72_Tnoraw(void)
/* Compute initial Raw T-Number value using original Dvorak rules
    Inputs  : global structure odtcurrent_v72 containing current analysis
    Outputs : return value is Raw T#

                       ODT SCENE/TEMPERATURE TABLE
    BD   | WMG   OW    DG    MG    LG     B     W    CMG   CDG | 
    TEMP |30.0   0.0 -30.0 -42.0 -54.0 -64.0 -70.0 -76.0 -80.0+|
---------------------------------------------------------------|
Atl EYE  | 3.5   4.0   4.5   4.5   5.0   5.5   6.0   6.5   7.0 |
    EMBC | 3.5   3.5   4.0   4.0   4.5   4.5   5.0   5.0   5.0 |
    CDO  | 3.0   3.0   3.5   4.0   4.5   4.5   4.5   5.0   5.0 |
---------------------------------------------------------------|
Pac EYE  | 4.0   4.0   4.0   4.5   4.5   5.0   5.5   6.0   6.5 |
    EMBC | 3.5   3.5   4.0   4.0   4.5   4.5   5.0   5.0   5.0 |
    CDO  | 3.0   3.5   3.5   4.0   4.5   4.5   4.5   4.5   5.0 |
---------------------------------------------------------------|
Cat diff |  0     1     2     3     4     5     6     7     8  |
    add  | 0.0   0.0   0.0   0.0   0.0-->0.5   0.5-->1.0   1.5 | (old)
    add  |-0.5  -0.5   0.0   0.0-->0.5   0.5   0.5-->1.0   1.0 | (new)
---------------------------------------------------------------|
*/
{
/*                               DG   MG   LG   B    W    CMG  CDG           */ 
/*                                             -64  -70  -76  -80  -84 -100  */ 
/*float eno[2][11]={ {2.20,2.90,4.25,4.75,5.15,5.50,5.75,6.00,6.50,6.75,7.50},  ADT7.1 - original plus adjusted > CDG+ /
                     {1.60,2.25,3.40,3.75,4.15,4.50,4.60,4.80,5.00,5.25,5.75} };  */ 
  float eno[2][11]={ {1.00,2.00,3.25,4.00,4.75,5.50,5.90,6.50,7.00,7.50,8.00},     /* original plus adjusted > CDG+ */
                     {1.50,2.25,3.30,3.85,4.50,5.00,5.40,5.75,6.25,6.50,7.00} };   /* adjusted based on observation that values are low */
  float cdo[2][11]={ {2.00,2.40,3.25,3.50,3.75,4.00,4.10,4.20,4.30,4.40,4.70},
                     {2.05,2.40,3.00,3.20,3.40,3.55,3.65,3.75,3.80,3.90,4.10} };
  float curbnd[7] =  {1.0,1.5,2.5,3.0,3.5,4.0,4.5};
  float shrdst[6] =  {0.0,35.0,50.0,80.0,110.0,140.0};
  float shrcat[6] =  {3.5, 3.0, 2.5, 2.25,  2.0,  1.5};

  float diffchk[3][10] = { {0.0,0.5,1.2,1.7,2.2,2.7,0.0,0.0,0.1,0.5 },   /* shear scene types... original Rule 8 rules */
                           {0.0,0.5,1.7,2.2,2.7,3.2,0.0,0.0,0.1,0.5 },   /*   eye scene types... add 0.5 to Rule 8 rules */
                           {0.0,0.5,0.7,1.2,1.7,2.2,0.0,0.0,0.1,0.5 } }; /* other scene types... subtract 0.5 from Rule 8 rules */
  float eyeadjfacEYE[2] = { 0.011, 0.015 };    /* modified wpac value to be closer to atlantic */
  float symadjfacEYE[2] = { -0.015, -0.015 };
  float dgraysizefacCLD[2] = { 0.002 , 0.001 };
  float symadjfacCLD[2] = { -0.030, -0.015 };

  int diffchkcat;
  int ixx,cloudcat,eyecat,diffcat,rp,xrp,rb;
  float incval,lastci,lasttno,lastr9,lastraw;
  float xpart,xparteye,xaddtno,eyeadj,spart,ddvor,dvorchart,ciadj;
  float sdist,cloudtemp,eyetemp,fftcloud;
  float t1val,t6val,t12val,t18val,t24val,delt1,delt6,delt12,delt18,delt24;
  float t1valraw,t1valrawx,txvalmin,txvalmax;
  double curtime,xtime,firsttime,firstlandtime;
  double ttime1,ttime6,ttime12,ttime18,ttime24,t1valrawxtime;
  struct odtdata *odthistory,*prevrec;
  logical oceancheck,adjustshear,firstland;
  logical t1found=FALSE,t6found=FALSE,t12found=FALSE,t18found=FALSE,t24found=FALSE;
  logical first6hrs=FALSE;
  float  symadj,dgraysizeadj,deltaT;

  /* if(((odthistoryfirst_v72==0)&&(ostartstr_v72==TRUE))&&(hfile_v72!=(char *)NULL)) { */
  if(((odthistoryfirst_v72==0)&&(ostartstr_v72==TRUE))&&((strlen(hfile_v72)!=0))) { 
    odtcurrent_v72->IR.TrawO=osstr_v72;
    return osstr_v72;
  } else {
    odthistory=odthistoryfirst_v72;
    curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);
    /* find record just prior to current record */
    prevrec=odthistory;
    while(odthistory!=0) {
      xtime=aodtv72_calctime(odthistory->IR.date,odthistory->IR.time);
      if((xtime==curtime)&&(odthistory==odthistoryfirst_v72)&&(osstr_v72!=0.0)) {
        odtcurrent_v72->IR.TrawO=osstr_v72;
        return osstr_v72;
      }
      if(xtime>curtime) break;
      prevrec=odthistory;
      odthistory=odthistory->nextrec;
    }
    if(prevrec!=0) {
      lastci=prevrec->IR.CI;
    } else {
      lastci=4.0;
    }
  }

  cloudtemp=odtcurrent_v72->IR.cloudt;
  eyetemp=odtcurrent_v72->IR.eyet;

  for(ixx=0;ixx<10;ixx++) {
    /* compute cloud category */
    if((cloudtemp<=ebd_v72[ixx])&&(cloudtemp>ebd_v72[ixx+1])) {
      cloudcat=ixx;
      xpart=(cloudtemp-ebd_v72[cloudcat])/(ebd_v72[cloudcat+1]-ebd_v72[cloudcat]);
    }
    /* compute eye category for eye adjustment */
    if((eyetemp<=ebd_v72[ixx])&&(eyetemp>ebd_v72[ixx+1])) {
      eyecat=ixx;
    }
    /* eyetemp=A_MIN(0.0,eyetemp); */
  }
  if(odtcurrent_v72->IR.eyescene==1) {
    /* for pinhole eye, determine what storm should be seeing */
    /* eyetemp=pinhole(odtcurrent_v72->IR.latitude,odtcurrent_v72->IR.longitude,eyetemp); */
    /*eyetemp=(9.0-eyetemp)/2.0;  / this matches DT used at NHC (jack beven) */
    eyetemp=(eyetemp-9.0)/2.0;  /* between +9C (beven) and measured eye temp (turk) */
    odtcurrent_v72->IR.eyet=eyetemp;
  }

  /* category difference between eye and cloud region */
  diffcat=A_MAX(0,cloudcat-eyecat);
  
  /* if scenetype is EYE */
  rp=odtcurrent_v72->IR.ringcbval;
  rb=odtcurrent_v72->IR.ringcb;
  fftcloud=odtcurrent_v72->IR.cloudfft;
  if(odtcurrent_v72->IR.cloudscene==3) {
    /* CURVED BAND */
    rp=A_MIN(30,rp+1);   /* added 1 for testing */
    xrp=rp/5;
    incval=0.1;
    if(xrp==1) incval=0.2;
    ddvor=curbnd[xrp];
    xaddtno=incval*(float)(rp-(xrp*5));
    /* printf("rp=%d  xrp=%d  rb=%d  ddvor=%f  xaddtno=%f\n",rp,xrp,rb,ddvor,xaddtno); */
    ddvor=ddvor+xaddtno;
    if(rb==5) ddvor=A_MIN(4.0,ddvor+0.5);
    if(rb==6) ddvor=A_MIN(4.5,ddvor+1.0);
    diffchkcat=2;   /* added for test - non-eye/shear cases */
  } else if(odtcurrent_v72->IR.cloudscene==4) {
    /* POSSIBLE SHEAR -- new definition from NHC */
    ixx=0;
    ddvor=1.0;
    sdist=odtcurrent_v72->IR.eyecdosize;  /* shear distance */
    while(ixx<5) {
      if((sdist>=shrdst[ixx])&&(sdist<shrdst[ixx+1])) {
        spart=(sdist-shrdst[ixx])/(shrdst[ixx+1]-shrdst[ixx]);
        xaddtno=(spart*(shrcat[ixx+1]-shrcat[ixx]));
        ddvor=shrcat[ixx]+xaddtno;
        ixx=5;
      } else {
        ixx++;
      }
    }
    diffchkcat=0;    /* added for test - shear cases */
  } else {
    /* EYE or NO EYE */
    if(odtcurrent_v72->IR.eyescene<=2) {
      /* EYE */
      xaddtno=(xpart*(eno[idomain_v72][cloudcat+1]-eno[idomain_v72][cloudcat]));
      /* cloud category must be white (-70C) or below for full adjustment;
         value will be merged in starting at black (-64C) /
      if(cloudcat<5) {         / gray shades /
        xparteye=0.00;
      } else if(cloudcat==5) { / black /
        xparteye=xpart;
      } else {                 / white and colder /
        xparteye=1.00;
      } */
      eyeadj=eyeadjfacEYE[idomain_v72]*(eyetemp-cloudtemp); 
      /* symadj=-0.02*(odtcurrent_v72->IR.cloudsymave);  */
      symadj=symadjfacEYE[idomain_v72]*(odtcurrent_v72->IR.cloudsymave); 
      /* printf("EYE : cloudsymave=%f  symadj=%f\n",odtcurrent_v72->IR.cloudsymave,symadj); */
      ddvor=eno[idomain_v72][cloudcat]+xaddtno+eyeadj+symadj;
      /* printf("EYE : xaddtno=%f  eyeadj=%f  symadj=%f   ddvor=%f\n",xaddtno,eyeadj,symadj,ddvor);  */
      ddvor=A_MIN(ddvor,9.0);
      /* printf("ddvor=%f\n",ddvor); */
      if(odtcurrent_v72->IR.eyescene==2)  ddvor=A_MIN(ddvor-0.5,6.5); /* LARGE EYE adjustment */
      /* if(odtcurrent_v72->IR.eyescene==3)  ddvor=A_MIN(ddvor-0.5,6.0);     / LARGE RAGGED EYE adjustment */
      diffchkcat=1;   /* added for test - eye cases */
      /* printf("ddvor=%f\n",ddvor); */
    } else {
      /* NO EYE */
      /* CDO */
      xaddtno=(xpart*(cdo[idomain_v72][cloudcat+1]-cdo[idomain_v72][cloudcat]));
      /* dgraysizeadj=0.002*odtcurrent_v72->IR.eyecdosize; */
      dgraysizeadj=dgraysizefacCLD[idomain_v72]*odtcurrent_v72->IR.eyecdosize;
      /* printf("CDO : dgraysize=%f  symadj=%f\n",odtcurrent_v72->IR.eyecdosize,dgraysizeadj); */
      /* symadj=-0.03*(odtcurrent_v72->IR.cloudsymave); */
      symadj=symadjfacCLD[idomain_v72]*(odtcurrent_v72->IR.cloudsymave); 
      /* printf("CDO : cloudsymave=%f  symadj=%f\n",odtcurrent_v72->IR.cloudsymave,symadj); */
      ddvor=cdo[idomain_v72][cloudcat]+xaddtno+dgraysizeadj+symadj;
      ddvor=ddvor-0.1;     /* bias adjustment */
      /* printf("CDO : xaddtno=%f dgraysizeadj=%f  symadj=%f   ddvor=%f\n",xaddtno,dgraysizeadj,symadj,ddvor); */
      ciadj=0.0;
      if(odtcurrent_v72->IR.cloudscene==0) { /* CDO */
        if(lastci>=4.5) ciadj=A_MAX(0.0,A_MIN(1.0,lastci-4.5));
        if(lastci<=3.0) ciadj=A_MIN(0.0,A_MAX(-1.0,lastci-3.0));
        /* printf("CDO : lastci=%f   xaddtno=%f\n",lastci,ciadj); */
        ddvor=ddvor+ciadj;
      }
      if(odtcurrent_v72->IR.cloudscene==1) { /* EMBEDDED CENTER */
        ciadj=A_MAX(0.0,A_MIN(1.5,lastci-4.0));
        /* printf("EMBC : lastci=%f   xaddtno=%f\n",lastci,ciadj); */
        ddvor=ddvor+ciadj;   /* changed from 0.5 */
      }
      if(odtcurrent_v72->IR.cloudscene==2) { /* IRREGULAR CDO (PT=3.5) */
        ddvor=ddvor+0.3;     /* additional IrrCDO bias adjustment */
        ddvor=A_MIN(3.5,A_MAX(2.5,ddvor)); 
      }
      diffchkcat=2;   /* added for test - non-eye/shear cases */
    }
  }
  
  dvorchart=((float)(int)(ddvor*10.0))/10.0;
  odtcurrent_v72->IR.TrawO=dvorchart;

  /* perform Dvorak EIR Rule 8 Constrants on Raw T# value
     All cases     : delT of 0.5 over  1 hour  : rule8 = 9 "velden rule" (actually 86.4 minutes... 0.06 of a day)
                     delT of 0.1 over  1 hour  : rule8 = 8 "additional velden rule", only over first 6 hours
     Raw T# <  4.0 : delT of 1.0 over  6 hours : rule8 = 2 / we removed 0.5/6hr for weakening cases /
                     No threshold exceeded     : rule8 = 0
     Raw T# >= 4.0 : delT of 1.0 over  6 hours : rule8 = 2  (actually 86.4 minutes... 0.06 of a day)
                     delT of 1.5 over 12 hours : rule8 = 3
                     delT of 2.0 over 18 hours : rule8 = 4
                     delT of 2.5 over 24 hours : rule8 = 5
                     No threshold exceeded     : rule8 = 0
  */
  odthistory=odthistoryfirst_v72;
  t1val=dvorchart;
  t6val=dvorchart;
  t12val=dvorchart;
  t18val=dvorchart;
  t24val=dvorchart;
  if(odthistory!=0) {
    ttime1=curtime-0.09;   /* 0.0416 is one hour... round to 0.06 to make sure I catch the hour previous report */
    ttime6=curtime-0.26;
    ttime12=curtime-0.51;
    ttime18=curtime-0.76;
    ttime24=curtime-1.01;
    firsttime=aodtv72_calctime(odthistory->IR.date,odthistory->IR.time);
    if(firsttime>=ttime6) first6hrs=TRUE;
    adjustshear=FALSE;
    while(odthistory!=0) {
      xtime=aodtv72_calctime(odthistory->IR.date,odthistory->IR.time);
      if(xtime>curtime) break;
      oceancheck=TRUE;
      if(((oland_v72)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) { 
        oceancheck=FALSE;
        if (firstland) {
          firstlandtime=xtime;
          firstland=FALSE;
        }
        if((xtime-firstlandtime)>=0.25) adjustshear=TRUE;
      } else {
        firstland=TRUE;
      }
      if((xtime>=ttime24)&&(xtime<curtime)&&(!t24found)&&(oceancheck)) {
        t24found=TRUE;
        t24val=odthistory->IR.Tfinal;
      }
      if((xtime>=ttime18)&&(xtime<curtime)&&(!t18found)&&(oceancheck)) {
        t18found=TRUE;
        t18val=odthistory->IR.Tfinal;
      }
      if((xtime>=ttime12)&&(xtime<curtime)&&(!t12found)&&(oceancheck)) {
        t12found=TRUE;
        t12val=odthistory->IR.Tfinal;
      }
      if((xtime>=ttime6)&&(xtime<curtime)&&(!t6found)&&(oceancheck)) {
        t6found=TRUE;
        t6val=odthistory->IR.Tfinal;
        t1valrawx=odthistory->IR.Traw;
        t1valrawxtime=xtime;
      }
      if((xtime>=ttime1)&&(xtime<curtime)&&(!t1found)&&(oceancheck)) {
        t1found=TRUE;
        t1val=odthistory->IR.Tfinal;
        t1valraw=odthistory->IR.Traw;
      }
      if((xtime<curtime)&&(oceancheck)) {
        lasttno=odthistory->IR.Tfinal;
        lastraw=odthistory->IR.Traw;
        lastr9=odthistory->IR.rule9;
        if((adjustshear)&&(odthistory->IR.cloudscene!=4)) {
          /* turn off shear scene adjustment */
          adjustshear=FALSE;
        }
      } else {
        if((xtime==curtime)&&(oceancheck)) {
          if((adjustshear)&&(odtcurrent_v72->IR.cloudscene!=4)) {
            /* turn off shear scene adjustment */
            adjustshear=FALSE;
          }
        }
      }
      odthistory=odthistory->nextrec;
    }
    if(adjustshear) {
      /* if storm was over land for >= 6 hours and current scene is a shear
         scene type, then add 0.5 to Raw T# value */
      dvorchart=dvorchart+0.5;
      odtcurrent_v72->IR.TrawO=dvorchart;
    }
    /* we are using the experimental "velden rule" which will limit the growth of the 
     * Raw T# to 0.5/hour to try and dampen out scene changes and obvious incorrect
     * jumps in intensity due to incorrect positioning (most likely in the 
     * automode analysis */
    odtcurrent_v72->IR.rule8=(diffchkcat*10)+0;
    if(lasttno<4.0) {
      /* Raw T# < 4.0 */
      if(first6hrs) {
        if(t1found) {
          delt1=A_ABS(t1valraw-dvorchart);
          if(delt1>diffchk[diffchkcat][8]) {
            dvorchart=A_MAX(t1valraw-diffchk[diffchkcat][8],A_MIN(t1valraw+diffchk[diffchkcat][8],dvorchart));
            odtcurrent_v72->IR.rule8=(diffchkcat*10)+8;
          }
        } else {
          /* no value available within past hour... must determine approx value */
          delt1=0.1*(A_ABS(curtime-t1valrawxtime)/.0416);
          txvalmin=t1valrawx-delt1;
          txvalmax=t1valrawx+delt1;
          if((dvorchart>txvalmax)||(dvorchart<txvalmin)) {
            dvorchart=A_MAX(txvalmin,A_MIN(txvalmax,dvorchart));
            odtcurrent_v72->IR.rule8=(diffchkcat*10)+8;
          }
        }
      } else {
        delt1=A_ABS(t1val-dvorchart);
        if((delt1>diffchk[diffchkcat][9])&&(t1found)) {
          dvorchart=A_MAX(t1val-diffchk[diffchkcat][9],A_MIN(t1val+diffchk[diffchkcat][9],dvorchart));
          odtcurrent_v72->IR.rule8=(diffchkcat*10)+9;
        }
        delt6=A_ABS(t6val-dvorchart);
        if(lastr9<2) {
          if((delt6>diffchk[diffchkcat][2])&&(t6found)) {
            dvorchart=A_MAX(t6val-diffchk[diffchkcat][2],A_MIN(t6val+diffchk[diffchkcat][2],dvorchart));
            odtcurrent_v72->IR.rule8=(diffchkcat*10)+2;
          }
        } else {
          if((delt6>diffchk[diffchkcat][1])&&(t6found)) {
            dvorchart=A_MAX(t6val-diffchk[diffchkcat][1],A_MIN(t6val+diffchk[diffchkcat][1],dvorchart));
            odtcurrent_v72->IR.rule8=(diffchkcat*10)+1;
          }
        }
      }
    } else {
      /* Raw T# >= 4.0 */
      delt1=A_ABS(t1val-dvorchart);
      if((delt1>diffchk[diffchkcat][9])&&(t1found)) {
        dvorchart=A_MAX(t1val-diffchk[diffchkcat][9],A_MIN(t1val+diffchk[diffchkcat][9],dvorchart));
        odtcurrent_v72->IR.rule8=(diffchkcat*10)+9;
      }
      delt6=A_ABS(t6val-dvorchart);
      delt12=A_ABS(t12val-dvorchart);
      delt18=A_ABS(t18val-dvorchart);
      delt24=A_ABS(t24val-dvorchart);
      if((delt6>diffchk[diffchkcat][2])&&(t6found)) {
        dvorchart=A_MAX(t6val-diffchk[diffchkcat][2],A_MIN(t6val+diffchk[diffchkcat][2],dvorchart));
        odtcurrent_v72->IR.rule8=(diffchkcat*10)+2;
      } else if((delt12>diffchk[diffchkcat][3])&&(t12found)) {
        dvorchart=A_MAX(t12val-diffchk[diffchkcat][3],A_MIN(t12val+diffchk[diffchkcat][3],dvorchart));
        odtcurrent_v72->IR.rule8=(diffchkcat*10)+3;
      } else if((delt18>diffchk[diffchkcat][4])&&(t18found)) {
        dvorchart=A_MAX(t18val-diffchk[diffchkcat][4],A_MIN(t18val+diffchk[diffchkcat][4],dvorchart));
        odtcurrent_v72->IR.rule8=(diffchkcat*10)+4;
      } else if((delt24>diffchk[diffchkcat][5])&&(t24found)) {
        dvorchart=A_MAX(t24val-diffchk[diffchkcat][5],A_MIN(t24val+diffchk[diffchkcat][5],dvorchart));
        odtcurrent_v72->IR.rule8=(diffchkcat*10)+5;
      } else {
        odtcurrent_v72->IR.rule8=odtcurrent_v72->IR.rule8;      
      }
    }
  }

  return dvorchart;

}

float aodtv72_Tnofinal(int itype)
/* Compute time averaged T-Number value using previous and current
   intensity estimates.  Average using a time-weighted averaging
   scheme.
    Inputs  : itype : time average duration flag : 0=6 hour;1=3 hour
              global structure odtcurrent_v72 containing current analysis
    Outputs : return value is Final T#
*/
{
  double curtime,xtime,tlimit,diff;
  double xint=1.0/24.0,baseval=6.0;
  float sumtop=0.0,sumbot=0.0;
  float dvorweight,weight,value;
  logical found=FALSE,oceancheck;
  struct odtdata *odthistory;

  odthistory=odthistoryfirst_v72;

  if(itype==1) baseval=3.0;   /* for NHC 3-hour time average value */
  if(itype==2) baseval=12.0;  /* for TIE Model time average value */

  curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);

  /* compute average with current value with any values
     from previous 6 hours */
  tlimit=curtime-(baseval/24.0);

  /* compute weighted time averaged value */
  while(odthistory!=0) {
    xtime=aodtv72_calctime(odthistory->IR.date,odthistory->IR.time);
    if((xtime>=tlimit)&&(xtime<curtime)) {
      oceancheck=TRUE;
      if(itype<=1) {
        value=odthistory->IR.Traw;
      } else {
        /*value=odthistory->IR.TIEraw;*/
        if(value<0.0) value=0.0;
      }
      if(((oland_v72)&&(odthistory->IR.land==1))||(value<1.0)) oceancheck=FALSE;
      if(oceancheck) {
        diff=curtime-xtime;
        if(itype==0) {
          weight=(float)(baseval-(diff/xint)); /* time weighted average */
        } else {
          weight=baseval;                      /* straight average */
        }
        sumtop=sumtop+(weight*value);
        sumbot=sumbot+weight;
        found=TRUE;
      }
    } else {
      if(found) break;
    }
    odthistory=odthistory->nextrec;
  }

  /* compute time-averaged T# value.
     if no previous records found, return Raw T# */
  if(itype<=1) {
    value=odtcurrent_v72->IR.Traw;
  } else {
    /*value=odtcurrent_v72->IR.TIEraw;*/
    if(value<=1.0) found=FALSE;
  }
  if(found) {
    sumtop=sumtop+(baseval*value);
    sumbot=sumbot+baseval;
    /* remove any value remainder past tenths */
    dvorweight=(float)((int)((sumtop/sumbot)*10.0))/10.0;
  } else {
    dvorweight=value;
  }

  return dvorweight;
}

float aodtv72_CIno(int *curstrength)
/* Compute final CI-Number applying various Dvorak Rules, such
   as the now famous Rule 9
    Inputs  : odtcurrent_v72  - structure containing current analysis
    Outputs : curstrength - current strengthening/weakening flag
              return value is Current Intensity (CI) # 
*/
{
  float  intensity;
  int    strength,lstrength,lrapidmin,rapdiss,oceanbasin;
  float  sigslope24=-1.0,sigslope,slopeval,rapidslopeval;
  float  lasttnomax,lasttnomax6,lasttno,lastCI,lastrapid,cival,swval1,swval2,ctno,ctnor9;
  float  tnomin=9.0,tnomax=0.0,r9add=1.0;
  double curtime,timem3,timem6,timem12,timem24,xtime,lasttime;
  struct odtdata *odthistory;
  logical allland=TRUE,tdts24=FALSE,oceancheck,found3hr=FALSE;

  odthistory=odthistoryfirst_v72;

  curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);

  if(odthistoryfirst_v72==0) {
    intensity=odtcurrent_v72->IR.Traw;
    /* no records in history file */
    *curstrength=0;
    /* this will trip the RULE 9 FLAG for an initial classification of >=6.0 */
    if(osstr_v72>=6.0) *curstrength=2;
    /* Apply Latitude Bias Adjustment to CI value */
    odtcurrent_v72->IR.CIadjp=aodtv72_latbias(intensity,odtcurrent_v72->IR.latitude,odtcurrent_v72->IR.longitude);
    return intensity;
  }

  /* determine various time threshold values */
  timem3=curtime-0.125;
  timem6=curtime-0.25;
  timem12=curtime-0.5;
  timem24=curtime-1.0;

  /* find record just prior to current record */
  odthistory=odthistoryfirst_v72;
  lasttnomax=0.0;
  lasttnomax6=0.0;
  lastCI=0.0;
  lastrapid=0;
  lrapidmin=99;
  while(odthistory!=0) {
    xtime=aodtv72_calctime(odthistory->IR.date,odthistory->IR.time);
    if(xtime>=curtime) break;
    oceancheck=TRUE;
    if(((oland_v72)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) oceancheck=FALSE;
    if(oceancheck) {
      if(odtcurrent_v72->IR.eyescene<=2) {
        lasttno=odthistory->IR.Tfinal3;
      } else {
        lasttno=odthistory->IR.Tfinal;
      }
      lastCI=odthistory->IR.CI;
      lstrength=odthistory->IR.rule9;
      lasttime=xtime;
      lastrapid=odthistory->IR.rapiddiss;
      if(xtime>=timem12) {   /* check Rule 9 -- must be over land for 12 hours to turn off */
        allland=FALSE;
        /* find largest finalT# in last 12 hours prior to current record */
        if(lasttno>lasttnomax) lasttnomax=lasttno;
      }
      if(xtime>=timem3) {   /* eye/non-eye three hour check */
        found3hr=TRUE;
      }
      if(xtime>=timem6) {   /* eastpac rapid dissapation check */
        if(lastrapid<lrapidmin) lrapidmin=lastrapid;
        if(lasttno>lasttnomax6) lasttnomax6=lasttno;
      }
      if(xtime>=timem24) { 
        /* find min and max finalT# in last 24 hours prior to current record */
        if(lasttno<tnomin) tnomin=lasttno;
        if(lasttno>tnomax) tnomax=lasttno;
        /* if storm was TD/TS at any time during last 24 hours, 
           don't allow trip of sig. strengthening flag */
        if(lasttno<=4.0) tdts24=TRUE;
      }
    }
    odthistory=odthistory->nextrec;
  }

  /* for new test */
  if(odtcurrent_v72->IR.eyescene<=2&&found3hr) {
    intensity=odtcurrent_v72->IR.Tfinal3;
  } else {
    intensity=odtcurrent_v72->IR.Tfinal;
  }

  if(odthistory==odthistoryfirst_v72) {
    /* current record is before first record in history file */
    *curstrength=0;
    odtcurrent_v72->IR.CIadjp=aodtv72_latbias(intensity,odtcurrent_v72->IR.latitude,odtcurrent_v72->IR.longitude);
    return intensity;
  }

  /* 24-hour slope determination */
  slopeval=aodtv72_slopecal(24.0,1);
  sigslope=sigslope24;

  strength=lstrength;

/*
  oceanbasin=aodtv72_oceanbasin(odtcurrent_v72->IR.latitude,odtcurrent_v72->IR.longitude);
  if(oceanbasin==2) {   / east/central pacific only /
*/
    /* rapid dissipation determination */
    /* rapidslopeval=aodtv72_slopecal(12.0,2); */
    rapidslopeval=aodtv72_slopecal(6.0,2);
    if(lastrapid<=1) {
      rapdiss=0;
      if(rapidslopeval>=2.0) rapdiss=1;  /* 2.0/24 hours */
      if((lrapidmin==1)&&(rapdiss==1)) {
        r9add=0.5;
        rapdiss=2;
      }
    } else {
      r9add=0.5;
      rapdiss=2;
      if(rapidslopeval<1.5) rapdiss=3;  /* 1.5/24 hours */
      if((lrapidmin==3)&&(rapdiss==3)) {
        r9add=1.0;
        rapdiss=0;
      }
    }
/*
  } else { 
    r9add=1.0;
    rapdiss=0;
    rapidslopeval=0.0;
  }
*/
  ctno=intensity;
  ctnor9=intensity+r9add;
  cival=A_MIN(ctnor9,A_MAX(lasttnomax6,ctno));  /* changed from lasttnomax */

  /* determine current strengthening/weakening values */
  swval1=(ctno-lastCI);
  swval2=(cival-lastCI);

  /* strength flags : 0 - strengthening, but no significant strengthening cycle noted;
                      1 - applying Max's 12 hour max Tno. rule during insignificant weakening
  */  

  /* will utilize Max's Rule all of the time */
  if(cival>ctno) strength=1;
  if((lstrength==1)&&(lastCI<=ctno)) strength=0;  /* changed from < to <= */

  /* check for land interaction
     if undergoing interactation with land "turn off" Rule 9 
     application and return CI value to Tno value for >= 12 hours. */
  if(allland) {
    /* if land flag is TRUE,
       turn off Rule 9 and let CI value be equal to the
       current Final T# value, and return
       current intensity flag to "insignificant value" until
       another significant strengtheing cycle occurs */
      strength=0;
      /*cival=(lastCI+intensity)/2.0;*/
      cival=intensity; 
      rapdiss=0;
  }

  /* Apply Latitude Bias Adjustment to CI value */
  odtcurrent_v72->IR.CIadjp=aodtv72_latbias(cival,odtcurrent_v72->IR.latitude,odtcurrent_v72->IR.longitude);
  odtcurrent_v72->IR.rapiddiss=rapdiss;

  *curstrength=strength;
  return cival;
} 

float aodtv72_latbias(float initval,float latitude,float longitude)
/* Apply Latitude Bias Adjustment to CI value
    Inputs  : initval  - initial CI value 
              latitude - current latitude of storm
    Outputs : adjusted MSLP value as return value
*/
{
  float initvalp;

  float initvalpx;
  float value;      /* lat bias adjustement amount (0.00-1.00) */
  int   sceneflag;  /* contains lat bias adjustment flag
                       0=no adjustment
                       1=intermediate adjustment (6 hours)
                       2=full adjustment 
*/

  sceneflag=aodtv72_scenesearch(0,&value);   /* 0 means search for EIR based parameters... cdo, etc */
  /* printf("sceneflag=%d  value=%f\n",sceneflag,value); */
  odtcurrent_v72->IR.LBflag=sceneflag; 
  /* initvalp=aodtv72_getpwval(0,initval); TLO */
  initvalp=0.0;
  if(sceneflag>=2) {
    /* EIR scene */
    if((latitude>=0.0)&&((longitude>=-100.0)&&(longitude<=-40.0))) {
      /* do not make adjustment in N Indian Ocean */
      return initvalp;
    } 
    /* apply bias adjustment to pressure */
    /* initvalp=-1.0*value*(-20.60822+(0.88463*A_ABS(latitude)));  */
    initvalp=value*(7.325-(0.302*A_ABS(latitude))); 
  } 

  return initvalp;
}

int aodtv72_scenesearch(int type,float *pvalue)
{
  int     curflag=1,flag,eirflag;
  float   eirvalue,civalue,ciadjvalue,latitude;
  double  curtime,xtime,curtimem6,mergetimefirst,mergetimelast,firsttime=-9999.0;
  struct  odtdata *odthistory;
  logical oceancheck,eirfound=TRUE,foundmergefirsttime=FALSE;
  
  /* if(((odthistoryfirst_v72==0)&&(ostartstr_v72==TRUE))&&(hfile_v72!=(char *)NULL)) { */
  if(((odthistoryfirst_v72==0)&&(ostartstr_v72==TRUE))&&((strlen(hfile_v72)!=0))) {
    flag=0;
    *pvalue=-999.9;
    return flag;
  }
  if(strlen(hfile_v72)==0) {
    flag=2;
    *pvalue=1.0;
    return flag;
  }

  eirflag=0;
  eirvalue=0.0;
  if(type==0) {
    /* check current value and set return flag */
    if((odtcurrent_v72->IR.cloudscene>=2)&&(odtcurrent_v72->IR.cloudscene<6)) curflag=0;

    /* search for EIR type scene for lat bias adjustment */
    curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);
    curtimem6=curtime-0.26;
    mergetimefirst=curtime;
    mergetimelast=curtime;
    odthistory=odthistoryfirst_v72;
    while(odthistory!=0) {
      xtime=aodtv72_calctime(odthistory->IR.date,odthistory->IR.time);
      oceancheck=TRUE;
      if(((oland_v72)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) oceancheck=FALSE;
      if((xtime<curtime)&&(oceancheck)) {
        /* store last values in case I need them for six hour gap */
        civalue=odthistory->IR.CI;
        ciadjvalue=odthistory->IR.CIadjp;
        latitude=odthistory->IR.latitude;
        eirflag=odthistory->IR.LBflag;
        if((xtime>=curtimem6)&&(eirfound)) {
          if(firsttime<0.0) firsttime=xtime;
          if(eirflag==0) eirfound=FALSE;
          if(eirflag==2) {
            if(!foundmergefirsttime) {
              mergetimefirst=xtime;
              foundmergefirsttime=TRUE;
            }
          }
        }
      }
      odthistory=odthistory->nextrec;
    }

    if(firsttime<0.0) {
      /* there is a six hour gap in the data for some reason... 
         I will use the last value available */
      flag=eirflag;
      if(eirflag>=1) flag=2;
      *pvalue=1.0;
    } else {
      if(eirfound) {
        /* entering or in valid lat bias adjustment period */
        flag=2;
        *pvalue=(curtime-mergetimefirst)/(curtime-firsttime);   /* return value from 0 to 1 */
      } else {
        *pvalue=-999.9;
        flag=curflag;
      }
    }
  } else {
    /* printf("not valid search type\n"); */
  }

  return flag;
}

/*
float aodtv72_TIEmodel(void)
/ determine TIE model intensity estimate based upon linear 
   regression model using various AODT parameters and SST 
   value at cursor location.
   Inputs  : values in odtcurrent_v72 structure
   Outputs : TIE model predicted value is return value
 /
{
  float ert,crt,cwcrt,xlat,sst,xmpi,tiemodel;
  float a=0.1813;
 
  / get current sst value, if available /
  sst=odtcurrent_v72->IR.sst;
  if(sst<-10.0) return 0.0;

  / get additional current predictor values /
  ert=odtcurrent_v72->IR.eyet;
  crt=odtcurrent_v72->IR.cloudt2;
  cwcrt=odtcurrent_v72->IR.cwcloudt;
  xlat=odtcurrent_v72->IR.latitude;

  / determine SST-based predictor /
  xmpi=A_EXP(a*sst);

  / calculate TIE model predicted intensity value /
  tiemodel=1003.1324076 - (0.3980090*ert)   + (0.6746796*crt) 
                        + (0.4220070*cwcrt) - (0.5440850*xlat) 
                        + (0.1780407*xmpi);

  return tiemodel;
}
*/

void maxtnoval(int stime,int *iweak,float *maxtno)
/* Determine maximum final T# value over period defined by 
   stime.  Return value and strengthening/weakening flag 
    Inputs  : stime  - number of hours to calculate slope over
              global structureodtcurrent_v72 structure containing current analysis
    Outputs : iweak  - weakening intensity flag (over last 12 hours)
                       1=weakening for less than 12 hours, 0=otherwise
              maxtno - maximum T# during last 12 hours if storm has
                       been or is currently in a significant
                       strengthening cycle 
*/
{
  int weak=0;
  float tmax=0.0;
  double curtime,xtime,slopetime=stime/24.0;
  logical found=FALSE,oceancheck;
  struct odtdata *odthistory;

  odthistory=odthistoryfirst_v72;
  
  curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);

  /* Determine maximum T-number.  If weakening has been occuring for
     less than 12 hours, return iweak value of 1, else return 0 */
  while(odthistory!=0) {
    xtime=aodtv72_calctime(odthistory->IR.date,odthistory->IR.time);
    if(xtime<curtime) {
      oceancheck=TRUE;
      if(((oland_v72)&&(odthistory->IR.land==1))||(odthistory->IR.Traw<1.0)) oceancheck=FALSE;
      if(oceancheck) {
        if(odthistory->IR.Tfinal>tmax) tmax=odthistory->IR.Tfinal;
        if(xtime>=(curtime-slopetime)) weak=1;
      }
      found=TRUE;
    } else {
      if(found) break;
    }
    odthistory=odthistory->nextrec;
  }  

  *iweak=weak;
  *maxtno=tmax;

}

/*
int aodtv72_tieflag(void)
{
  int    flagval=0;
  float  tietno;
  double curtime,xtime;
  logical found=FALSE;
  struct odtdata *odthistory;

  /tietno=aodtv72_ptotno(odtcurrent_v72->IR.TIEavg);/
  if(A_ABS(tietno-odtcurrent_v72->IR.Tfinal)<=0.5) {
    / if difference is <= 0.5, then return "found" TIE flag /
    flagval=1;
  } else {
    / check for previous "found" TIE flag /
    odthistory=odthistoryfirst_v72;
    curtime=aodtv72_calctime(odtcurrent_v72->IR.date,odtcurrent_v72->IR.time);
    while((odthistory!=0)&&(!found)) {
      xtime=aodtv72_calctime(odthistory->IR.date,odthistory->IR.time);
      if(xtime<curtime) {
        if(odthistory->IR.TIEflag==1) {
          flagval=1;
          found=TRUE;
        }
      }
      odthistory=odthistory->nextrec;
    }
  }
  
  return flagval;
}
*/


#define lv 74


float temp[lv]={195.64000,196.22000,197.54000,198.37000,199.30000,201.80000,
                204.26000,204.59000,206.69000,208.75000,209.11000,211.62000,
                214.08000,216.49000,218.85000,220.96000,221.17000,223.50000,
                225.78000,228.02000,230.22000,230.67000,232.51000,234.80000,
                237.04000,239.25000,241.44000,243.60000,245.71000,246.59000,
                247.83000,249.94000,252.01000,253.14000,254.07000,256.12000,
                256.77000,258.15000,260.16000,261.82000,262.14000,264.10000,
                264.45000,266.08000,268.04000,269.96000,271.35000,271.87000,
                273.78000,275.66000,275.88000,277.54000,279.40000,280.12000,
                281.23000,282.53000,282.88000,284.12000,285.33000,286.53000,
                286.64000,287.91000,289.29000,290.51000,290.68000,292.23000,
                293.76000,294.62000,295.28000,296.32000,296.79000,298.28000,
                299.02000,299.74000};

float  alt[lv]={16.180,15.991,15.562,15.293,15.142,14.732,
                 4.331,14.277,13.939,13.609,13.555,13.179,
                 2.812,12.441,12.098,11.784,11.753,11.414,
                 1.081,10.755,10.430,10.363,10.109, 9.792,
                 9.476, 9.164, 8.855, 8.548, 8.244, 8.117,
                 7.942, 7.644, 7.348, 7.186, 7.054, 6.764,
                 6.671, 6.475, 6.188, 5.951, 5.905, 5.624,
                 5.574, 5.345, 5.069, 4.795, 4.596, 4.523,
                 4.254, 3.986, 3.955, 3.721, 3.458, 3.355,
                 3.197, 3.013, 2.938, 2.682, 2.427, 2.175,
                 2.153, 1.925, 1.677, 1.457, 1.431, 1.186,
                 0.944, 0.807, 0.704, 0.540, 0.465, 0.229,
                 0.111, 0.000};

float pinhole(float lat,float lon,float eyetemp)
{
  int    ixx;
  float  a,b,x,tout;
  float  esiz=4.0;
  float  angle,xang,xval,height,angck;
  double z=0.01745329;
  double dlat,dlon,sublat,sublon,xangx;
  logical found=FALSE;

  angck=A_TAN(esiz/16.0)/z;
  
  dlat=(double)lat;
  dlon=(double)lon;
  sublat=0.0;
  sublon=75.0;
  angle=aodtv72_xlza(sublat,sublon,dlat,dlon);

  if(angle<=angck) {
    return 10.0;
  }

  xang=90.0-angle;
  
  xangx=(double)xang*z;
  xval=tan(xangx)*esiz;
  height=15.0-xval;

  ixx=lv;
  while((ixx>0)&&(!found)) {
    if(height<=alt[ixx-1]) found=TRUE;
     ixx--;
  }
  if(found) {
    a=height-alt[ixx];
    b=alt[ixx+1]-alt[ixx];
    x=temp[ixx+1]-temp[ixx];
    tout=temp[ixx]+((a/b)*x);
  }
  tout=tout-273.16;
  tout=A_MIN(10.0,10.0+(eyetemp-tout));
  return tout;
}


float aodtv72_xlza(double rrlat,double rrlon,double plat,double plon)
/* approximate local zenith angle (angle from vertical to the satellite)
   Inputs  : rlat - latitude of sub-satellite point
             rlon - longitude of sub-satellite point
             plat - latitude of target position on earth
             plon - longitude of target position on earth
   Outputs : ang  - local zenith angle
*/
{
  double z=0.017453292,r=6371.0,h=35792.0;
  double xzlat,xzlon,rlat,rlon;
  double crlat,crlon,srlat,srlon;
  double cplat,cplon,splat,splon;
  double xx,yy,zz,dist,alpha,beta,ang;
       
  xzlat=plat;
  xzlon=plon;
  rlat=rrlat*z;
  rlon=rrlon*z;
  plat=plat*z;
  plon=plon*z;
       
  crlat=cos(rlat);
  crlon=cos(rlon);
  srlat=sin(rlat);
  srlon=sin(rlon);
  cplat=cos(plat);
  cplon=cos(plon);
  splat=sin(plat);
  splon=sin(plon);
       
  xx=(cplat*cplon)-(crlat*crlon);
  yy=(cplat*splon)-(crlat*srlon);
  zz=splat-srlat;
  dist=sqrt((xx*xx)+(yy*yy)+(zz*zz));
       
  xx=r*sin(dist);
  yy=r*cos(dist);
  zz=h+r-yy;
  alpha=(atan(zz/xx))/z;
  dist=dist/z;
  beta=90.0-dist;
  ang=180.0-beta-alpha;
       
  plat=xzlat;
  plon=xzlon;
  return(ang);
}
