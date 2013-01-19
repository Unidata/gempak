#include "inc/odt.h"

extern int checkrapid( struct odtdata * );
extern void xprintf(char *);

void odtintensity( struct odtdata * );
float Tnoraw( struct odtdata * ); 
float Tnofinal( struct odtdata * ); 
float CIno( struct odtdata *,int * ); 
void maxtnoval( struct odtdata *,int,int *,float * );
float slopecal( struct odtdata *,double,int );
double calctime( int, int );

/* extern global variables */
extern int kstart,keyer,kres;
extern int idomain;
extern logical spotanal,ofirst48,o48hr;
extern char iout[200];

extern struct odtdata *odthistoryfirst;

void odtintensity(struct odtdata *odtcurrent)
/* Compute intensity values CI, Final T#, and Raw T#.
    Inputs  : odtcurrent - structure containing current analysis
    Outputs : none
*/
{
  int strength;

  /* (re)obtain the rapid flag for the current record */
  odtcurrent->IR.rapid=checkrapid(odtcurrent);
  odtcurrent->IR.Traw = Tnoraw(odtcurrent);

  /* check for spot analysis or full analysis using history file */
  if(!spotanal) {
    /* perform full analysis (Tfinal and CI) */
    odtcurrent->IR.Tfinal = Tnofinal(odtcurrent);
    odtcurrent->IR.CI = CIno(odtcurrent,&strength);
    odtcurrent->IR.rule9=strength;
  } else {
    /* perform spot analysis (only Traw) */
    odtcurrent->IR.Tfinal = odtcurrent->IR.Traw;
    odtcurrent->IR.CI = odtcurrent->IR.Traw;
    odtcurrent->IR.rule9 = 1;
    odtcurrent->IR.rapid = 0;
  }

}

float Tnoraw(struct odtdata *odtcurrent)
/* Compute initial Raw T-Number value using original Dvorak rules
    Inputs  : odtcurrent - structure containing current analysis
    Outputs : return value is Raw T#

                            ODT SCENE/TEMPERATURE TABLE
    BD   |<----- WMG   OW    DG    MG    LG     B     W    CMG   CDG ---------->| 
    TEMP |30.0   0.0 -15.0 -35.5 -47.0 -58.0 -66.0 -72.0 -77.5 -80.0 -84.0-100.0|
--------------------------------------------------------------------------------|
Atl EYE  | 4.0   4.0   4.0   4.5   4.5   5.0   5.5   6.0   6.5   7.0   7.0   7.0|
    EMBC | 3.5   3.5   3.5   4.0   4.5   4.5   5.0   5.0   5.5   6.0   6.0   6.0|
    CDO  | 3.5   3.5   3.5   4.0   4.5   4.5   4.5   4.5   5.0   5.5   5.5   5.5|
--------------------------------------------------------------------------------|
Pac EYE  | 4.0   4.0   4.0   4.0   4.5   4.5   5.0   5.5   6.0   6.5   7.0   7.0|
    EMBC | 3.5   3.5   3.5   4.0   4.0   4.5   4.5   5.0   5.0   5.5   6.0   6.0|
    CDO  | 3.5   3.5   3.5   4.0   4.0   4.5   4.5   4.5   4.5   5.0   5.0   5.5|
--------------------------------------------------------------------------------|
Cat diff |  0     1     2     3     4     5     6     7     8     9    10       |
    add  |-0.5   0.0   0.0   0.0   0.25  0.5   0.5   0.5   0.5   0.5   0.5      |
--------------------------------------------------------------------------------|
*/
{
  float ebd[12]={ 30.0F,  0.0F,-15.0F,-35.5F,-47.0F,-58.0F,
                 -66.0F,-72.0F,-77.5F,-80.0F,-84.0F,-100.0F};
  float eno[2][12]={ {4.0F,4.0F,4.0F,4.5F,4.5F,5.0F,5.5F,6.0F,6.5F,7.0F,7.0F,7.0F},
                     {4.0F,4.0F,4.0F,4.0F,4.5F,4.5F,5.0F,5.5F,6.0F,6.5F,7.0F,7.0F} };
  float cno[2][12]={ {3.5F,3.5F,3.5F,4.0F,4.5F,4.5F,5.0F,5.0F,5.5F,6.0F,6.0F,6.0F},
                     {3.5F,3.5F,3.5F,4.0F,4.0F,4.5F,4.5F,5.0F,5.0F,5.5F,6.0F,6.0F} };
  float cdo[2][12]={ {3.5F,3.5F,3.5F,4.0F,4.5F,4.5F,4.5F,4.5F,5.0F,5.5F,5.5F,5.5F},
                     {3.5F,3.5F,3.5F,4.0F,4.0F,4.5F,4.5F,4.5F,4.5F,5.0F,5.0F,5.5F} };
  float iadd[11]={-0.5F,0.0F,0.0F,0.0F,0.25F,0.5F,0.5F,0.5F,0.5F,0.5F,0.5F};

  int ixx,cloudcat,eyecat,diffcat;
  int laststrength=0;
  float xpart,xaddtno,ddvor,dvorchart,cloudtemp;
  double firsttime,curtime,xtime,tdiffx;
  struct odtdata *odthistory,*prevrec;

  odthistory=odthistoryfirst;

  curtime=calctime(odtcurrent->IR.date,odtcurrent->IR.time);

  if(odthistoryfirst!=0) { 
    firsttime=calctime(odthistoryfirst->IR.date,odthistoryfirst->IR.time);
  } else {
    firsttime=curtime;
  }

  /* find record just prior to current record */
  prevrec=odthistory;
  while(odthistory!=0) {
    xtime=calctime(odthistory->IR.date,odthistory->IR.time);
    if(xtime>curtime) break;
    prevrec=odthistory;
    odthistory=odthistory->nextrec;
  }

  if(prevrec!=odthistoryfirst) laststrength=prevrec->IR.rule9;

  cloudtemp=odtcurrent->IR.cloudt;
  /* if storm is rapidly intensifying, use minimum temperature between
     Cloud Top temperature and Mean Cloud temperature */
  if(odtcurrent->IR.rapid>1) cloudtemp=MIN(odtcurrent->IR.cloudt,odtcurrent->IR.meancloudt);

  /* compute cloud category */
  for(ixx=0;ixx<11;ixx++) {
    if((cloudtemp<=ebd[ixx])&&(cloudtemp>ebd[ixx+1])) {
      cloudcat=ixx;
      xpart=(cloudtemp-ebd[cloudcat])/(ebd[cloudcat+1]-ebd[cloudcat]);
    }
  }

  /* compute eye category for eye adjustment */
  for(ixx=0;ixx<11;ixx++) {
    if((odtcurrent->IR.eyet<=ebd[ixx])&&(odtcurrent->IR.eyet>ebd[ixx+1])) {
      eyecat=ixx;
    }
  }
  diffcat=MAX(0,cloudcat-eyecat);
  
  /* adjustments to initial T# */

  /* if scenetype is EYE, apply rule 2C */
  if(odtcurrent->IR.scenetype<=4) {
    xaddtno=(xpart*(eno[idomain][cloudcat+1]-eno[idomain][cloudcat]));
    ddvor=eno[idomain][cloudcat]+xaddtno+iadd[diffcat];
    if((cloudcat>=5)&&(eyecat==0)) {
      ddvor=ddvor+0.25F;
      if(cloudcat>=7) ddvor=ddvor+0.25F;
    }
  }

  /* if scenetype is EMBC, apply rule 2E */
  if(odtcurrent->IR.scenetype==5) {
    if(diffcat==0) diffcat=1;
    xaddtno=(xpart*(cno[idomain][cloudcat+1]-cno[idomain][cloudcat]));
    ddvor=cno[idomain][cloudcat]+xaddtno+iadd[diffcat];
    if((diffcat>=2)&&(cloudcat>=6)) {
      ddvor=ddvor+0.5F;
    }
  }

  /* if scenetype is CDO, apply modified rule 2E */
  if(odtcurrent->IR.scenetype==6) {
    if(diffcat==0) diffcat=1;
    xaddtno=(xpart*(cdo[idomain][cloudcat+1]-cdo[idomain][cloudcat]));
    ddvor=cdo[idomain][cloudcat]+xaddtno+iadd[diffcat];
    if((diffcat>=2)&&(cloudcat>=6)) {
      ddvor=ddvor+0.5F;
    }
  }

  /* adjust value of raw T# if storm is :
     1. within first 48 hours of development AND not
        in or after a "significant strengthening" cycle
     2. CDO, EYE, or EMBEDDED CENTER scene type (changed 11.3.98)
     this rule was added to help with overestimate bias
     observed with developing storms before initial
     strengthening occurs (and still possesses a CDO structure)
     NOTE : THIS RULE WILL NOT BE APPLIED IN THE CASE OF
            A "SPOT CHECK" ON AN INDIVIDUAL IMAGE
            (NOT WITHIN A STORM HISTORY EVALUATION)!!!
            IF cfile=ODTDUMP.ODT, THE PROGRAM WILL ASSUME
            THAT A SPOT CHECK OF A SINGLE IMAGE IS
            BEING PERFORMED, AND THE 0.5 SUBTRACTION WILL
            NOT BE DONE! */
  o48hr=FALSE; 
  if((!spotanal)&&(ofirst48)) {
    tdiffx=curtime-firsttime;
    if((laststrength<2)&&(tdiffx<=2.0)) {
      if(odtcurrent->IR.scenetype<=6) {
        o48hr=TRUE; 
        ddvor=ddvor-(float)(0.5*((2.0-tdiffx)/2.0));
      }
    }
  }

  /* if storm is SHEAR, assign T# of 3.5 (Rule 2B) */
  if(odtcurrent->IR.scenetype==9) ddvor=3.5F;

  dvorchart=((float)(int)(ddvor*10.0F))/10.0F;

  return dvorchart;

}

float Tnofinal(struct odtdata *odtcurrent)
/* Compute time averaged T-Number value using previous and current
   intensity estimates.  Average using a time-weighted averaging
   scheme.
    Inputs  : odtcurrent - structure containing current analysis
    Outputs : return value is Final T#
*/
{
  double curtime,xtime,tlimit,diff;
  double xint=1.0/24.0,baseval=12.0;
  float sumtop=0.0F,sumbot=0.0F;
  float dvorweight,weight;
  logical found=FALSE;
  struct odtdata *odthistory;

  odthistory=odthistoryfirst;

  /* if rapid deepening cycle, use time averaging base of 3 hours */
  if(odtcurrent->IR.rapid>1) baseval=3.0;

  curtime=calctime(odtcurrent->IR.date,odtcurrent->IR.time);

  /* compute average with current value with any values
     from previous 11 hours (2 hours for rapid deepening) */
  tlimit=curtime-((baseval-1.0)/24.0);

  /* compute weighted time averaged value */
  while(odthistory!=0) {
    xtime=calctime(odthistory->IR.date,odthistory->IR.time);
    if((xtime>=tlimit)&&(xtime<curtime)) {
      diff=curtime-xtime;
      weight=(float)(baseval-(diff/xint));
      sumtop=sumtop+(weight*odthistory->IR.Traw);
      sumbot=sumbot+weight;
      found=TRUE;
    } else {
      if(found) break;
    }
    odthistory=odthistory->nextrec;
  }

  /* compute time-averaged T# value.
     if no previous records found, return Raw T# */
  if(found) {
    sumtop=sumtop+(float)(baseval*(double)odtcurrent->IR.Traw);
    sumbot=sumbot+(float)baseval;
    /* remove any value remainder past tenths */
    dvorweight=(float)((int)((sumtop/sumbot)*10.0F))/10.0F;
  } else {
    dvorweight=odtcurrent->IR.Traw;
  }


  return dvorweight;
}

float CIno(struct odtdata *odtcurrent,int *curstrength)
/* Compute final CI-Number applying various Dvorak Rules, such
   as the now famous Rule 9
    Inputs  : odtcurrent  - structure containing current analysis
    Outputs : curstrength - current strengthening/weakening flag
              return value is Current Intensity (CI) # 
*/
{
  int first12flag=0,strength,lstrength,iweak;
  int slopeflag24=0,slopeflag12=0;
  float sigslope24=-1.0F,sigslope12=-2.5F,r9v=1.0F;
  float lasttno12,lasttno,lastCI,slopeval1,slopeval2,maxtno,cival;
  double curtime,firsttime,timediff,timem12,xtime,slopetime,a12;
  struct odtdata *odthistory,*prevrec;
  logical found=FALSE,land=TRUE,shear=TRUE;

  odthistory=odthistoryfirst;

  curtime=calctime(odtcurrent->IR.date,odtcurrent->IR.time);

  if(odthistoryfirst!=0) {
    firsttime=calctime(odthistoryfirst->IR.date,odthistoryfirst->IR.time);
  } else {
    /* no records in history file */
    firsttime=curtime;
    *curstrength=0;
    return odtcurrent->IR.Tfinal;
  }

  /* determine various time threshold values */
  timediff=curtime-firsttime;
  timem12=curtime-0.5;
  if(timem12<firsttime) first12flag=1;

  /* find record just prior to current record */
  lasttno12=0.0F;
  prevrec=0;
  while(odthistory!=0) {
    xtime=calctime(odthistory->IR.date,odthistory->IR.time);
    if(xtime>=curtime) break;
    if(xtime>=timem12) {
      /* find largest finalT# in last 12 hours prior to current record*/
      if(odthistory->IR.Tfinal>lasttno12) lasttno12=odthistory->IR.Tfinal;
    }
    prevrec=odthistory;
    odthistory=odthistory->nextrec;
  }

  if(prevrec!=0) {
    lasttno=prevrec->IR.Tfinal;
    lastCI=prevrec->IR.CI;
    lstrength=prevrec->IR.rule9;
  } else {
    /* current record is before first record in history file */
    *curstrength=0;
    return odtcurrent->IR.Tfinal;
  }

  /* determine current strengthening/weakening value */
  strength=0;
  if((odtcurrent->IR.Tfinal-lasttno)>0.0F) strength=1;
  if((odtcurrent->IR.Tfinal-lasttno)<0.0F) strength=-1;

  /* check slopes of FinalT# values over last 6/12/24 hours */

  /* adjust to account for first 48 hour adjustment to CDO and
     EMB C scenes (otherwise a false "significant strengthening"
     signal might occur at/around cutoff time).  Will just add
     the 0.5 T# adjustment to the significant slope values. */
  
  if((timediff>=2.0)&&(timediff<=3.0)) sigslope24=-1.5F;
  if((timediff>=2.0)&&(timediff<=2.5)) sigslope12=-3.0F;

  /* 24-hour determination */
  if((curtime-1.0)>=firsttime) {
    if((slopecal(odtcurrent,24.0,1))<=sigslope24) slopeflag24=1;
  }

  /* 12-hour determination */
  if((curtime-0.5)>=firsttime) {
    if((slopecal(odtcurrent,12.0,1))<=sigslope12) slopeflag12=1;
  }

  slopeval1=slopeflag24;
  slopeval2=slopeflag12;
  slopetime=12.0;

  /* Assign CI values */

  /* strength flags : 1 - no significant strengthening cycle noted
                      2 - significant strengthening cycle noted;
                           T# is increasing or steady
                      3 - applying Rule9 to CI value;
                           T# is decreasing
                      4 - strengthening during Rule9 application;
                           holding CI until T# converges
                      5 - were applying Rule9, but another strengthening
                          cycle was noted and CI/T# have converged;
                           will increase CI until weakening starts again
  */  
  if(lstrength<=1) {
    /* Storm has not been in a significant strengthening cycle.
       Never allow Tno to be less than greatest Tno in last
        12 hours (but never greater than 1.0 - Rule 9 rules
        below will handle this part).  This rule will flatten
        out CI values during strengthening cycle before Rule 9
        is applied
       In first 12 hours, allow time averaging to take effect,
       so just use current Tno as CI value. */
    if(first12flag==1) {
      /* first 12 hours of storm analysis */ 
      cival=odtcurrent->IR.Tfinal;
    } else {
      cival=MIN(odtcurrent->IR.Tfinal+r9v,MAX(lasttno12,odtcurrent->IR.Tfinal));
    }
    if(slopeval1==1) {
      /* significant strengthening cycle has been noted */
      strength=2;
    } else {
      strength=1;
    }
  } else if(lstrength==2) {
    /* storm has been under a significant strengthening cycle */
    if(strength>=0) {
      /* storm is still strengthening (significantly), but has stopped
         for the moment... will mark as strongest to date */
      strength=2;
      cival=MAX(lastCI,odtcurrent->IR.Tfinal);
    } else {
      /* storm has begun initial weakening; hold CI value constand
         also never allow CI value to be >1.0 than final T# */
      strength=3;
      cival=MIN(lastCI,odtcurrent->IR.Tfinal+r9v);
    }
  } else if(lstrength==3) {
    /* storm has been weakening after a significant strengthening cycle */ 
    maxtnoval(odtcurrent,slopetime,&iweak,&maxtno);
    if(odtcurrent->IR.Tfinal>lastCI) {
      /* storm has begun to restrengthen and current CI value
         is less than the last CI value (and the strongest CI
         value to date during stregthening cycle */
      strength=2;
      cival=odtcurrent->IR.Tfinal;
    } else {
      /* storm has been weakening; Rule 9 is to be applied
         since a significant strengthening cyclone was noted */
      if(iweak==1) {
        /* storm has been weakening for < 12 hours; maintain last T#
           also never allow CI value to be >1.0 than final T# */ 
        strength=3;
        cival=MIN(maxtno,MIN(odtcurrent->IR.Tfinal+r9v,lastCI));
      } else {
        if(strength<=0) {
          /* storm has been weakening for > 12 hours; hold T# up 1.0
             never allowing CI value to be > 1.0 than final T# */
          strength=3;
          cival=MIN(maxtno,MIN(odtcurrent->IR.Tfinal+r9v,lastCI));
        } else {
          /* do NOT allow CI number to increase after a storm is under 
             Rule 9 unless final T# and CI converge */
          cival=MIN(odtcurrent->IR.Tfinal+r9v,MAX(lastCI,odtcurrent->IR.Tfinal));
          if(slopeval2==1) {
            /* storm has begun to strengthen, and another significant
               strengthening cycle has been detected with 12 hour slope
               will mark with special strength flag and hold CI value
               constant until CI and Tno values converge */
            strength=5;
          } else {
            /* storm has begun to strengthen while Rule 9 is being
               applied to Tno.  Will hold CI constant until Tno
               and CI values converge */
            strength=4;
          }
        }
      }
    }
  } else if(lstrength==4) {
    /* storm has begun to strengthen while Rule 9 is being applied. */
    maxtnoval(odtcurrent,slopetime,&iweak,&maxtno);
    /* current CI number will be the minimum between the Rule 9
       CI value (Tno + 1.0) and the maximum CI value obtained
       previously, unless current Tno is less than this minimum value
       Do NOT allow CI number to increase after a storm is under
       Rule 9 unless Tno and CI converge. */
    cival=MIN(odtcurrent->IR.Tfinal+r9v,MAX(lastCI,odtcurrent->IR.Tfinal));
    if(strength>=0) {
      /* storm is currently strengthening */
      if(odtcurrent->IR.Tfinal>maxtno) {
        /* storm final T# is greater than max CI value obtained previously */
        strength=2;
      } else if(slopeval2==1) {
        /* storm has begun to strengthen, and another significant
           strengthening cycle has been detected with 12 hour slope
           will mark with special strength flag and hold CI value
           constant until CI and Tno values converge */
        strength=5;
        cival=lastCI;
      } else {
        /* storm is stregthening under Rule 9, but not significantly */
        strength=4;
      }
    } else {
      /*  storm has begun to weaken (again) under Rule 9 constraints
          also never allow CI value to be >1.0 than Final Tno */
      strength=3;
      cival=MIN(maxtno,MIN(odtcurrent->IR.Tfinal+r9v,lastCI));
    }
  } else if(lstrength==5) {
     /* storm has begun to restrengthen while Rule 9 was being
       applied to the storm Tno.  Another significant strengthening
       cycle has been detected, so CI value will be held constant
       until Tno and CI values converge (want to avoid a unnatural
       jump in CI value as Rule 9 is "turned off") */
    if(strength>=0) {
      /* current storm intensity value will the greater value
         between current Tno and last CI value (being held
         constant while storm restrengthens) */
      strength=5;
      cival=MAX(odtcurrent->IR.Tfinal,lastCI);
      if(odtcurrent->IR.Tfinal>lastCI) {
        /* if Tno and last CI value (being held constant) finally
           converge, "turn off" Rule 9 application and reassign
           current intensity flag to "insignificant value" until
           another significant strengtheing cycle occurs */
        strength=1;
        /* if a significant strengtheing cycle has been detected
           during the immediate time period, flag as a significant
           strengthening cycle for Rule 9 application when storm weakens */
        if(slopeval1==1) strength=2;
      }
    } else {
      /* storm has been weakening; Rule 9 is to be applied
         since a significant strengthening cycle was noted
         also never allow CI value to be >1.0 than Final Tno */
      strength=3;
      cival=MIN(odtcurrent->IR.Tfinal+r9v,lastCI);
    }
  }

  /* check for shear conditions or land interaction
     if undergoing shear or interacting with land for >= 12 hours
     "turn off" Rule 9 application and return CI value to Tno value
     for >= 12 hours. */
  if(lstrength>1) {
    if((odtcurrent->IR.scenetype==9)||(odtcurrent->IR.land==1)) {
      a12=curtime-0.5;
      odthistory=odthistoryfirst;
      while(odthistory!=0) {
        /* check to see how long storm has been undergoing
           "shear" conditions or land interaction...
           if >= 12 hours and has been under constant
           "shear" conditions or land interaction, 
           shear/land flag will be equal to 1, else 0 */
        xtime=calctime(odthistory->IR.date,odthistory->IR.time);
        if((xtime>=a12)&&(xtime<curtime)) {
          if(odthistory->IR.scenetype!=9) shear=FALSE;
          if(odthistory->IR.land!=1) land=FALSE;
          found=TRUE;
        } else {
          if(found) break;
        } 
        odthistory=odthistory->nextrec;
      }
      if((shear)||(land)) {
        /* if either shear/land flag is TRUE,
           turn off Rule 9 and let CI value be equal to the
           average of current and last CI values, and return
           current intensity flag to "insignificant value" until
           another significant strengtheing cycle occurs */
        strength=1;
        cival=MIN(lastCI,MAX(odtcurrent->IR.Tfinal,lasttno12));
      }
    }
  }

  *curstrength=strength;
  return cival;
} 

void maxtnoval(struct odtdata *odtcurrent,int stime,int *iweak,float *maxtno)
/* Determine maximum final T# value over period defined by 
   stime.  Return value and strengthening/weakening flag 
    Inputs  : odtcurrent - structure containing current analysis
              stime      - number of hours to calculate slope over
    Outputs : iweak      - weakening intensity flag (over last 12 hours)
                           1=weakening for less than 12 hours, 0=otherwise
              maxtno     - maximum T# during last 12 hours if storm has
                           been or is currently in a significant
                           strengthening cycle 
*/
{
  int weak=0;
  float tmax=0.0F;
  double curtime,xtime,slopetime=stime/24.0;
  logical found=FALSE;
  struct odtdata *odthistory;

  odthistory=odthistoryfirst;
  
  curtime=calctime(odtcurrent->IR.date,odtcurrent->IR.time);

  /* determine maximum CI number by looking for the last strength flag
     of 2 or 5 within history file.  If weakening has been occuring for
     less than 12 hours, return iweak value of 1, else return 0 */
  while(odthistory!=0) {
    xtime=calctime(odthistory->IR.date,odthistory->IR.time);
    if(xtime<curtime) {
      if((odthistory->IR.rule9==2)||(odthistory->IR.rule9==5)) {
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

float slopecal(struct odtdata *odtcurrent,double tval,int itype)
/* Calculate slope or y-intercept of all points over tval time period
    Inputs  : odtcurrent - structure containing current analysis
              tval       - time period to calculate slope or y-intercept
              itype      - flag value indicating slope or y-intercept calculation 
                           and parameter to utilize
    Outputs : return value is slope or y-intercept of line over time period desired
*/
{
  /* itype : 1 = Final T# (will return slope)
             2 = latitude (will return y-intercept)
             3 = longitude (will return y-intercept) */

  int icnt=0;
  float sumx=0.0,sumy=0.0,sumsqx=0.0,sumsqy=0.0,sumxy=0.0;
  float xvalue,yvalue,xbar,ybar,varx,vary,covxy,r,b,slope;
  double curtime,xtime,tlimit;
  logical found=FALSE;
  struct odtdata *odthistory;

  odthistory=odthistoryfirst;

  curtime=calctime(odtcurrent->IR.date,odtcurrent->IR.time);
  tlimit=curtime-(tval/24.0);

  while(odthistory!=0) {
    xtime=calctime(odthistory->IR.date,odthistory->IR.time);
    if((xtime<curtime)&&(xtime>=tlimit)) {
      icnt++;
      xvalue=(float)(curtime-xtime);
      if(itype==1) yvalue=odthistory->IR.Tfinal;
      if(itype==2) yvalue=odthistory->IR.latitude;
      if(itype==3) yvalue=odthistory->IR.longitude;
      sumx=sumx+xvalue;
      sumy=sumy+yvalue;
      sumsqx=sumsqx+(xvalue*xvalue);
      sumsqy=sumsqy+(yvalue*yvalue);
      sumxy=sumxy+(yvalue*xvalue);
      found=TRUE;
    } else {
      if(found) break;
    }
    odthistory=odthistory->nextrec;
  }
  /* if calculating slope of Final T# values, add in current value */
  if(itype==1) {
    /* add current record to slope calculation */
    sumy=sumy+odtcurrent->IR.Tfinal;
    sumsqy=sumsqy+(odtcurrent->IR.Tfinal*odtcurrent->IR.Tfinal);
    icnt++;
  }

  /* compute least squares fit of line for data points
     using the equation  Y = Y* + r(varX/varY)(X - X*) = Mx + B
     X* = mean of X values (time values)  = xbar
     Y* = mean of Y values (T# values)    = ybar
     varX = variance of X                 = varx
     varY = variance of Y                 = vary
     r = covariance - Y*X*                = r
     M = slope of line (desired value)    = slopecal = r*(sqrt(vary/varx))
     B = y-intercept                      = ybar-(slopecal*xbar)  */
  /* must have more than 3 data values to calculate slope */
  if(icnt<4) {
    slope=999.99;
    if(itype==1) slope=0.0;
    return slope;
  }

  xbar=sumx/(float)icnt;
  ybar=sumy/(float)icnt;
  varx=(sumsqx/(float)icnt)-(xbar*xbar);
  vary=(sumsqy/(float)icnt)-(ybar*ybar);
  covxy=(sumxy/(float)icnt)-(xbar*ybar);
  r=covxy/SQRT(varx*vary);
  if((varx==0.0F)||(vary==0.0F)) {
    slope=0.0F;
  } else {
    slope=r*(SQRT(vary/varx));
  }

  slope=(float)((int)(slope*10.0F))/10.0F;
  if(itype>=2) {
    b=ybar-(slope*xbar);
    return b;      /* y-intercept for latitude/longitude extrapolation */
  } else {
    return slope;  /* slope for Final T# slope calculation */
  }
  
}

double calctime(int date,int time)
/* Compute time in xxxxx.yyy units, where xxxxx is the
   day and yyy is the percentage of the day.  This routine
   will also correct for Y2K problems.
    Inputs  : date - Julian date
              time - time in HHMMSS format
    Outputs : function return value
*/
{
  int iyy;
  float sec,min,hour,partday;
  double timeout;

  if((date%1000)==0) {
    return 0.0;
  }

  iyy=date/1000;  /* obtain year */

  /* check for millenium designation in the year.
     if it is not there, add it onto the beginning */
  if(iyy<1900) {
    if(iyy>70) {
      date=1900000+date;
    } else {
      date=2000000+date;
    }
  }
  
  sec=((float)(time%100))/3600.0F;
  min=((float)((time/100)%100))/60.0F;
  hour=(float)(time/10000);
  partday=(hour+min+sec)/24.0F;
  timeout=(double)date+(double)partday;

  return timeout;
}
