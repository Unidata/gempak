#include "inc/odt.h"

int readhistoryfile(void);
int writehistoryfile(void);
void inserthistoryrec( struct odtdata * );
void deletehistoryrec( double, double );
int listhistory( double, double );
int cmonth2julian( char * );
void julian2cmonth( int, char * );
float getpwval( int,float );

extern void odtintensity( struct odtdata *);
extern double calctime( int, int );
extern int idmyyd( int,int,int );
extern void yddmy( int,int *,int *,int * );
extern void xprintf(char *);

/* extern global variables */
extern int kstart,keyer,kres;
extern struct odtdata *odthistoryfirst;
extern char hfile[200],listfile[200];
extern char iout[200];
extern char *od1,*od2;
extern int  ot1,ot2,idomain;
extern logical odel,odump,owind,o48hr;
extern float tno[63],wind[63],pres[2][63];

int readhistoryfile(void)
/* Read history file from ASCII file into structure odthistory
    Inputs  : none
    Outputs : global structure odthistory 
*/
{
  FILE *fp;
  int  maxline=120,count=0;
  int  time,scene,r9,rapid,land;
  char line[120],cdate[12];
  float rawt,finalt,ci,eye,cloud,mean,lat,lon;
  struct odtdata *odthistory;

  sprintf(iout,"\nCURRENT HISTORY FILE = %s\n\n",hfile);
  xprintf(iout);

  /* get file information */
  fp=fopen(hfile,"r+");
  if(fp==0) {
    sprintf(iout,"HISTORY FILE DOES NOT EXIST... WILL CREATE\n");
    xprintf(iout);
    fp=fopen(hfile,"a+");
    if(fp==0) {
      return -2;
    } else {
      fclose(fp);
      return 0;
    }
  }

  /* read history file, quit at EOF */
  while(fgets(line,maxline,fp) != NULL) {
    if(count==0) {
      odthistoryfirst=(struct odtdata *)malloc(sizeof(struct odtdata));
      odthistory=odthistoryfirst;
    } else {
      odthistory->nextrec=(struct odtdata *)malloc(sizeof(struct odtdata));
      odthistory=odthistory->nextrec;
    }
    (void)sscanf(line,"%s %d %f %f %f %d %d %d %f %f %f %f %f %d",
                 cdate,&time,&rawt,&finalt,&ci,&scene,&r9,&rapid,
                 &eye,&cloud,&mean,&lat,&lon,&land);
    odthistory->IR.date=cmonth2julian(cdate);
    odthistory->IR.time=time;
    odthistory->IR.Traw=rawt;
    odthistory->IR.Tfinal=finalt;
    odthistory->IR.CI=ci;
    odthistory->IR.scenetype=scene;
    odthistory->IR.rule9=r9;
    odthistory->IR.rapid=rapid;
    odthistory->IR.eyet=eye;
    odthistory->IR.cloudt=cloud;
    odthistory->IR.meancloudt=mean;
    odthistory->IR.latitude=lat;
    odthistory->IR.longitude=lon;
    odthistory->IR.land=land;
    odthistory->IR.ringdist=0;
    odthistory->nextrec=0; /* make pointer for last record equal to 0 */
    count++;
  }
  sprintf(iout,"SUCCESSFULLY READ %d RECORD(S) FROM HISTORY FILE\n",count);
  xprintf(iout);

  fclose(fp);
  return 0;
}

void inserthistoryrec(struct odtdata *odtcurrent)
/* Insert or overwrite a record in a history file.  Global structure
   odthistory will be modified and the ASCII history file will be
   rewritten in another routine.
    Inputs  : odtcurrent - structure for current image information
    Outputs : modified structure odthistory
*/
{
  int modcount=0,count=0,icnpl;
  double curtime,xtime;
  logical found=FALSE,o48hrtmp;
  char cinpl[2][12]={ "AT START OF", "WITHIN" };
  struct odtdata *prevrec,*odttemp,*odthistory;

  odthistory=odthistoryfirst;

  curtime=calctime(odtcurrent->IR.date,odtcurrent->IR.time);

  o48hrtmp=o48hr;
  prevrec=odthistory;                  /* save previous record pointer */
  while(odthistory!=0) {
    count++;
    xtime=calctime(odthistory->IR.date,odthistory->IR.time);
    if((xtime==curtime)&&(!found)) {
      /* OVERWRITE RECORD */
      sprintf(iout,"OVERWRITING RECORD %d IN HISTORY FILE\n",count);
      xprintf(iout);
      odttemp=odthistory;
      if(odthistory==odthistoryfirst) {
        /* overwrite record at beginning of record list */
        odthistoryfirst=odtcurrent;
      } else {
        prevrec->nextrec=odtcurrent;
      }
      odthistory=odtcurrent;
      odthistory->nextrec=odttemp->nextrec;
      prevrec=odthistory;              /* save previous record pointer */
      odthistory=odthistory->nextrec;  /* switch pointer to next record */
      found=TRUE;
    } else if((xtime>curtime)&&(!found)) {
      /* INSERT RECORD */
      if(odthistory==odthistoryfirst) {
        /* insert at beginning of record list */
        odttemp=odthistory;
        odthistoryfirst=odtcurrent;   /* reset first history file pointer to odtcurrent */
        odthistoryfirst->nextrec=odttemp;  /* this is original first record */
        icnpl=0;
      } else {
        /* insert between two records */
        prevrec->nextrec=odtcurrent;    /* reset previous next record pointer to inserted record */
        odtcurrent->nextrec=odthistory; /* set next record pointer for inserted record */
        icnpl=1;
      }
      sprintf(iout,"INSERTING RECORD %s HISTORY FILE\n",cinpl[icnpl]);
      xprintf(iout);
      found=TRUE;
    } else {
      if(found) {
        /* previously found records to insert, so all records following
           the inserted record must be recalculated */
        odttemp=odthistory;
        odtintensity(odttemp);      /* recompute intensity */
        odthistory=odttemp;
        modcount++;
      }
      /* nothing yet... keep searching */
      prevrec=odthistory;              /* save previous record pointer */
      odthistory=odthistory->nextrec;  /* switch pointer to next record */
    }
  }

  if(!found) {
    odtcurrent->nextrec=0;       /* set next record pointer to 0 for end */
    if(odthistoryfirst==odthistory) {
      /* record will be placed at start of new history structure */
      odthistoryfirst=odtcurrent;
      sprintf(iout,"ADDING RECORD TO EMPTY HISTORY FILE\n");
      xprintf(iout);
    } else {
      /* record will be placed at end of history structure */
      prevrec->nextrec=odtcurrent; /* reset previous next record pointer to inserted record */
      sprintf(iout,"ADDING RECORD TO END OF HISTORY FILE\n");
      xprintf(iout);
    }
  } else {
    if(modcount>0) {
      sprintf(iout,"MODIFIED %d SUBSEQUENT RECORDS IN HISTORY FILE\n",modcount);
      xprintf(iout);
    }
    o48hr=o48hrtmp;
  }

}

int writehistoryfile(void)
/* Write odthistory structure to ASCII history file.
   Inputs  : none
   Outputs : none
*/
{
  FILE *fp;
  int  count=0;
  char cdate[12];
  struct odtdata *odthistory;

  odthistory=odthistoryfirst;

  /* get file information */

  fp=fopen(hfile,"w+");
  if(fp==0) {
    return -2;
  }
  /* print history file to output ASCII history file */
  while (odthistory!=0) {

    (void)julian2cmonth(odthistory->IR.date,cdate);

    (void)fprintf(fp,"%9s %6d %3.1f %3.1f %3.1f %1d %1d %1d %6.2f %6.2f %6.2f %6.2f %6.2f %1d\n",
                 cdate,odthistory->IR.time,
                 odthistory->IR.Traw,odthistory->IR.Tfinal,odthistory->IR.CI,
                 odthistory->IR.scenetype,odthistory->IR.rule9,odthistory->IR.rapid,
                 odthistory->IR.eyet,odthistory->IR.cloudt,odthistory->IR.meancloudt,
                 odthistory->IR.latitude,odthistory->IR.longitude,odthistory->IR.land);
    odthistory=odthistory->nextrec;
    count++;
  }

  sprintf(iout,"SUCCESSFULLY WROTE %d RECORD(S) TO HISTORY FILE\n",count);
  xprintf(iout);

  fclose(fp);
  return 0;
}

void deletehistoryrec(double starttime,double endtime)
/* Delete record(s) in a history file.  Routine will modify
   structure odthistory, which will then be rewritten to 
   ASCII history file in another subroutine.
    Inputs  : starttime - first date/time of record(s) delete
              endtime   - last date/time of record(s) delete
    Outputs : global structure odthistory will be modified
*/
{
  int  modcount=0,count=0;
  double curtime;
  logical found=FALSE;
  struct odtdata *prevrec,*odttemp,*odthistory;

  odthistory=odthistoryfirst;

  prevrec=odthistory;                  /* save previous record pointer */
  while(odthistory!=0) {
    curtime=calctime(odthistory->IR.date,odthistory->IR.time);
    if((curtime>=starttime)&&(curtime<=endtime)) {
      /* record falls within time boundaries to delete */
      if(odthistory==prevrec) {
        /* deleting first record, need to reset first record pointer */
        odthistoryfirst=prevrec->nextrec;
        prevrec=odthistory->nextrec;
      } else {
        /* reset previous next record pointer to next record */
        prevrec->nextrec=odthistory->nextrec;
      }
      count++;
      found=TRUE;
    } else {
      /* record not within time boundaries */
      if(found) {
        /* previously found records to delete, so all records following
           the last deleted record must be recalculated */
        odttemp=odthistory;
        odtintensity(odttemp);
        odthistory=odttemp;
        modcount++;
      }
      /* reset prevrec pointer to current record */
      prevrec=odthistory;
    }
    /* reset record pointer ot next record */
    odthistory=odthistory->nextrec;
  }

  if(found) {
    sprintf(iout,"DELETED %d RECORD(S) IN HISTORY FILE\n",count);
    xprintf(iout);
    if(modcount>0) {
      sprintf(iout,"MODIFIED %d SUBSEQUENT RECORDS IN HISTORY FILE\n",modcount);
      xprintf(iout);
    }
  } else {
    /* no records deleted since date/times were out of range */
    sprintf(iout,"NO RECORDS WERE DELETED HISTORY FILE\n");
    xprintf(iout);
  }
}

int listhistory(double starttime,double endtime)
/* List the ASCII history file between given date/times.
    Inputs  : starttime - first date/time of record(s) delete
              endtime   - last date/time of record(s) delete
    Outputs : none
*/
{
  FILE *fp,*fpo;
  int  maxline=120;
  int  date,time,scene,r9,rapid,land,xr9,xrap;
  char line[120],cdate[12];
  char cyn[2][4]={ "OFF","ON " };
  char cpw[2][5]={ "MSLP", "Wind" };
  char cscene[10][4]={ "EYE","EYE","EYE","EYE","EYE","EMB","CDO","   ","   ","SHR" };
  char cland;
  float rawt,finalt,ci,eye,cloud,mean,lat,lon,pwi,cloudtemp;
  double curtime;
  logical firstrec=TRUE;

  /* get history file information */
  fp=fopen(hfile,"r+");
  if(fp==0) {
    sprintf(iout,"ERROR READING HISTORY FILE");
    xprintf(iout);
    return -2;
  }
  if(odump) {
    fpo=fopen(listfile,"w+");
    if(fpo==0) {
      sprintf(iout,"ERROR WRITING OUTPUT FILE %s",listfile);
      xprintf(iout);
      return -2;
    }
  }

  /* determine Rule 9 and Rapid Deepening screen output values */

  /* read history file, quit at EOF */
  while(fgets(line,maxline,fp) != NULL) {
    (void)sscanf(line,"%s %d %f %f %f %d %d %d %f %f %f %f %f %d\n",
                 cdate,&time,&rawt,&finalt,&ci,&scene,&r9,&rapid,
                 &eye,&cloud,&mean,&lat,&lon,&land);
    date=cmonth2julian(cdate);
    curtime=calctime(date,time);
    xr9=0;if(r9>=2) xr9=1;
    xrap=0;if((rapid==2)||(rapid==3)) xrap=1;
    pwi=getpwval(owind,ci);
    cland=' ';if(land==1) cland='*';
    cloudtemp=cloud;
    if(rapid>1) cloudtemp=MIN(cloud,mean);
    if((curtime>=starttime)&&(curtime<=endtime)) {
      if(odump) {
        /* dump contents of history file to ASCII file */
        if(firstrec) {
          (void)fprintf(fpo,"                  Intensity       T-No    Temperatures(C)  Scene  Rule9  Rapid\n");
          (void)fprintf(fpo,"   Date    Time   CI   %4s     Avg  Raw    Eye   Cloud    Type   Flag   Flag     Lat     Lon\n",cpw[owind]);
          firstrec=FALSE;
        }
        (void)fprintf(fpo,"%9s %6d  %3.1f %6.1f    %3.1f  %3.1f  %6.2f %6.2f    %c%3s    %3s    %3s  %7.2f %7.2f\n",
               cdate,time,ci,pwi,finalt,rawt,eye,cloudtemp,cland,cscene[scene],cyn[xr9],cyn[xrap],lat,lon);
      } else {
        /* dump contents of history file to screen */
        if(firstrec) {
          (void)sprintf(iout,"                  Intensity       T-No    Temperatures(C)  Scene  Rule9  Rapid\n");
          xprintf(iout);
          (void)sprintf(iout,"   Date    Time   CI   %4s     Avg  Raw    Eye   Cloud    Type   Flag   Flag\n",cpw[owind]);
          xprintf(iout);
          firstrec=FALSE;
        }
        sprintf(iout,"%9s %6d  %3.1f %6.1f    %3.1f  %3.1f  %6.2f %6.2f    %c%3s    %3s    %3s\n",
               cdate,time,ci,pwi,finalt,rawt,eye,cloudtemp,cland,cscene[scene],cyn[xr9],cyn[xrap]);
        xprintf(iout);
      }
    }
  }

  fclose(fp);
  if(odump) fclose(fpo);
  return 0;

}

int getdates(double *starttime, double *endtime)
/* Obtain start and end time to list/graph/delete using DATE=
   command line entries for start and end dates.  Output values
   will be in ddd.ddd format.
    Inputs  : global variables containing date/time information
    Outputs : starttime - first date/time of record(s) delete
              endtime   - last date/time of record(s) delete
*/
{
  int iyyddd1=0,iyyddd2=0;

  /* convert character date format to Julian date format */
  iyyddd1=cmonth2julian(od1);
  iyyddd2=cmonth2julian(od2);

  /* if deleting records, must specify beginning record */
  if(odel) {
    if(iyyddd1==0) {
      sprintf(iout,"MUST SPECIFY FIRST RECORD FOR DELETE OPTION\n");
      xprintf(iout);
      return -1;
    }
    *starttime=calctime(iyyddd1,ot1);
    if(iyyddd2==0) {
      *endtime=*starttime;
    } else {
      *endtime=calctime(iyyddd2,ot2);
    }

  /* if graphing or listing, no input specifying date limits will
     result in list/graph of entire file */
  } else {
    if(iyyddd1==0) {
      *starttime=calctime(odthistoryfirst->IR.date,odthistoryfirst->IR.time);
    } else {
      *starttime=calctime(iyyddd1,ot1);
    }
    if(iyyddd2==0) {
      *endtime=calctime(9999999,235959);
    } else {
      *endtime=calctime(iyyddd2,ot2);
    }
  }
  
  return 0;
}

int cmonth2julian( char *cval1 )
/* Convert YYYYMMMDD format to julian date.
    Inputs  : cval1 - character representation of date in YYYYmonDD format
                      where DD and YYYY are integers and mon is a three
                      character abbreviation for the month (e.g. 2000MAR13)
    Outputs : return value is julian date 
*/
{
  int  iday,iyear;
  int  imon=0,iyyddd=0;
  char cmon[4];
  char month[12][4]={ "JAN","FEB","MAR","APR","MAY","JUN",
                      "JUL","AUG","SEP","OCT","NOV","DEC" };

  /* break down character string into various components */
  (void)sscanf(cval1,"%4d%3s%2d",&iyear,cmon,&iday);

  /* calculate integer month value */
  while((strncmp(cmon,month[imon],3)!=0)&&(imon<12)) { imon++; }

  /* determine julian date from day/month/year */
  if(imon<12) iyyddd=idmyyd(iday,imon+1,iyear);

  return iyyddd;

}

void julian2cmonth( int julian,char *cvalue )
/* convert julian date to YYYYMMMDD format for output.
    Inputs  : julian - Julian date representation of date
    Outputs : cvalue - character representation of date in YYYYmonDD format
                       where DD and YYYY are integers and mon is a three
                       character abbreviation for the month (e.g. 2000MAR13)
*/
{
  int iday,imon,iyear;
  char month[12][4]={ "JAN","FEB","MAR","APR","MAY","JUN",
                      "JUL","AUG","SEP","OCT","NOV","DEC" };

  /* calculate date/month/year from julian date */
  (void)yddmy(julian,&iday,&imon,&iyear);

  /* form character string representation from various components */
  if(iday<10) {
    (void)sprintf(cvalue,"%4d%3s0%1d",iyear,month[imon-1],iday);
  } else {
    (void)sprintf(cvalue,"%4d%3s%2d",iyear,month[imon-1],iday);
  }

}

float getpwval(int ival,float cival)
/* Obtain pressure or wind value (for Atlantic or
   West Pacific storms) given the intensity estimate
   value.
    Inputs  : ival  - flag for wind (1) or pressure (0) output
              cival - Current Intensity (CI) value
    Outputs : return value is pressure/wind value
*/
{
  float value;
  int ixx=2;
 
  /* determine correct pressure/wind array bin */
  while((cival>tno[ixx])&&(ixx<62)) { ixx++; }

  /* convert CI value to wind/pressure value */
  if(ival==1) {
    value=wind[ixx];           /* WIND */
  } else {
    value=pres[idomain][ixx];  /* PRESSURE */
  }

  return value;
}
