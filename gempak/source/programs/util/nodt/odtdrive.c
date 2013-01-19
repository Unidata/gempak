#include "inc/odt.h"

/* various constants */
int kstart=24;     /* inner cloud region analysis radius (km) */
int kend=136;      /* outer cloud region analysis radius (km) */
int kenda=190;     /* automated cursor position analysis radius (km) */
int keyer=40;      /* outer eye region analysis radius (km) */
int kres=4;        /* resolution of the imagery in question */
int arfd;	   /* the FILE id for the image in question */

/* global variables */
char scenetype[][6]={ "EYE", "EYEL","EYES","EYER","DRYR",
                      "EMBC","CDO", "",   "",     "SHEAR" }; 

/* T#-Pressure/Wind relationships (Atlantic and Pacific) */
float tno[63]={-9999.F,-8888.F,
   2.0F,2.1F,2.2F,2.3F,2.4F,2.5F,2.6F,2.7F,2.8F,2.9F,
   3.0F,3.1F,3.2F,3.3F,3.4F,3.5F,3.6F,3.7F,3.8F,3.9F,
   4.0F,4.1F,4.2F,4.3F,4.4F,4.5F,4.6F,4.7F,4.8F,4.9F,
   5.0F,5.1F,5.2F,5.3F,5.4F,5.5F,5.6F,5.7F,5.8F,5.9F,
   6.0F,6.1F,6.2F,6.3F,6.4F,6.5F,6.6F,6.7F,6.8F,6.9F,
   7.0F,7.1F,7.2F,7.3F,7.4F,7.5F,7.6F,7.7F,7.8F,7.9F,8.0F};

float pres[2][63]= {
/* Atlantic pressure relationship values */
{-9999.0F,-8888.0F,1009.0F,
  1008.2F,1007.4F,1006.6F,1005.8F,1005.0F,1004.0F,1003.0F,1002.0F,1001.0F,
  1000.0F,998.8F,997.6F,996.4F,995.2F,994.0F,992.6F,991.2F,989.8F,988.4F,
   987.0F,985.4F,983.8F,982.2F,980.6F,979.0F,977.2F,975.4F,973.6F,971.8F,
   970.0F,968.0F,966.0F,964.0F,962.0F,960.0F,957.6F,955.2F,952.8F,950.4F,
   948.0F,945.4F,942.8F,940.2F,937.6F,935.0F,932.2F,929.4F,926.6F,923.8F,
   921.0F,918.0F,915.0F,912.0F,909.0F,906.0F,902.8F,899.6F,896.4F,893.2F,890.0F},
/* Pacific pressure relationship values */
{-9999.0F,-8888.0F,
  1000.0F,999.4F,998.8F,998.2F,997.6F,997.0F,995.8F,994.6F,993.4F,992.2F,
   991.0F,989.6F,988.2F,986.8F,985.4F,984.0F,982.4F,980.8F,979.2F,977.6F,
   976.0F,974.0F,972.0F,970.0F,968.0F,966.0F,963.6F,961.2F,958.8F,956.4F,
   954.0F,951.4F,948.8F,946.2F,943.6F,941.0F,938.2F,935.4F,932.6F,929.8F,
   927.0F,924.4F,921.8F,919.2F,916.6F,914.0F,910.8F,907.6F,904.4F,901.2F,
   898.0F,894.2F,890.4F,886.6F,882.8F,879.0F,874.8F,870.6F,866.4F,862.2F,858.0F} };

/* Atlantic/Pacific pressure relationship values */
float wind[63]={-9999.0F,-8888.0F,
    30.0F, 31.0F, 32.0F, 33.0F, 34.0F, 35.0F, 37.0F, 39.0F, 41.0F, 43.0F,
    45.0F, 47.0F, 49.0F, 51.0F, 53.0F, 55.0F, 57.0F, 59.0F, 61.0F, 63.0F,
    65.0F, 67.4F, 69.8F, 72.2F, 74.6F, 77.0F, 79.6F, 82.2F, 84.8F, 87.4F,
    90.0F, 92.4F, 94.8F, 97.2F, 99.6F,102.0F,104.6F,107.2F,109.8F,112.4F,
   115.0F,117.4F,119.8F,122.2F,124.6F,127.0F,129.6F,132.2F,134.8F,137.4F,
   140.0F,143.0F,146.0F,149.0F,152.0F,155.0F,158.0F,161.0F,164.0F,167.0F,170.0F};

/* global variables -- command line inputs */
int narg;
char **varg;
logical odt,olist,ograph,odel,owind,oautomode,ofirst48,override;
logical odump,spotanal,o48hr,oremote,ographtemp;
int     drawCI,drawTW,drawTR,igraph,iword;
int     ot1,ot2,idomain,ifixtype,larea;
char    hfile[200]="\0",listfile[200]="\0",fixfile[200]="\0";
char    iout[200]="\0",lserver[100]="\0";
char    *od1,*od2;

/* global structures */
struct odtdata *odthistoryfirst;
struct ringdata *tcircfirst;

/* external routines */
extern int  readhistoryfile(void);
extern int  writehistoryfile(void);
extern void xprintf(char *);
extern int  getuserinputs(void);
extern char *getsatname(void);
extern char *odtremote(void);
extern int  getcursorloc(float *,float *);
extern int  getdatetime(char *,int *,int *);
extern int  readsatdata(char *,float,float,int,float [maxd][maxd],float [maxd][maxd],float [maxd][maxd],int *,int *);
extern void odtautomode1(int,int,float *,float *,int *);
extern void odtautomode2(struct odtdata *,float [maxd][maxd],float [maxd][maxd],float [maxd][maxd],int,int,int *,float *,float *);
extern int  odtscene(struct odtdata *);
extern void odtintensity(struct odtdata *);
extern int  gettemps(float [maxd][maxd],float [maxd][maxd],float [maxd][maxd],int,int,struct odtdata *);
extern void inserthistoryrec(struct odtdata *);
extern void deletehistoryrec(double,double);
extern int  listhistory(double,double);
extern int  graphhistoryrec(double,double);
extern void textscreenoutput(struct odtdata *);
extern int  overrideloc(logical);
extern int  overridescene(logical,int *);


int odtdrive( int curdate, int curtime, char *imagefile, 
        float cenlat, float cenlon,
        char *historyfile, int lodt, int lauto, int ldelete, int ldomain,
        int ldump, int llist, int lrule48, int loverride, char *overridescene, 
        int lwind, int lspotanal, char *fixfil, char *outfil, 
        char *date1, int time1, char *date2, int time2 )
{
  int ii, iok,iuse;
  int numx,numy;
  float xlat,xlon;
  double firsttime,lasttime;
  float temps[maxd][maxd], lats[maxd][maxd], lons[maxd][maxd];
  char	datafile[256];

  struct odtdata *odtcurrent;


/* Get data inputs from commmand line */
/*   varg = argv; narg = argc; */


  /* define local and global structures */
  odthistoryfirst=0;
  odtcurrent=(struct odtdata *)malloc(sizeof(struct odtdata));
  tcircfirst=(struct ringdata *)malloc(sizeof(struct ringdata));

  /* obtain user input values via calling sequence from application pgm	*/
	ograph    = FALSE;
	oremote   = FALSE;

	odt       = ( lodt      == 1 );
	odel      = ( ldelete   == 1 );
	odump     = ( ldump     == 1 );
	olist     = ( llist     == 1 );
	owind     = ( lwind     == 1 );
	oautomode = ( lauto     == 1 );
	ofirst48  = ( lrule48   == 1 );
	override  = ( loverride == 1 );
	spotanal  = ( lspotanal == 1 );
	spotanal=FALSE;
	if(strstr(historyfile,"ODTDUMP.ODT")!=(char *)NULL) spotanal=TRUE;
  	strcpy ( hfile, historyfile );
  	strcpy ( fixfile, fixfil );
  	strcpy ( listfile, outfil );

	od1 = date1;
	od2 = date2;
	ot1 = time1;
	ot2 = time2;

	idomain = ldomain;
	iok = 0;

  if(iok!=0) {
    sprintf(iout,"ERROR READING COMMAND LINE INPUTS... EXITING\n");
    xprintf(iout);
    return(-1);
  }

  /* read history file into structure */
  iok=readhistoryfile();
  if(iok!=0) {
    sprintf(iout,"ERROR READING HISTORY FILE %s... EXITING\n",hfile);
    xprintf(iout);
    return(-2);
  }

  /* run ODT and obtain an intensity estimate */
  if(odt) {

    /*
     *  obtain full satellite file name and path
     *  *** NOW PASSED IN THRU CALLING SEQUENCE ***
     *
     */
    strcpy ( datafile, imagefile );

    if((long)datafile==0) {
      sprintf(iout,"ERROR FORMING DATAFILE NAME... EXITING\n"); 
      xprintf(iout);
      return(-8);
    }

    /*
     *  obtain image date (yyjjj) and time (hhmmss); 
     *  *** NOW PASSED IN THRU CALLING SEQUENCE ***
     *
    curdate = 99258;
    curtime = 104500;
     */
    iok = 0;

    if(iok!=0) {
      sprintf(iout, "ERROR OBTAINING DATE/TIME FROM IMAGE FILE...  EXITING\n"); 
      xprintf(iout);
      return(-2);
    }
    /* load date/time to odtcurrent */
    odtcurrent->IR.date=curdate;
    odtcurrent->IR.time=curtime;

    /* run ODT and obtain an intensity estimate */
    /* get cursor location either automatically or by user input */
    if(oautomode) {

      /* 
       *  AUTO-MODE
       *  get center location from official forecasts 
       */
      odtautomode1(curdate,curtime,&xlat,&xlon,&iuse);
      if(iuse==0) {
        /* 
	 *  interpolation and extrapolation failed... 
	 *  get user-defined cursor position 
         *  *** NOW PASSED IN THRU CALLING SEQUENCE ***
	 */
	iok = 0;
	if ( cenlat <  -90.0F || cenlat > 90.0F || 
	     cenlon < -180.0F || cenlon > 180.0F )  iok = -1;

        if(iok!=0) {
          sprintf(iout,"ERROR OBTAINING CURSOR POSITION... EXITING\n"); 
	  xprintf(iout);
          return(-6);
        }
      } 
      else {

        /* 
	 *  interpolation or extrapolation position was valid, 
	 *  proceed with auto center finding 
	 */
	  iok=readsatdata(datafile,xlat,xlon,kenda,temps,lats,lons,&numx,&numy);
	  if(iok!=0) {
	    sprintf(iout,"... EXITING\n"); xprintf(iout);
	    exit(-4);
	  }

        /* 
	 *  reposition center location, if necessary, 
	 *  using laplacian or 10^ log spiral analysis 
	 */
        odtautomode2(odtcurrent,temps,lats,lons,numx,numy,&iuse,&cenlat,&cenlon);
  
        /*  
	 *  override position, if necessary, using user input.
	 */
        if(override) {
	    iok = 0;
	    if ( cenlat <  -90.0F || cenlat > 90.0F || 
	         cenlon < -180.0F || cenlon > 180.0F )  iok = -1;

            if(iok!=0) {
              sprintf(iout,"ERROR OBTAINING CURSOR POSITION... EXITING\n"); 
	      xprintf(iout);
              return(-6);
            }
        }
      }
    } else {

	/* 
         *  USER-INTERACTION MODE
	 *  get cursor location at location chosen by user
         *  *** NOW PASSED IN THRU CALLING SEQUENCE ***
	 */
	iok = 0;
	if ( cenlat <  -90.0F || cenlat > 90.0F || 
	     cenlon < -180.0F || cenlon > 180.0F )  iok = -1;

        if(iok!=0) {
          sprintf(iout,"ERROR OBTAINING CURSOR POSITION... EXITING\n"); 
	  xprintf(iout);
          return(-6);
        }
    }

    /* read satellite data with center point defined from auto-mode or by user */
    iok=readsatdata(datafile,cenlat,cenlon,kend,temps,lats,lons,&numx,&numy);
    if(iok!=0) {
      sprintf(iout,"... EXITING\n"); xprintf(iout);
      exit(-4);
    }


    /* 
     *  obtain temperature values for eye and cloud top regions
     */

    iok=gettemps(temps,lats,lons,numx,numy,odtcurrent);
    if(iok!=0) {
      sprintf(iout,"ERROR OBTAINING LAND/OCEAN FLAG FROM TOPOHRES... EXITING\n");
      xprintf(iout);
      return(-9);
    }

    /* 
     *  determine scene type via Fourier Transform Analysis 
     */
    iok=odtscene(odtcurrent);
    if(iok!=0) {
      sprintf(iout,"ERROR IN FOURIER TRANSFORM ANALYSIS... EXITING\n");
      xprintf(iout);
      return(-7);
    }

    /* 
     *  override scene type if desired
     */
    if(override) {
        iok = -1;
	for ( ii = 0; ii < (int)(sizeof(scenetype)/sizeof(scenetype[0])); ii++ )  {
	    if ( strcmp(overridescene,scenetype[ii]) == 0 )  {
	        odtcurrent->IR.scenetype = ii;
		iok = 0;
		break;
	    }
	}
        if(iok==0) {
          sprintf(iout,"SCENE SET TO TYPE %s (%d)\n", overridescene, ii );
          xprintf(iout);
        }
        else  {
          sprintf(iout,"UNABLE TO SET SCENE TO TYPE %s\n", overridescene );
          xprintf(iout);
        }
    }

    /* 
     *  obtain intensity estimate using temperatures and scene type values
     */
    odtintensity(odtcurrent);

    /* 
     *  insert current intensity estimate into history file structure
     */
    inserthistoryrec(odtcurrent);

    /* 
     *  write history file to output data ASCII file
     */
    iok=writehistoryfile();
    if(iok!=0) {
      sprintf(iout,"ERROR WRITING HISTORY FILE %s... EXITING\n",hfile); 
      xprintf(iout);
      return(-2);
    }

    /* 
     *  output current intensity estimate to screen
     */
    textscreenoutput(odtcurrent);  /*** can be replaced with GUI interface ***/

  } else {
    /* 
     *  only LIST, GRAPH, or DELETE records within history file... 
     *  do not perform analysis
     */

    /* 
     *  obtain dates for functions based upon user inputs from command line
     */
    iok=getdates(&firsttime,&lasttime);
    if(iok!=0) {
      sprintf(iout,"ERROR OBTAINING DATES FROM COMMAND LINE ENTRY... EXITING\n");
      xprintf(iout);
      return(-5);
    }

    /* 
     *  delete records from history file
     */
    if(odel) {
      /* 
       *  delete history file records from structure
       */
      deletehistoryrec(firsttime,lasttime);
      /* 
       *  output edited history file structure to existing ASCII file
       */
      iok=writehistoryfile();
      if(iok!=0) {
        sprintf(iout,"ERROR WRITING HISTORY FILE %s... EXITING\n",hfile);
	xprintf(iout);
        return(-2);
      }
    }

    /* 
     *  list records from history file
     */
    if(olist) iok=listhistory(firsttime,lasttime);
    if(iok!=0) {
      sprintf(iout,"... EXITING\n"); 
      xprintf(iout);
      return(-2);
    }

  }
  
  /* free malloc'd memory defined for data structures */
  free(odthistoryfirst);
  free(odtcurrent);
  free(tcircfirst);
  sprintf(iout,"         *** Finished with ODT Analysis *** \n\n");
  xprintf(iout);

  return (0);

}

void xprintf(char *cout)
{
    printf("%s",cout);
/*   Mcprintf("%s",cout); */
}
