/* C library routines */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <math.h>
#include <float.h>
/* library file containting external AODT library function calls */
#include "../inc/odtapi.h"
#include "../inc/odtextern.h"
#include "../inc/odtmcidas.h"

#define maxd 500
#define maxc1 5000
#define maxc2 50000
#define maxc5 6
#define maxc20 20
#define maxc50 50
#define maxc100 100
#define maxc200 200
#define nvt 92

/* global variables (malloc'd and freed) */
  char    *retmsg;
  char    *scenestr,*oscenestr,*comment;
  char    *version;
  char    *bulletin,*listing;
  char    *infomsg;
  char    *ccursat;
  char    *hpath,*tpath,*spath;
  char    *lpath,*apath,*dpath;
  char    *hhfile,*listfile,*ffixfile;
  char    *topofile,*sstfile;
  char    *datafile,*atcfpath,*atcffile;
  char    *lserver,*od1,*od2,*stormID,*atcfsrcID;
  char    *fixxfile,*gridpath;
  struct odtdata *historyrec;

/*------------------------------------------------------------------------------------*/
void allocvars();
void freevars();
void exitaodt(char *);

/* plotting for center fix routines */
extern void initpl_( int*, int*);
extern void page_( int*, int*, int*, int*, int*);
extern void plot_( int*, int*, int*);
extern void wrtext_( int*, int*, int*, const char*, int*, int*, size_t);
/* end plotting variables */

int main(int argc, char **argv)
{
  FILE    *fpo;
  int     izz;
  int     iok,iok2,b,bptr,bptr2,cc,ixx,iyy;
  int     odt,olist,ograph,odel,owind,oautomode,override,odc;
  int     odump,oremote,ographtemp,ostartstr,oland,osearch,ocommadd,oatcf,ovrad;
  int     drawCI,drawCIadj,drawTW,drawTR,drawTRO,drawTN,drawTIEr,drawTIEa,igraph,iword;
  int     ot1,ot2,ixdomain,idomain,ifixtype,larea,ilisttype,cursat,wvmax,wgrid;
  int     idomainx,topovalue,ifixxtype,rmwsize;
  int     nlines,neles;
  int     deleted,modified;
  int     oeyescene,ocloudscene,eyescene,cloudscene;
  int     origeyescene,origcloudscene,neweyescene,newcloudscene;
  int     pos,imagedate,imagetime,hrecs,hmods;
  int     xcursat,xdate,xtime;
  int     radius=225,radiusAUTOFIX=500;   /* changed radius from 190 to 225 */
  float   osstr,lat,lon,flat,flon,wlat,wlon,sstvalue;
  float   **temps,**lats,**lons;
  float   *vcrit,***v2d;

  b=sizeof(float);
  bptr=sizeof(float*);
  bptr2=sizeof(float**);

  /* Initialization call for McIDAS Environment ONLY */
  iok=mcidas_initenv(argc,argv);
  if(iok!=0) { mcidas_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-91 ; info : none */
  /* end McIDAS initialization */

  /* for(izz=0;izz<=5;izz++) {
  printf("IZZ=%d\n",izz); */
  /* allocate variables */
  allocvars();
 
  /* initialize McIDAS variables */
  iok=mcidas_initialize( );  /* return 0 */

  /* initialize AODT current analysis structure elementes */
  iok=aodt_initialize( );   /* return 0 */

  /* retrieve AODT version from AODT library */
  iok=aodt_getversion(version);  /* return 0 */
  printf("===== %18s =====\n",version);

  /* obtain path names for variables stored in env definitions via McIDAS */
  iok=mcidas_getpaths(hpath,tpath,spath,lpath,apath,dpath);  /* return 0 */
  /* send path names for variables stored in env definitions to McIDAS */
  iok=mcidas_setpaths(hpath,tpath,spath,lpath,apath,dpath);  /* return 0 */

  /* obtain user input values from command line entry via McIDAS */
  iok=mcidas_getinputs(argc,argv,
		    &odt,&ograph,&olist,&odel,&oland,&owind,
                    &override,&oautomode,&ographtemp,&odump,&ostartstr,
                    &oremote,&osearch,&ocommadd,&oatcf,&ovrad,
                    &ot1,&ot2,&ixdomain,&ifixtype,&larea,&ilisttype,
                    &drawCI,&drawCIadj,&drawTW,&drawTR,&drawTRO,&drawTN,&drawTIEr,&drawTIEa,
                    &igraph,&iword,&wvmax,&wgrid,&rmwsize,
                    &osstr,
                    hhfile,listfile,ffixfile,lserver,topofile,sstfile,atcfpath,stormID,atcfsrcID,
                    gridpath,comment,od1,od2);
  if(iok!=0) { mcidas_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error: none ; info :121 */

  /* send various command line options to AODT library */
  iok=aodt_setmiscoptions(oland,osearch,rmwsize);   /* return 0 */
  /* retrieve various command line options from AODT library */
  /* iok=aodt_getmiscoptions(&olandx,&osearchx,&rmwsizex);   / return 0 */

  /* send initial classification flag and value to AODT library */
  iok=aodt_setstartstr(ostartstr,osstr);   /* return 0 */
  /* retrieve initial classification flag and value from AODT library */
  /* iok=aodt_getstartstr(&ostartstrx,&osstrx);   / return 0 */

  if(strlen(hhfile)>0) {
    /* send history file name to AODT library */
    iok=aodt_sethistoryfile(hhfile);
    if(iok!=0) { aodt_qmessage(iok,0,hhfile,retmsg); if(iok<0) exitaodt(retmsg); }  /* error: -1 ; info : 11,17 */
    /* retrieve history file name from AODT library */
    /* iok=aodt_gethistoryfile(hxfile); */
    /* if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  / error: -1 ; info : none */
  }

  /* Run AODT */
  if(odt) {
    /* echo AODT analysis technique : full or abbreviated */
    iok=1;
    if(strlen(hhfile)>0) iok=2;
    aodt_qmessage(iok,0,NULL,retmsg);  /* error: none ; info : 2 */

    /* obtain full satellite file name and path via McIDAS */
    iok=mcidas_getsatdatainfo(datafile,larea,lserver,&imagedate,&imagetime,&cursat);
    if(iok!=0) { mcidas_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-11,-12,-23 ; info : none */

    /* send satellite file name and date/time information to AODT library */
    iok=aodt_setIRimageinfo(imagedate,imagetime,cursat);   /* return 0 */
    /* retrieve satellite file name and date/time information from AODT library */
    iok=aodt_getIRimageinfo(&xdate,&xtime,&xcursat,ccursat);   /* return 0 */

    /* if executing AODT in AUTO MODE, obtain location */
    if(oautomode) {
      /* send forecast file name to AODT library */
      iok=aodt_setforecastfile(ffixfile,ifixtype);    /* return 0 */
      /* retrieve forecast file name from AODT library */
      iok=aodt_getforecastfile(fixxfile,&ifixxtype);
      if(iok!=0) { aodt_qmessage(iok,0,ffixfile,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-51 ; info :14 */

      /* determine initial auto-center position estimate */
      iok=aodt_runautomode1(&flat,&flon,&pos);
      /* send center point location to AODT library */
      iok2=aodt_setlocation(flat,flon,pos);
      if(iok2<0) { aodt_qmessage(iok2,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-21 ; info : 21,22 */
      if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-43,-44,-45,-46 ; info : 42,43,44,45,46 */

      /* get satellite data arrays via McIDAS */
      /* allocate memory */
      temps=(float **)calloc((size_t)maxd,bptr);
      lats=(float **)calloc((size_t)maxd,bptr);
      lons=(float **)calloc((size_t)maxd,bptr);
      for(iyy=0;iyy<maxd;iyy++) {
        temps[iyy]=(float *)calloc((size_t)maxd,b);
        lats[iyy]=(float *)calloc((size_t)maxd,b);
        lons[iyy]=(float *)calloc((size_t)maxd,b);
      }
      iok=mcidas_getsatimagedata(datafile,flat,flon,radiusAUTOFIX,temps,lats,lons,&nlines,&neles);
      if(iok!=0) { mcidas_qmessage(iok,0,datafile,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-11,-12,-13,-15,-16,-17 ; info : 12 */
      /* send satellite image data arrays to AODT library */
      iok=aodt_loadIRimage(temps,lats,lons,nlines,neles);   /* return 0 */

      /* free memory */
      for(iyy=0;iyy<maxd;iyy++) {
        free(temps[iyy]);
        free(lats[iyy]);
        free(lons[iyy]);
      }
      free(lons);
      free(lats);
      free(temps);
 
      /* set warmest pixel temp value in AODT library, return position for IR image data read */
      iok=aodt_getwarmeyetemplocation(&wlat,&wlon);
      if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error: none ; info : 91 */

      /* determine "best" autometed position */
      iok=aodt_runautomode2(flat,flon,&lat,&lon,&pos);

      /* send center point location to AODT library */
      iok2=aodt_setlocation(lat,lon,pos);
      if(iok2<0) { aodt_qmessage(iok2,0,NULL,retmsg); if(iok2<0) exitaodt(retmsg); }  /* error:-21 ; info : 21,22 */
      if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error: none ; info : 51 */

    } else {
      /* get MANUAL satellite cursor location via McIDAS */
      iok=mcidas_getcursorloc(datafile,&lat,&lon);
      iok2=aodt_setlocation(lat,lon,0);
      if(iok2!=0) { aodt_qmessage(iok2,0,NULL,retmsg); if(iok2<0) exitaodt(retmsg); }  /* error:-21 ; info : 21,22 */
      if(iok!=0) { mcidas_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-22,-23 ; info : none */
    }

    /* retrieve center point location from AODT library */
    /* iok=aodt_getlocation(&xlat,&xlon,&xpos); */
    /* if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  / error:-21 ; info : 21,22 */

    /* send oceanic domain flag to AODT library, set domain within subroutine */
    iok=aodt_setdomain(ixdomain);
    if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-81 ; info : none */
    /* retrieve oceanic domain from AODT library */
    iok=aodt_getdomain(&idomainx);
    if(iok!=0) { aodt_qmessage(iok,idomainx,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error: none ; info : 72,73 */

    /* obtain SST value from external SST GRIB file reader */
    iok=extern_readsstvalue(sstfile,lat,lon,&sstvalue );
    /* if(iok!=0) { extern_qmessage(15,0,sstfile,retmsg); if(iok<0) exitaodt(retmsg); }  / error: none ; info : 15 */
    /* if(iok!=0) { extern_qmessage(iok,(int)(sstvalue*10),NULL,retmsg); if(iok<0) exitaodt(retmsg); }  / error: none ; info : 81,111,112,113,114 */
    /* set SST value in AODT library */
    iok=aodt_setsstvalue(sstvalue);
    if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-21 ; info : 115 */
    /* retrieve SST value from AODT library */
    /* iok=aodt_getsstvalue(&sstvalue); */
    /* if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  / error: none ; info : 115 */

    /* obtain topography value from AODT library topography file reader */
    iok=aodt_readtopofile(topofile,lat,lon,&topovalue );
    if(iok!=0) { 
      aodt_qmessage(16,0,topofile,retmsg);   /* error: none ; info : 16 */
      if(iok<0) aodt_qmessage(iok,0,NULL,retmsg);      /* error:-31,-32 ; info : 71 */
      if(iok<0) exitaodt(retmsg); 
    }
    /* set topography value in AODT library */
    iok=aodt_settopovalue(topovalue);
    if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-21,-33 ; info : none */
    /* retrieve topography value from AODT library */
    /* iok=aodt_gettopovalue(&topovalue); */
    /* if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  / error:-33 ; info : none */

    /* get satellite data arrays via McIDAS */
    /* allocate memory */
    temps=(float **)calloc(maxd,bptr);
    lats=(float **)calloc(maxd,bptr);
    lons=(float **)calloc(maxd,bptr);
    for(iyy=0;iyy<maxd;iyy++) {
      temps[iyy]=(float *)calloc(maxd,b);
      lats[iyy]=(float *)calloc(maxd,b);
      lons[iyy]=(float *)calloc(maxd,b);
    }
    iok=mcidas_getsatimagedata(datafile,lat,lon,radius,temps,lats,lons,&nlines,&neles);
    if(iok!=0) { mcidas_qmessage(iok,0,datafile,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-11,-12,-13,-15,-16,-17 ; info : 12 */
    /* send satellite image data arrays to AODT library */
    iok=aodt_loadIRimage(temps,lats,lons,nlines,neles);   /* return 0 */

    /* free memory */
    for(iyy=0;iyy<maxd;iyy++) {
      free(temps[iyy]);
      free(lats[iyy]);
      free(lons[iyy]);
    }
    free(lons);
    free(lats);
    free(temps);
 
    /* set eye and cloud temp values in AODT library, 
       return position for IR image data read */
    iok=aodt_seteyecloudtemp( );
    if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-51 ; info : none */

    /* determine scene type */
    iok=aodt_scenetype( );
    if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-41,-51 ; info : none */

   /* obtain eye and cloud scene types from AODT library */
   /* oeye and ocloud will be -1 unless override has been performed */
   iok=aodt_getscenetypes(&eyescene,&cloudscene,&oeyescene,&ocloudscene);
   iok=aodt_scenemap(eyescene,cloudscene,scenestr); /* return 0 */

    /* override scene types, if desired */
    if(override) {
      iok=mcidas_overridescenetype(&origeyescene,&origcloudscene,&neweyescene,&newcloudscene);
      if(iok!=0) { mcidas_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-92 ; info : 31,32 */
      if(iok==32) {
        /* send new eye and cloud scene types to AODT library */
        iok=aodt_setscenetypes(neweyescene,newcloudscene,origeyescene,origcloudscene);  /* return 0 */
      }
      /* obtain eye and cloud scene types from AODT library */
      /* oeye and ocloud will be -1 unless override has been performed */
      iok=aodt_getscenetypes(&eyescene,&cloudscene,&oeyescene,&ocloudscene);
      iok=aodt_scenemap(eyescene,cloudscene,scenestr); /* return 0 */
      iok=aodt_scenemap(oeyescene,ocloudscene,oscenestr); /* return 0 */
    }

    /* determine intensity */
    iok=aodt_intensity( );
    if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error: none ; info : 71 */

    if(ovrad) {
      /* determine wind profile and critical values */
      vcrit=(float *)calloc((size_t)9,b);
      v2d=(float ***)calloc((size_t)nvt,bptr2);
      for(ixx=0;ixx<nvt;ixx++) {
        v2d[ixx]=(float **)calloc((size_t)nvt,bptr);
        for(iyy=0;iyy<nvt;iyy++) {
          v2d[ixx][iyy]=(float *)calloc((size_t)3,b);
        }
      }
      iok=aodt_getwindprofile(wvmax,vcrit,v2d);
      iok=mcidas_writegrid(gridpath,wgrid,v2d,nvt,nvt,xdate,xtime); 
      if(iok!=0) { mcidas_qmessage(iok,wgrid,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-101,-102,-103,-104,-105, ; info : 106 */

      iok=extern_writegrid(gridpath,wgrid,v2d,nvt,nvt,xdate,xtime,vcrit);
      if(iok!=0) { extern_qmessage(iok,wgrid,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-107,-108 ; info : 107 */
      free(vcrit);
      for(ixx=0;ixx<nvt;ixx++) {
        for(iyy=0;iyy<nvt;iyy++) {
          free(v2d[ixx][iyy]);
        }
        free(v2d[ixx]);
      }
      free(v2d);
    }

    /* print AODT intensity estimate in bulletin format */
    iok=aodt_bulletinoutput(bulletin);   /* return 0 */
    aodt_qmessage(500,0,bulletin,retmsg);  /* error: none ; info : 104 */

    /* write output to history file, if necessary */
    if(strlen(hhfile)>0) {
      /* insert current intensity analysis into history file */
      iok=aodt_historyrecordinsert(&hmods,&hrecs);
      if(iok!=0) {
        aodt_qmessage(iok,hrecs-hmods+1,hhfile,retmsg);  /* error: none ; info : 61,62,63,64 */
        if(hmods!=0) aodt_qmessage(65,hmods,hhfile,retmsg);  /* error: none ; info : 65 */
      }
      /* write updated history records to file */
      iok=aodt_historywritefile(&hrecs);
      if(iok!=0) { aodt_qmessage(iok,hrecs,hhfile,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-2,-3 ; info : 67 */
    }

    if(oatcf) {
      iok=aodt_atcfoutputfile(atcfpath,ilisttype,stormID,atcffile);
      /* open ASCII dump file if requested */
      fpo=fopen(atcffile,"w+");
      if(fpo!=0) { aodt_qmessage(-72,0,atcffile,retmsg); if(fpo<0) exitaodt(retmsg); }  /* error: -72 ; info : none */
      iok=aodt_historygetnextrec(-1,&historyrec); 
      iok=aodt_historylistfmt(historyrec,ilisttype,atcfsrcID,stormID,listing);  /* return 0 */ 
      /* send header to ASCII output file */
      fprintf(fpo,"%s",listing);
      aodt_qmessage(105,0,atcffile,retmsg);  /* error: none ; info : 105 */
    }

    aodt_qmessage(104,0,NULL,retmsg);  /* error: none ; info : 104 */

  } else {

    /* send dates and times to AODT library */
    odc=((odel)||(ocommadd)) ? 1 : 0;
    iok=aodt_setdatetime(ot1,ot2,od1,od2,odc);   /* return 0 */
    if(iok!=0) { aodt_qmessage(iok,deleted,hhfile,retmsg); if(iok<0) exitaodt(retmsg); }  /* error: -61 ; info : none */

    /* send bogus center point location to AODT library */
    iok2=aodt_setlocation(0.0,0.0,0);

    /* send oceanic domain flag to AODT library, set domain within subroutine */
    iok=aodt_setdomain(ixdomain);
    if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-81 ; info : none */

    if(olist) {
      /* open ASCII dump file if requested */
      if(odump) {
        fpo=fopen(listfile,"w+");
        if(fpo!=0) { aodt_qmessage(-71,0,listfile,retmsg); if(fpo<0) exitaodt(retmsg); }  /* error: -71 ; info : none */
      }
      /* obtain header for history file listing */
      iok=aodt_historylistfmt(0,ilisttype,atcfsrcID,stormID,listing);   /* return 0 */ 
      printf("%s",listing);

      /* send header to ASCII output file */
      if(odump) fprintf(fpo,"%s",listing); 

      /* obtain pointer to first record in history file data structure */
      iok=aodt_historygetnextrec(0,&historyrec); 
      if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-1 ; info : none */
      /* loop thru history file while records remain */
      while(historyrec>0) {
        /* obtain current history file record listing string */
        iok=aodt_historylistfmt(historyrec,ilisttype,atcfsrcID,stormID,listing);  /* return 0 */ 
        printf("%s",listing);
	/* obtain history file record listing in bulletin format */
        /* iok=aodt_historybullfmt(historyrec,listing);  / return 0 */
        /* printf("%s",bulletin); */

        /* send list output to ASCII output file */
        if(odump) fprintf(fpo,"%s",listing); 

        /* obtain pointer to next record in history file data structure */
        iok=aodt_historygetnextrec(1,&historyrec);
        if(iok!=0) { aodt_qmessage(iok,0,NULL,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-1 ; info : none */
      }
      aodt_qmessage(101,0,NULL,retmsg);  /* error: none ; info : 101 */
      /* close ASCII dump file if requested */
      if(odump) { fclose(fpo); aodt_qmessage(13,0,listfile,retmsg); }   /* error: none ; info : 13 */
    }
    if(ograph) {
      /* send graph options to McIDAS graphing functions */
      iok=mcidas_setgraphoptions(drawCI,drawCIadj,drawTW,drawTR,drawTRO,drawTN,drawTIEr,drawTIEa,
                                 igraph,iword,ographtemp,owind);    /* return 0 */
      iok=mcidas_graphhistoryrec( );
      if(iok!=0) { mcidas_qmessage(iok,0,hhfile,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-31,-32 ; info : 102 */
    }
    if(odel) {
      /* delete records requested within AODT library */
      iok=aodt_historydeleterec(&deleted,&modified);
      aodt_qmessage(iok,deleted,hhfile,retmsg);                  /* error: none ; info : 66 */
      if(modified>0) aodt_qmessage(65,modified,hhfile,retmsg);   /* error: none ; info : 65 */
      aodt_qmessage(103,0,NULL,retmsg);                         /* error: none ; info : 103 */
      /* write updated history structure to history file */
      iok=aodt_historywritefile(&hrecs);
      if(iok!=0) { aodt_qmessage(iok,hrecs,hhfile,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-2,-3 ; info : 67 */
    }
    if(ocommadd) {
      /* add comment to record requested within AODT library */
      iok=aodt_historyaddcomment(comment,&modified);
      if(iok!=0) { aodt_qmessage(iok,modified,hhfile,retmsg); if(iok<0) exitaodt(retmsg); }  /* error:-4 ; info : 68 */
      /* write updated history structure to history file */
      iok=aodt_historywritefile(&hrecs);
      if(iok<0) exitaodt(retmsg);   /* error:-2,-3 */
    }
    free(historyrec);
     
  }

  /* print out all diagnostic runtime messages during AODT execution */
  iok=aodt_qdiagnostics(infomsg);    /* return 0 */
  /* write output diagnostic messages to screen */
  printf("%s\n",infomsg);

  /* free any allocated memory */
  iok=aodt_freememory();
  iok=mcidas_freememory();

  /* free allocated memory */
  freevars();

  /* }  IZZ */
  return 0;
}

void exitaodt(char *errormsg)
/* Exit routine for McIDAS AODT.  This function will exit gracefully 
   from the main subroutine and output all diagnostic and error messages
   to the screen before exiting
   Inputs : error message to output to screen
   Outputs: none
*/
{
  int  iok;
  char *infomsg;

  infomsg=(char *)calloc((size_t)maxc2,sizeof(char));

  iok=aodt_qdiagnostics(infomsg);
  printf("%s\n",infomsg);
  printf("AODT Error : %s \n\n",errormsg);
  /* free any allocated memory */
  iok=aodt_freememory();
  freevars();

  free(infomsg);
  exit(0);
}

void allocvars( )
{
  /* initialize constants */
  bulletin=(char *)calloc((size_t)maxc1,sizeof(char));
  infomsg=(char *)calloc((size_t)maxc2,sizeof(char));
  listing=(char *)calloc((size_t)maxc2,sizeof(char));
  retmsg=(char *)calloc((size_t)maxc2,sizeof(char));
  comment=(char *)calloc((size_t)maxc200,sizeof(char));
  ccursat=(char *)calloc((size_t)maxc20,sizeof(char));
  version=(char *)calloc((size_t)maxc20,sizeof(char));
  scenestr=(char *)calloc((size_t)maxc20,sizeof(char));
  oscenestr=(char *)calloc((size_t)maxc20,sizeof(char));
  listfile=(char *)calloc((size_t)maxc200,sizeof(char));
  ffixfile=(char *)calloc((size_t)maxc200,sizeof(char));
  fixxfile=(char *)calloc((size_t)maxc200,sizeof(char));
  gridpath=(char *)calloc((size_t)maxc200,sizeof(char));
  lserver=(char *)calloc((size_t)maxc100,sizeof(char));
  topofile=(char *)calloc((size_t)maxc200,sizeof(char));
  sstfile=(char *)calloc((size_t)maxc200,sizeof(char));
  datafile=(char *)calloc((size_t)maxc200,sizeof(char));
  atcffile=(char *)calloc((size_t)maxc200,sizeof(char));
  atcfpath=(char *)calloc((size_t)maxc200,sizeof(char));
  stormID=(char *)calloc((size_t)maxc5,sizeof(char));
  atcfsrcID=(char *)calloc((size_t)maxc5,sizeof(char));
  od1=(char *)calloc((size_t)maxc20,sizeof(char));
  od2=(char *)calloc((size_t)maxc20,sizeof(char));
  hpath=(char *)calloc((size_t)maxc200,sizeof(char));
  tpath=(char *)calloc((size_t)maxc200,sizeof(char));
  spath=(char *)calloc((size_t)maxc200,sizeof(char));
  lpath=(char *)calloc((size_t)maxc200,sizeof(char));
  apath=(char *)calloc((size_t)maxc200,sizeof(char));
  dpath=(char *)calloc((size_t)maxc200,sizeof(char));
  hhfile=(char *)calloc((size_t)maxc200,sizeof(char));
}
                                                                                                      
void freevars()
{
  /* free character strings */
  free(version); version=NULL;
  free(retmsg); retmsg=NULL;
  free(hpath); hpath=NULL;
  free(tpath); tpath=NULL;
  free(spath); spath=NULL;
  free(lpath); lpath=NULL;
  free(apath); apath=NULL;
  free(dpath); dpath=NULL;
  free(scenestr); scenestr=NULL;
  free(oscenestr); oscenestr=NULL;
  free(comment); comment=NULL;
  free(ccursat); ccursat=NULL;
  free(bulletin); bulletin=NULL;
  free(infomsg); infomsg=NULL;
  free(listing); listing=NULL;
  free(hhfile); hhfile=NULL;
  free(listfile); listfile=NULL;
  free(ffixfile); ffixfile=NULL;
  free(fixxfile); fixxfile=NULL;
  free(gridpath); gridpath=NULL;
  free(lserver); lserver=NULL;
  free(topofile); topofile=NULL;
  free(sstfile); sstfile=NULL;
  free(datafile); datafile=NULL;
  free(atcffile); atcffile=NULL;
  free(atcfpath); atcfpath=NULL;
  free(stormID); stormID=NULL;
  free(atcfsrcID); atcfsrcID=NULL;
  free(od1); od1=NULL;
  free(od2); od2=NULL;
}
