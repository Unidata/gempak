/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"   
/* include file containing all AODT library global variables */
#include "../inc/odtlibfuncs.h"

#define histlen 181
#define histnum 38

int  mcidas_initenv( int, char ** );
int  mcidas_initialize( void );
int  mcidas_freememory( void );
int  mcidas_getcursorloc( char *,float *,float * );  
int  mcidas_getsatdatainfo( char *,int,char *,int *,int *,int * ); 
int  mcidas_getsatimagedata( char *,float,float,int,float **,float **,float **,int *,int * ); 
int  mcidas_overridescenetype( int *,int *,int *,int * );

int  mcidas_getlocalname( char * ); 
int  mcidas_getremotename( int,char *,char * ); 
int  mcidas_getdatetime( char *,int *,int *,int * ); 
int  mcidas_BTEST( int,int );
void mcidas_plotdot( int, int, int );
int  mcidas_overrideloc(void);
int  mcidas_itvxyll( int,int,int *,int *,double *,double *,int );
void aodt_xprintf( char * );

/* McIDAS FORTRAN plotting routine calls */
extern void initpl_( int*, int*);
extern void page_( int*, int*, int*, int*, int*);
extern void plot_( int*, int*, int*);
extern void wrtext_( int*, int*, int*, const char*, int*, int*, size_t);

/* defined in navcal library */
extern int ARTOEA( int, int, int , int *, int *, int *, double *, double  *);
extern int EATOAR( int, double, double, int *, int *, int * );

/* other external routines */
extern void aodt_classifyredo(void);
extern void aodt_distance( float,float,float,float,int,float *,float * );
extern void aodt_distance2( float, float, float, float, float *, float * );

char *topopath,*histpath,*sstpath,*listpath,*autopath,*datapath; 

extern char *datapath;
extern owind,ographtemp;
extern int drawCI,drawCIadj,drawTW,drawTN,drawTR,drawTRO,drawTIEa,drawTIEr,igraph,iword;

int mcidas_initenv( int argc, char **argv )
/* initialize mcidas environment
   Return : -91 : error with McIDAS initialization 
              0 : o.k.
*/
{
  int iret;

  if (Mcinit(argc, argv) < 0) {
    fprintf(stderr, "%d\n", Mciniterr());
    iret=-91;
  } else {
    iret=0;
  }
  
  return iret;
}

int mcidas_initialize( void )
{
  /* initialize path string variables */
  topopath=(char *)calloc((size_t)200,(size_t)sizeof(char));
  histpath=(char *)calloc((size_t)200,(size_t)sizeof(char));
  sstpath=(char *)calloc((size_t)200,(size_t)sizeof(char));
  listpath=(char *)calloc((size_t)200,(size_t)sizeof(char));
  autopath=(char *)calloc((size_t)200,(size_t)sizeof(char));
  datapath=(char *)calloc((size_t)200,(size_t)sizeof(char));

  return 0;
}

int mcidas_freememory( void )
{
  if(topopath != (char *)NULL) {
    free(topopath);
    topopath=NULL;                        /* added by CDB */
  }
  if(histpath != (char *)NULL) {
    free(histpath);
    histpath=NULL;                        /* added by CDB */
  }
  if(sstpath != (char *)NULL) {
    free(sstpath);
    sstpath=NULL;                        /* added by CDB */
  }
  if(listpath != (char *)NULL) {
    free(listpath);
    listpath=NULL;                        /* added by CDB */
  }
  if(autopath != (char *)NULL) {
    free(autopath);
    autopath=NULL;                        /* added by CDB */
  }
  if(datapath != (char *)NULL) {
    free(datapath);
    datapath=NULL;                        /* added by CDB */
  }

  return 0;
}

int mcidas_getcursorloc( char *datafile, float *retlat,float *retlon) 
/* Obtain cursor location utilizing McIDAS routine itvll.
    Inputs  : image file name and path
    Outputs : lat - latitude of cursor
              lon - longitude of cursor
    Return  : -22 : error with image navigation
              -23 : bad navigation in image file
	        0 : good cursor location
*/
{
  int iras,ipix,iline,iele,iok,fdir,iret;
  int navsiz,bufarr[64],iarr[mxcdsz];
  double xlat=-999.99,xlon=-999.99;

  int ilx,iex;

  /* set up (prep) navigation for image one */
  fdir=opnara(datafile, 'R');
  iok=readd(fdir,bufarr);
  navsiz=bufarr[62]-bufarr[34];
  if(bufarr[62]==0) navsiz=bufarr[33]-bufarr[34];
  iok = araget(fdir,bufarr[34],navsiz, iarr);
  if(nvprep(1,iarr)!=0) return -22 ;
  iras = Mcluc (-11);    /* obtain raster value at cursor */
  ipix = Mcluc (-12);    /* obtain pixel value at cursor */
  iok=mcidas_itvxyll(iras,ipix,&iline,&iele,&xlat,&xlon,1);
  if(iok==0) {
    *retlat=xlat;
    *retlon=xlon;
    iret=0;
  } else {
    iret=-23;
  }

  return iret;
}

int mcidas_getsatdatainfo( char *satname,int larea,char *lserver,int *rdate,int *rtime,int *rsat ) 
/* Obtain McIDAS full path and file name of satellite data.
    Inputs  : satname - satellite name and path
              larea   - local area number
	      lserver - local area McIDAS server name
    Outputs : rdate   - satellite image date
              rtime   - satellite image time
              rsat    - satellite ID
    Return  : -11 : error opening image data
              -12 : error reading image data
              -23 : bad navigation in image file
	        0 : good information
*/
{
  int iok,xdate,xtime,xsat;
  char *mcsatfile; 

  mcsatfile=(char *)calloc((size_t)200,(size_t)sizeof(char));

  if(larea>0) {
    iok=mcidas_getremotename(larea,lserver,mcsatfile);
  } else {
    iok=mcidas_getlocalname(mcsatfile);
  }
  if(iok==0) {
    strcpy(satname,mcsatfile);  
    satname[strlen(mcsatfile)]='\0';
    /* obtain sat data date/time info */
    iok=mcidas_getdatetime(satname,&xdate,&xtime,&xsat);
    *rdate=xdate;
    *rtime=xtime;
    *rsat=xsat;
  }

  free(mcsatfile);
  return iok;
}

int mcidas_getsatimagedata(char *datafile,float cenlat,float cenlon,int radius,float **temps,float **lats,float **lons,int *numx,int *numy) 
{
/* Obtains a rectangular array of data from a McIDAS area file.
   Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
   Refer to "McIDAS Software Acquisition and Distribution Policies"
   Inputs : datafile - satellite name and path
            cenlat   - center latitude location of data array
            cenlon   - center longitude location of data array
            radius   - outer radius of data read
   Output : temps    - array containing temperature grid
            lats     - array containing latitude location grid
            lons     - array containing longitude location grid
            numx     - number of points in x-axis (number of columns)
            numy     - number of points in y-axis (number of rows)
   Remarks:
        This routine does not initialize the output array.
        It does not reduce resolution.
   Return : -11 : error opening image data
            -12 : error reading image data
	    -13 : bad navigation
	    -15 : multiple banded image
	    -16 : lat/lon conversion error
	    -17 : data read off edge of image
	     12 : good read of image
*/

  int a,fstel,lstel,fstln,lstln;
  int nele,line,ele,band=0,badpoint,badline=0;
  int iex,ilx,imlin,imele;
  int ixx,iyy,fdir,kk,iok;
  int bufarr[64],*istrm;
  int navsiz,iarr[mxcdsz];
  char *iout2;
  float lres,eres,xangle;
  double xlat,xlon,xlat0,xlon0,xlat1,xlon1,xlat2,xlon2;


  /* open data file */
  fdir=opnara(datafile, 'R');
  if(fdir<=0) {
    return -11;
  }
  iok=readd(fdir,bufarr);
  if(iok!=0) return -12;

  /* set up (prep) navigation for image one */
  navsiz=bufarr[62]-bufarr[34];
  if(bufarr[62]==0) navsiz=bufarr[33]-bufarr[34];
  iok = araget(fdir,bufarr[34],navsiz, iarr);
  if(nvprep(1,iarr)!=0) return -13 ;
  /* end navigation setup */

  /* check to make sure image contains only one band */
  if(bufarr[13]>1) {
    return -15;
  }

  /* determine band number of image */
  while((mcidas_BTEST(bufarr[18],band))!=1) { band++; }

  /* convert latitude/longitude to image line/element coordinates */
  if(EATOAR(fdir,(double)cenlat,(double)cenlon,bufarr,&imlin,&imele)!=0) {
    return -16;
  }

  /* determine line and element resolution for data box read */
  /* this ARTOEA is more for the X-AODT to assure that the lat/lon value for the image
   * line and element are the actual values for the point, not the user input values
   * for the image read */
  ARTOEA(fdir,imlin,imele,(int *)bufarr,&ilx,&iex,&xlat0,&xlon0);
  ARTOEA(fdir,imlin+1,imele,(int *)bufarr,&ilx,&iex,&xlat1,&xlon1);
  ARTOEA(fdir,imlin,imele+1,(int *)bufarr,&ilx,&iex,&xlat2,&xlon2);
  aodt_distance(xlat1,xlon1,xlat0,xlon0,1,&lres,&xangle);
  aodt_distance(xlat2,xlon2,xlat0,xlon0,1,&eres,&xangle);

  /* define image box boundaries */
  fstln=imlin-(int)((float)radius/lres)-5;
  lstln=imlin+(int)((float)radius/lres)+5;
  fstel=imele-(int)((float)radius/eres)-5;
  lstel=imele+(int)((float)radius/eres)+5;

  /* make sure data box is not off upper left part of image */
  if(fstel<0||fstln<0)
  {    /* first element is negative (left of the image) */
    return -17;
  } else {
    nele=lstel-fstel;  /* total number of elements in each line */
  }
  if((lstln-fstln)>maxd) {
    /* too many lines image read... will reduce number of lines */
    ixx=((lstln-fstln)-maxd)/2;
    fstln=fstln+ixx+1;
    lstln=lstln-ixx-1;
  }
  if((lstel-fstel)>maxd) {
    /* too many elements image read... will reduce number of elements */
    ixx=((lstel-fstel)-maxd)/2;
    fstel=fstel+ixx+1;
    lstel=lstel-ixx-1;
  }

  /* allocate memory */
  a=(size_t)sizeof(int);
  istrm=(int *)calloc((size_t)500000,a);
  iout2=(char *)calloc((size_t)5000,(size_t)sizeof(char)); 

  /* read nele elements for each line of data */
  ARABOX(fdir,fstln,lstln,fstel,lstel,band,"TEMP",4,nele,(long *)istrm);

  for(line=fstln;line<=lstln;line++) {
    iyy=line-fstln;
    kk=(iyy*nele);
    badpoint=0;
    for(ixx=0;ixx<nele;ixx++) {
      ele=fstel+ixx;
      /* save temperature value to array */
      temps[iyy][ixx]=(float)istrm[kk]/10.0;
      if((temps[iyy][ixx]>=320.0)||(temps[iyy][ixx]<=120.0)) {
        /* data at point is bad (>47C or <-153C).  Count number
           of bad points in line... if there are more than 10 bad 
           points, replace entire line with previous line */
        badpoint++;
        if(ixx>0) temps[iyy][ixx]=temps[iyy][ixx-1];
      }
      /* convert line/element to latitude/longitude */
      ARTOEA(fdir,line,ele,(int *)bufarr,&ilx,&iex,&xlat,&xlon);
      /* save latitude/longitude location to arrays */
      lats[iyy][ixx]=(float)xlat;
      lons[iyy][ixx]=(float)xlon;
      ++kk;
    }
    if((badpoint>10)&&(iyy>0)) {
      /* data line in image is bad... replace with previous */
      sprintf(iout2,"BAD DATA POINTS DETECTED AT LINE %d, REPLACING WITH PREVIOUS LINE\n",line); strcat(diagnostics,iout2);
      for(ixx=0;ixx<nele;ixx++) {
        temps[iyy][ixx]=temps[iyy-1][ixx];
      }
      badline++;
      if(badline>=10) {
        sprintf(iout2,"BAD IMAGE... EXITING\n",line); strcat(diagnostics,iout2);
        free(istrm);
        free(iout2);
        iok = clsara(fdir);
        return -17;
      }
    }
  }

  /* free memory */
  free(istrm);
  free(iout2);

  iok = clsara(fdir);
  /* calculate total number of lines/elements */
  *numx=lstel-fstel; /* # columns in temps array */
  *numy=lstln-fstln; /* # rows in temps array */

  return 12;
}

int mcidas_overridescenetype(int *origeye,int *origcloud,int *neweye,int *newcloud)
/* Utilize mouse button press to toggle YES/NO
   agreement with SCENE TYPE determination.
    Inputs  : none
    Outputs : neweye/cloud - new eye/cloud scene type flag value (or -1 if unchanged)
              origeye/cloud - old eye/cloud scene type flag value (or orig value if unchanged)
    Return  : -92 : error with mouse button inputs
               31 : accepted scene type
               32 : modified scene type
*/
{
   int iok,button1,button2,itvlin,itvele,istat,maxscene,maxsceneinit;
   int oldscene,sceneinit,escenei,cscenei,esceneo,csceneo,iret;
   char *iout2;
   logical quit=FALSE;

   iout2=(char *)calloc((size_t)5000,(size_t)sizeof(char)); 

   escenei=odtcurrent->IR.eyescene;
   cscenei=odtcurrent->IR.cloudscene;
   csceneo=cscenei;
   esceneo=escenei;

   if((escenei<6)&&(cscenei<2)) {
     sprintf(iout2,"AODT has classified the EYE SCENE as %s\n",eyetype[escenei]);aodt_xprintf(iout2); 
     sprintf(iout2,"Do you agree with this scene classification?\n");aodt_xprintf(iout2); 
     sceneinit=escenei;
     maxscene=6;
   } else {
     sprintf(iout2,"AODT has classified the CLOUD SCENE as %s\n",cloudtype[cscenei]);aodt_xprintf(iout2); 
     sprintf(iout2,"Do you agree with this scene classification?\n");aodt_xprintf(iout2); 
     sceneinit=cscenei;
     maxscene=5;
   }

   sprintf(iout2,"TOGGLE : Press MIDDLE mouse button\n");aodt_xprintf(iout2);
   sprintf(iout2,"ACCEPT : Press RIGHT  mouse button\n");aodt_xprintf(iout2);

   istat= Mcmoubtn(3, &button1, &button2, &itvlin, &itvele);
   if(istat!=0) return -92;
   if(button1==3) {
     oldscene=sceneinit;
     /* buttons : left,center=1, right button=2 */
     while(quit==FALSE) {
       if(button1 != 0) {
         sceneinit++;
         if(sceneinit > maxscene) {
           sceneinit = 0;
         }
         if(maxscene==6) {
           sprintf(iout2,"Change eye scene to %s\n",eyetype[sceneinit]);aodt_xprintf(iout2);
         } else {
           sprintf(iout2,"Change cloud scene to %s\n",cloudtype[sceneinit]);aodt_xprintf(iout2);
         }
       }
       istat= Mcmoubtn(3, &button1, &button2, &itvlin, &itvele);
       if(istat!=0) {
         free(iout2);
         iout2=NULL;
         return -92;
       }
       if(button2!=0) {
         if(maxscene==6) {    /* eye scene */
           if (sceneinit==6) {
             /* changed from eye scene to cloud scene */
             sceneinit=-1;
             maxscene=5;
             /* esceneo=escenei; */
             esceneo=6;  /* no eye */
             button1=1;
           } else {
             quit=TRUE;
             esceneo=sceneinit;
           }
         } else {
           if (sceneinit==5) {   /* cloud scene */
             /* changed from cloud scene to eye scene */
             sceneinit=-1;
             maxscene=6;
             csceneo=cscenei;
             button1=1;
           } else {
             quit=TRUE;
             csceneo=sceneinit;
           }
         }
       }
     }
   }
   if(cscenei != csceneo) {
     sprintf(iout2,"CLOUD SCENE has been changed to %s\n",cloudtype[csceneo]);strcat(diagnostics,iout2);
     *neweye=6;
     *newcloud=csceneo;
     *origeye=escenei;
     *origcloud=cscenei;
     odtcurrent->IR.cloudscene=csceneo;
     if((csceneo==3)||(csceneo==4)) {
       aodt_classifyredo( );
     }
     iret=32;
   } else if(escenei != esceneo) {
     sprintf(iout2,"EYE SCENE has been changed to %s\n",eyetype[esceneo]);strcat(diagnostics,iout2);
     *neweye=esceneo;
     *newcloud=A_MIN(cscenei,1);
     *origeye=escenei;
     *origcloud=cscenei;
     iret=32;
   } else {
     sprintf(iout2,"CLOUD AND EYE SCENES have not been changed\n");strcat(diagnostics,iout2);
     *neweye=-1;
     *newcloud=-1;
     *origeye=escenei;
     *origcloud=cscenei;
     iret=31;
   }

   free(iout2);
   iout2=NULL;
   return iret;

}

/*-----------------------------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------------*/

int mcidas_BTEST(int i, int pos)
/* Tests if a bit in a word is set.
   Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
   Refer to "McIDAS Software Acquisition and Distribution Policies"
   Inputs  : i   - Word containing bit to be tested
             pos - Position of bit being tested (0-based)
   Outputs : return value (1 if bit is set,0 if bit is not set)
*/
{
        pos = pos - 1;
        return ((i) & (1 << pos)) ? 1 : 0;
}

int mcidas_graphhistoryrec( )
/* Graph the history file between given date/times
    Inputs  : none
    Outputs : none
    Return  : -32 - error opening history file
              -31 - error reading history file
              102 - successful graphing
*/
{
  FILE *fp;
  int  maxline=200,lwidth=2,hfilelen,bytes,iok;
  int  const0=0,const1=1,const2=2,const5=5;
  int  const6=6,const7=7,const9=9,const11=11,const50=50,const80=80;
  int  it1,it2,it3,it4,it5,it2lastT,it2lastD;
  int  date,time,scene,r9,r8,rlb,land,csat;
  int  ifrm,ilin,iele,lb,le,eb,ee,ia,ib,ic,id,ie,ig,ih,ii,npts;
  int  cnt,isub,dlen,yax1,xax1,ihour,idd,im,iy,ixx,idraw;
  int  esceneo,csceneo,cwring,apos,plotvalx1,plotvalx2;
  int  escene,cscene,efft,cfft,rcb,rcbv,tief;
  int  ix[1000],iy1[1000],iy2[1000],iy3[1000],iy4[1000],iy5[1000],iy6[1000],iy7[1000],iy8[1000];
  float gwidth,gheight,theight,tval,tvalB,tvalE,tvalint;
  float rawt,rawto,finalt,f3t,ci,eye,cloud,cloud2,cw,lat,lon,wval,pval,fval;
  float esize,estdv,csym,ciadjp,tier,tiea,sst,rmw,cip;
  float mint=100.0,maxt=-100.0;
  double xbeg,xend,xtime,xspace,juldate;
  double curtime,xtimediff,buf[15][1000];
  char line[maxline],ca[8],cb[8],cc[8],cd[8],ce[8],cg[8],ch[8],cax[8];
  char ctno[12],chour[12],cdate[12],cd1[12],cdate1[12],cwp[12],comm[50];
  int  padj[2][15]={ {0,32,30,28,26,24,20,18,16,14,12,10,8,6,4},
                      {0,42,38,32,26,28,26,24,20,16,14,12,6,6,4} };
  int  wadj[15]={0,30,30,26,24,26,24,26,24,20,20,20,10,10,0};
  int  pstart[2]={8900,8580};
  char leftlabel[12]="T NUMBER   ";
  char rightlabel[3][12]={"TEMPERATURE","WIND SPEED ","PRESSURE   "};
  int  rightflag;
  logical TIEavl=FALSE,firstrec=TRUE;

  /* get file information */
  fp=fopen(hfile,"r+");
  if(fp==0) {
    return -32;
  }

  /* read history file, quit at EOF */
  npts=-1;
  while(fgets(line,maxline,fp) != NULL) {
    iok=sscanf(line,"%s %d %lf %f %f %f %f %f %f %f %f %f %f %f %f %f %f %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %f %d %f %f %f %f %50c%n",
                 cdate,&time,&juldate,
                 &rawto,&rawt,&finalt,&f3t,&ci,&eye,&cloud,&cloud2,&cw,&lat,&lon,&esize,&estdv,&csym,
                 &csat,&escene,&cscene,&esceneo,&csceneo,&r9,&r8,&rlb,&land,&efft,&cfft,
                 &rcb,&rcbv,&cwring,&apos,&ciadjp,&tief,&tier,&tiea,&sst,&rmw,comm,&bytes);

    if(iok<histnum) {
      return -31;
    } else if (iok==histnum) {
      comm[0]='\0';
    } else {
      comm[bytes-histlen-1]='\0';
    }

    if((rawt>=1.0)||(land==2)) {
      date=aodt_cmonth2julian(cdate);
      curtime=aodt_calctime(date,time);
      if((curtime>=starttime)&&(curtime<=endtime)) {
        npts++;
        buf[0][npts]=curtime;
        buf[1][npts]=rawt;
        buf[2][npts]=finalt;
        buf[3][npts]=ci;
        buf[4][npts]=eye;
        buf[5][npts]=cloud;
        buf[6][npts]=cw;
        buf[7][npts]=f3t;
        if(tief>0) {
          TIEavl=TRUE;
          buf[8][npts]=aodt_ptotno(tiea);
          buf[9][npts]=aodt_ptotno(tier);
        } else {
          buf[8][npts]=-99.0;
          buf[9][npts]=-99.0;
        }
        if(ixdomain==0&&firstrec) {
          idomain=0;
          if(lon<0.0) idomain=1;
          firstrec=FALSE;
        }
        cip=aodt_getpwval(0,ci);
        buf[10][npts]=aodt_ptotno(cip+ciadjp);
        buf[11][npts]=rawto;

        if(eye>maxt) maxt=eye;
        if(cloud<mint) mint=cloud;

      }
    }
  }
  if(!TIEavl) {
    drawTIEr=0;
    drawTIEa=0;
  }
  fclose(fp);
  
  if(endtime>=9999999.) endtime=curtime;
  xtimediff=endtime-starttime;

  /* initial graphic, normal line width */
  ifrm=Mcluc(51);
  Mcfsize(ifrm,&ilin,&iele);

  /* set up graphic boundary
    80,80           - upper left
    iele-80,ilin-80 - lower right */
  lb=80;
  eb=80;
  le=ilin-lb;
  ee=iele-eb;
  
  /* draw bounding box for graph */
  initpl_(&ifrm,&lwidth);
  page_(&const0,&const0,&ilin,&iele,&const0);
  plot_(&lb,&eb,&const0);
  plot_(&le,&eb,&igraph);
  plot_(&lb,&eb,&const0);
  plot_(&lb,&ee,&igraph);
  plot_(&le,&ee,&const0);
  plot_(&le,&eb,&igraph);
  plot_(&le,&ee,&const0);
  plot_(&lb,&ee,&igraph);

  /* set absolute coordinates (width and height)
     gwidth and gheight = graph width and height
     theight = height (in CI or temperature) of y-axis */
  gwidth=(float)(ee-eb);
  gheight=(float)(le-lb);
  if(ographtemp) {
    /* graphing temperature */
    ia=4;
    ib=5;
    ic=6;
    id=0;
    ie=0;
    ig=0;
    ih=0;
    strncpy(ca,"Eye T  ",7);
    strncpy(cb,"Cld T  ",7);
    strncpy(cc,"C-W T  ",7);
    tvalB=(float)(10*(int)((maxt+10.0)/10.0));
    tvalE=(float)(10*(int)((mint-10.0)/10.0));
    theight=tvalB-tvalE;
    tvalint=-(theight/50.0);
  } else {
    /* graphing Tno/CI values */
    ia=1;
    ib=2;
    ic=3;
    id=7;
    ie=8;
    ig=9;
    ih=10;
    ii=11;
    strncpy(ca,"AdjT#  ",7);
    strncpy(cb,"FinalT#",7);
    strncpy(cc,"CI#    ",7);
    strncpy(cd,"T#-3hr ",7);
    strncpy(ce,"TIEavg ",7);
    strncpy(cg,"TIEraw ",7);
    strncpy(ch,"adjMSLP",7);
    strncpy(cax,"RawT#  ",7);
    tvalB=8.0;
    tvalE=1.0;
    theight=7.0;
    tvalint=-0.1;
  }

  /* determine tick marks for x-axis and y-axis */

  cnt=0;
  isub=0;
  pval=pstart[idomain];
  wval=1700;
  /* Y-AXIS/Left Side (T-No. or Temp (C)) */
  /* Y-AXIS/Right Side (wind/pressure scale or Temp (F)) */
  for(tval=tvalB;tval>=tvalE;tval=tval+tvalint) {
    dlen=5; 
    if(cnt==5) {
      dlen=10;
      cnt=0;
    }
    cnt++;
    yax1=lb+(int)(((tval-tvalE)/theight)*gheight);

    /* left side scale */
    it1=eb+1;
    it2=eb+dlen+1;
    it3=ee-1;
    it4=ee-dlen-1;
    plot_(&yax1,&it1,&const0);
    plot_(&yax1,&it2,&igraph);
    plot_(&yax1,&it3,&const0);
    plot_(&yax1,&it4,&igraph);
    /* compute wind speed */
    wval=wval-wadj[isub];
    /* compute pressure */
    pval=pval+padj[idomain][isub];
    if(cnt==1) {
      isub++;
      sprintf(ctno,"%6.1f",tval);
      it1=yax1-5;
      it2=eb-65;
      /* right side value plot determination */
      if(ographtemp) {
        fval=(1.8*tval)+32.0;
        sprintf(cwp,"%6.1f",fval);
        it4=ee+5;
        rightflag=0;
      } else {
        if(owind) {
          sprintf(cwp,"%6d",(int)wval/10);
          it4=ee-10;
          rightflag=1;
        } else {
          sprintf(cwp,"%6d",(int)pval/10);
          it4=ee-5;
          rightflag=2;
        }
      }
      /* left side value plot */
      plotvalx1=(drawCI>0) ? drawCI : iword;
      wrtext_(&it1,&it2,&const9,ctno,&const6,&plotvalx1,(size_t)6);
      /* right side value plot */
      plotvalx2=(drawCIadj>0) ? drawCIadj : iword;
      wrtext_(&it1,&it4,&const9,cwp,&const6,&plotvalx2,(size_t)6);
    }
  }
  it1=eb-60;              /* left side element */
  it3=ee+60;              /* right side element */
  it2=((lb+le)/2)-50;     /* line starting point */
  for(ixx=0;ixx<12;ixx++) {
    it4=it2+(ixx*13);
    wrtext_(&it4,&it1,&const9,&leftlabel[ixx],&const1,&iword,(size_t)6);
    wrtext_(&it4,&it3,&const9,&rightlabel[rightflag][ixx],&const1,&iword,(size_t)6);
  }

  /* X-AXIS/Bottom */
  xbeg=(double)(int)starttime;
  xend=(double)(int)(endtime+1.0);
  if(xtimediff<1.0) {
    /* hourly tick marks */
    xspace=(double)0.0416;
  } else if(xtimediff<2.0) {
    /* three-hour tick marks */
    xspace=(double)0.125;
  } else if(xtimediff<8.0) {
    /* six-hour tick marks */
    xspace=(double)0.250;
  } else {
    /* twelve-hour tick marks */
    xspace=(double)0.500;
  }
  it2lastT=0;
  it2lastD=0;
  for(xtime=xbeg;xtime<=xend;xtime=xtime+xspace) {
    dlen=5;
    ihour=(int)(24.0*(xtime-(double)(int)xtime));
    if(ihour==00) dlen=10;
    if((ihour==12)&&(xtimediff<1.0)) dlen=10;
    xax1=eb+(int)(((xtime-xbeg)/(xend-xbeg))*gwidth);
    it1=lb+1;
    it2=lb+dlen+1;
    it3=le;
    it4=le-dlen;
    plot_(&it1,&xax1,&const0);
    plot_(&it2,&xax1,&igraph);
    plot_(&it3,&xax1,&const0);
    plot_(&it4,&xax1,&igraph);
    ihour=(int)(24.0*(xtime-(double)(int)xtime));
    sprintf(chour,"%2.2d",ihour);
    it1=le+15;
    it2=xax1-8;
    if(it2>it2lastT) {
      wrtext_(&it1,&it2,&const9,chour,&const2,&iword,(size_t)2);
      it2lastT=it2+20;
    }
    if(dlen==10) {
      sprintf(cdate,"%7.7d",(int)xtime);
      (void)aodt_yddmy((int)xtime,&idd,&im,&iy);
      sprintf(cd1,"%2.2d",idd);
      sprintf(cdate1,"%s%s",cmon[im-1],cd1);
      it1=le+30;
      it2=xax1-22;
      it3=lb-20;
      if(it2>it2lastD) {
        wrtext_(&it1,&it2,&const7,cdate,&const7,&iword,(size_t)7);
        wrtext_(&it3,&it2,&const9,cdate1,&const5,&iword,(size_t)5);
        it2lastD=it2+50;
      }
    }
    it1=le+15;
    it2=ee+10;
    wrtext_(&it1,&it2,&const9,"(UTC)",&const5,&iword,(size_t)5);
  }

  /* determine TV plotting location for each data point */
  for(ixx=0;ixx<=npts;ixx++) {
    ix[ixx]=eb+(int)(((buf[0][ixx]-xbeg)/(xend-xbeg))*gwidth);
    iy1[ixx]=lb+(int)(((buf[ia][ixx]-tvalE)/theight)*gheight);
    iy2[ixx]=lb+(int)(((buf[ib][ixx]-tvalE)/theight)*gheight);
    iy3[ixx]=lb+(int)(((buf[ic][ixx]-tvalE)/theight)*gheight);
    if(id>0) iy4[ixx]=lb+(int)(((buf[id][ixx]-tvalE)/theight)*gheight);
    if(ih>0) iy7[ixx]=lb+(int)(((buf[ih][ixx]-tvalE)/theight)*gheight);
    if(ii>0) iy8[ixx]=lb+(int)(((buf[ii][ixx]-tvalE)/theight)*gheight);
    if((ie>0)&&(buf[ie][ixx]>0)&&(drawTIEa)) {
      iy5[ixx]=lb+(int)(((buf[ie][ixx]-tvalE)/theight)*gheight);
      iy6[ixx]=lb+(int)(((buf[ig][ixx]-tvalE)/theight)*gheight);
    } else {
      iy5[ixx]=-99;
      iy6[ixx]=-99;
    }
  }

  /* plot lines of intensity estimates/temperatures */
  for(ixx=0;ixx<npts;ixx++) {
    if(drawTR>0) {
      plot_(&iy1[ixx],&ix[ixx],&const0);
      plot_(&iy1[ixx+1],&ix[ixx+1],&drawTR);
    }
    if(drawTRO>0) {
      plot_(&iy8[ixx],&ix[ixx],&const0);
      plot_(&iy8[ixx+1],&ix[ixx+1],&drawTRO);
    }
    if(drawTW>0) {
      plot_(&iy2[ixx],&ix[ixx],&const0);
      plot_(&iy2[ixx+1],&ix[ixx+1],&drawTW);
    }
    if((ih>0)&&(drawCIadj>0)) {
      plot_(&iy7[ixx],&ix[ixx],&const0);
      plot_(&iy7[ixx+1],&ix[ixx+1],&drawCIadj);
    }
    if(drawCI>0) {
      plot_(&iy3[ixx],&ix[ixx],&const0);
      plot_(&iy3[ixx+1],&ix[ixx+1],&drawCI);
    }
    if((id>0)&&(drawTN>0)) {
      plot_(&iy4[ixx],&ix[ixx],&const0);
      plot_(&iy4[ixx+1],&ix[ixx+1],&drawTN);
    }
    if((ie>0)&&(drawTIEa>0)) {
      if((iy5[ixx]>0)&&(iy5[ixx+1]>0)) {
        plot_(&iy5[ixx],&ix[ixx],&const0);
        plot_(&iy5[ixx+1],&ix[ixx+1],&drawTIEa);
        plot_(&iy6[ixx],&ix[ixx],&const0);
        plot_(&iy6[ixx+1],&ix[ixx+1],&drawTIEr);
      } 
    }
  }

  /* plot points at data point locations */
  for(ixx=0;ixx<=npts;ixx++) {
    if(drawTR>0) mcidas_plotdot(iy1[ixx],ix[ixx],const7);
    if(drawTRO>0) mcidas_plotdot(iy8[ixx],ix[ixx],const7);
    if(drawTW>0) mcidas_plotdot(iy2[ixx],ix[ixx],const7);
    if(drawCI>0) mcidas_plotdot(iy3[ixx],ix[ixx],const7);
    if((id>0)&&(drawTN>0)) mcidas_plotdot(iy4[ixx],ix[ixx],const7);
    if((ih>0)&&(drawCIadj>0)) mcidas_plotdot(iy7[ixx],ix[ixx],const7);
    if((ie>0)&&(iy5[ixx]>0)&&(drawTIEa>0)) mcidas_plotdot(iy5[ixx],ix[ixx],const7);
    if((ig>0)&&(iy6[ixx]>0)&&(drawTIEr>0)) mcidas_plotdot(iy6[ixx],ix[ixx],const7);
  }
    
  /* draw legend at top of graph */
  it1=10;
  wrtext_(&it1,&eb,&const11,"UW-CIMSS AODT Tropical Cyclone Intensity Estimate",&const50,&iword,(size_t)50);
  it1=35;
  it2=39;
  wrtext_(&it1,&eb,&const9,"Legend : ",&const9,&iword,(size_t)9);
  idraw=1;
  it3=eb+(100*idraw);
  it4=eb+(100*idraw)+20;
  it5=eb+(100*idraw)+25;
  if(drawTR>0) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawTR);
    wrtext_(&it1,&it5,&const9,ca,&const7,&iword,(size_t)7);
  }
  it3=eb+(100*idraw);
  it4=eb+(100*idraw)+20;
  it5=eb+(100*idraw)+25;
  if(drawTRO>0) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawTRO);
    wrtext_(&it1,&it5,&const9,cax,&const7,&iword,(size_t)7);
  }
  it3=eb+(100*idraw);
  it4=eb+(100*idraw)+20;
  it5=eb+(100*idraw)+25;
  if(drawTW>0) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawTW);
    wrtext_(&it1,&it5,&const9,cb,&const7,&iword,(size_t)7);
  }
  it3=eb+(100*idraw);
  it4=eb+(100*idraw)+20;
  it5=eb+(100*idraw)+25;
  if(drawCI>0) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawCI);
    wrtext_(&it1,&it5,&const9,cc,&const7,&iword,(size_t)7);
  }
  it3=eb+(100*idraw);
  it4=eb+(100*idraw)+20;
  it5=eb+(100*idraw)+25;
  if((ih>0)&&(drawCIadj>0)) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawCIadj);
    wrtext_(&it1,&it5,&const9,ch,&const7,&iword,(size_t)7);
  }
  it3=eb+(100*idraw);
  it4=eb+(100*idraw)+20;
  it5=eb+(100*idraw)+25;
  if((id>0)&&(drawTN>0)) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawTN);
    wrtext_(&it1,&it5,&const9,cd,&const7,&iword,(size_t)7);
  }
  it3=eb+(100*idraw);
  it4=eb+(100*idraw)+20;
  it5=eb+(100*idraw)+25;
  if((ie>0)&&(drawTIEa>0)) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawTIEa);
    wrtext_(&it1,&it5,&const9,ce,&const7,&iword,(size_t)7);
  }
  it3=eb+(100*idraw);
  it4=eb+(100*idraw)+20;
  it5=eb+(100*idraw)+25;
  if((ig>0)&&(drawTIEr>0)) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawTIEr);
    wrtext_(&it1,&it5,&const9,cg,&const7,&iword,(size_t)7);
  }

  /* stamp history file name on bottom of graph */
  it1=le+60;
  it2=10;
  hfilelen=strlen(hfile);
  /* wrtext_(&it1,&it2,&const9,hfile,&hfilelen,&const1,(size_t)strlen(hfile)); */

  endplt_();

  return 102;
}

void mcidas_plotdot(int y,int x,int color)
{
  int const0=0,const1=color;

  plot_(&y,&x,&const0);
  plot_(&y,&x,&const1);

}

int mcidas_getdatetime(char *satfile,int *date,int *time,int *rsat) 
/* obtain the date/time of the current image.
    Inputs  : satfile - satellite file name and path
    Outputs : date    - image date (Julian date)
              time    - image time (HHMMSS format)
              rsat    - satellite ID (1=GOES9,10=MET7,etc)
    Return : -11 : error opening image data
             -12 : error reading image data
	       0 : good image read
*/
{
  int fdir,iok,isat,bufarr[64];

  fdir=opnara(satfile, 'R');
  if(fdir<=0) {
    return -11;
  }

  iok=readd(fdir,bufarr);
  if(iok!=0) return -12;
  /* obtain date/time */
  isat=bufarr[2];
  *date=bufarr[3];
  *time=bufarr[4];
  if(isat==70) *rsat= 0; /* strcpy(csat," GOES8"); */
  if(isat==72) *rsat= 1; /* strcpy(csat," GOES9"); */
  if(isat==74) *rsat= 2; /* strcpy(csat,"GOES10"); */
  if(isat==76) *rsat= 3; /* strcpy(csat,"GOES11"); */
  if(isat==78) *rsat= 4; /* strcpy(csat,"GOES12"); */
  if(isat==83) *rsat= 5; /* strcpy(csat,"  GMS5"); */
  if(isat==84) *rsat= 6; /* strcpy(csat,"MTSAT1"); */
  if(isat==84) *rsat= 7; /* strcpy(csat,"MTSAT2"); */
  if(isat==56) *rsat= 8; /* strcpy(csat,"  MET5"); */
  if(isat==57) *rsat= 9; /* strcpy(csat,"  MET6"); */
  if(isat==58) *rsat=10; /* strcpy(csat,"  MET7"); */
  if(isat==51) *rsat=11; /* strcpy(csat,"  MSG1"); */
  if(isat==52) *rsat=12; /* strcpy(csat,"  MSG2"); */
  if(isat==53) *rsat=13; /* strcpy(csat,"  MSG3"); */
  if(isat==34) *rsat=14; /* strcpy(csat,"  FY2B"); */
  if(isat==35) *rsat=15; /* strcpy(csat,"  FY2C"); */
  if(isat==36) *rsat=16; /* strcpy(csat,"  FY2D"); */

  clsara(fdir);
  return 0;
}

int mcidas_getlocalname( char *satname ) 
/* Obtain satellite name via McIDAS routine.
    Inputs  : none
    Outputs : return value will be image name
*/
{
  char fname[64],*ptr;
  int fdir[64],frame,iok;
  int xdate,xtime;
 
  frame=Mcluc(51);
  getfrm_(&frame, fdir);
  sprintf(fname,"AREA%4d\n",fdir[61]);
  ptr=strrchr(fname,' ');
  while(ptr!=NULL) {
    *ptr='0';
    ptr=strrchr(fname,' ');
  }
  strcpy(satname,datapath); 
  strncat(satname,fname,strlen(fname));
  satname[strlen(satname)-1]='\0';

  return 0;

}

int mcidas_getremotename( int larea,char *lserver,char *satname ) 
{
  int  iok,len,x,y,il,ie,fdir[64],frame,xdate,xtime;
  char *name,*llpos,*cband;
  char *command,*cname,*carea,*fname;
  char *ptr,*iout2;
  double dlat,dlon;

  /* get line/element info from remote data file */
  y=Mcluc(-12);
  x=Mcluc(-11);
  frame=Mcluc(51);
  getfrm_(&frame, fdir);

  /* convert line/element to lat/lon */
  iok=mcidas_itvxyll(x,y,&il,&ie,&dlat,&dlon,2);
  if(iok!=0) return -23;

  cband=(char *)calloc((size_t)20,(size_t)sizeof(char));
  name=(char *)calloc((size_t)25,(size_t)sizeof(char));
  llpos=(char *)calloc((size_t)40,(size_t)sizeof(char));
  cname=(char *)calloc((size_t)50,(size_t)sizeof(char));
  fname=(char *)calloc((size_t)64,(size_t)sizeof(char));
  carea=(char *)calloc((size_t)100,(size_t)sizeof(char));
  command=(char *)calloc((size_t)200,(size_t)sizeof(char));
  iout2=(char *)calloc((size_t)5000,(size_t)sizeof(char)); 

  strncpy(name, &fdir[55], 24); 
  ptr=strchr(name,' ');
  *ptr='\0';

  /* form IMGCOPY command for Mcskeyin command */
  strcpy (command,"IMGCOPY");
  sprintf(cname," %s.%d",name,fdir[61]);
  strcat (command,cname);
  sprintf (carea," %s.%d",lserver,larea);
  strcat (command,carea);
  strcat (command," LINELE=");
  sprintf(llpos,"%d %d I",il,ie);
  strcat (command,llpos);
  sprintf(cband," PLACE=CENTER BAND=%d",fdir[3]);
  strcat (command,cband);
  strcat (command," DEV=NNN");

  /* form local data name string variable */
  sprintf(fname,"AREA%4d\n",larea);
  ptr=strrchr(fname,' ');
  while(ptr!=NULL) {
    *ptr='0';
    ptr=strrchr(fname,' ');
  }

  strcpy(satname,datapath); 
  strncat(satname,fname,strlen(fname));
  satname[strlen(satname)-1]='\0';

  sprintf(iout2,"DOWNLOADING REMOTE IMAGE%s TO LOCAL IMAGE%s\n",cname,carea); strcat(diagnostics,iout2);
  sprintf(iout2," WITHIN LOCAL DIRECTORY AT %s\n",satname); strcat(diagnostics,iout2);

  /* McIDAS command to download area from remote to local server */
  iok=Mcskeyin(command);

  free(command);
  command=NULL;
  free(name);
  name=NULL;
  free(llpos);
  llpos=NULL;
  free(cband);
  cband=NULL;
  free(cname);
  cname=NULL;
  free(carea);
  carea=NULL;
  free(fname);
  fname=NULL;
  free(iout2);
  iout2=NULL;

  return 0;
}

int mcidas_overrideloc(void)
/* Utilize mouse button press to toggle YES/NO
   agreement with AUTO FIX position location.
    Inputs  : none
    Outputs : function return value
              1=use Auto Fix position,0=use User Fix position
*/
{
   int istat,iret,button1,button2,itvlin,itvele;
   char *iout2;

   iout2=(char *)calloc((size_t)5000,(size_t)sizeof(char)); 

   sprintf(iout2,"Do you agree with this position?\n");aodt_xprintf(iout2);
   sprintf(iout2,"YES: Press RIGHT mouse button\n");aodt_xprintf(iout2);
   sprintf(iout2,"NO : Position cursor at desired location\n");aodt_xprintf(iout2); 
   sprintf(iout2,"     and press MIDDLE mouse button\n");aodt_xprintf(iout2);

   istat= Mcmoubtn(3, &button1, &button2, &itvlin, &itvele);
   if(istat!=0) {
     free(iout2);
     iout2=NULL;
     return 1;
   }

   /* buttons : left,center=1, right button=2 */
   if (button2 != 0) {
     iret = 1;
   } else if (button1 != 0) {
     iret = 0;
   }

   free(iout2);
   iout2=NULL;
   return iret;
}


int mcidas_itvxyll(int iras,int ipix,int *iline,int *iele,double *dlat,double *dlon,int iflag)
{
  int iok,iframe,magElem,frmdir[64];
  double xlat,xlon,dumb;

  xlat=0.0;
  xlon=0.0;
  iframe=Mcluc(51);
  getfrm_(&iframe, frmdir);
  /* end navigation setup */
  /* calculate satellite line/element */
  magElem =frmdir[32];
  if (magElem <= 0) {
    magElem = frmdir[10];
  }
  *iline = frmdir[4] + frmdir[10] * (iras - frmdir[7]) / frmdir[9];
  *iele = frmdir[5] + frmdir[11] * (ipix - frmdir[8]) / magElem;
  if(iflag==2) return 0;
  iok=nv1sae((double)*iline,(double)*iele,0.,&xlat,&xlon,&dumb);
  /* bad nav : iok=-1 */
  *dlat=xlat;
  *dlon=xlon;
  return iok;
}

void aodt_xprintf(char *cout)
/* Print message to McIDAS text window.
*/
{
  Mcprintf("%s",cout);
}
