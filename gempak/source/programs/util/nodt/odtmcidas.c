#include "inc/odt.h"

/* extern global variables */
extern int kres,larea;
extern int igraph,drawTR,drawTW,drawCI,iword,ograph,owind,idomain;
extern char scenetype[10][6],hfile[200],lserver[100];
extern logical oremote,ographtemp;
extern char hfile[200],iout[200];

int  getdatetime( char *,int *,int * );
int  readsatdata( char *,float,float,int,float [maxd][maxd],float [maxd][maxd],float [maxd][maxd],int *,int * );
int  BTEST( int,int );
char *getsatname();
int  getcursorloc( float *,float * );
int  graphhistoryrec( double, double );
void plotdot( int, int, int );
int  overrideloc( logical );
int  overridescene( logical,int *);
char *odtremote( void );

extern double calctime( int, int );
extern int ARTOEA( int, int, int , int *, int *, int *, double *, double  *);
extern int EATOAR( int, double, double, int *, int *, int * );
extern int IMTOAR( int, int, int *, int *, int * );
extern int ARTOIM( int *, int *, int *, int, int );
extern char *ckwp( char *,int,char * );
extern int ikwp( char *,int,int );
extern void yddmy( int,int *,int *,int * );
extern void xprintf( char * );
extern char *Mcpathname( char *);
extern float getpwval( int,float );
extern void xprintf(char *);

/* McIDAS FORTRAN plotting routine calls */
extern void initpl_(int*, int*);
extern void page_( int*, int*, int*, int*, int*);
extern void wrtext_( int*, int*, int*, const char*, int*, int*, size_t);

int getdatetime(char *area,int *date,int *time)
/* obtain the date/time of the current image.
    Inputs  : area    - image name
    Outputs : date    - image date (Julian date)
              time    - image time (HHMMSS format)
*/
{
  int fdir,iok,bufarr[64];

  fdir=opnara(area, 'R');
  if(fdir<=0) {
    return -1;
  }

  iok=readd(fdir,bufarr);
  if(iok!=0) return -2;
  /* obtain date/time */
  *date=bufarr[3];
  *time=bufarr[4];

  clsara(fdir);
  return 0;
}

int readsatdata(char *area,float cenlat,float cenlon,int radius,
                float temps[maxd][maxd],float lats[maxd][maxd],float lons[maxd][maxd],
                int *numx,int *numy)
{
/* Obtains a rectangular array of data from a McIDAS area file.
   Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
   Refer to "McIDAS Software Acquisition and Distribution Policies"
   Inputs : area       - file descriptor for area
            cenlat     - center latitude location of data array
            cenlon     - center longitude location of data array
            radius     - outer radius of data read
   Output : temps      - array containing temperature values
            lats       - array containing latitude values
            lons       - array containing longitude values
            numx       - number of elements in arrays
            numy       - number of lines in arrays
   Remarks:
        This routine does not initialize the output array.
        It does not reduce resolution.
*/

  int fstel,lstel,fstln,lstln;
  int nele,line,ele,band=0,badpoint;
  int iex,ilx,imlin,imele;
  int ixx,iyy,fdir,kk,iok;
  int bufarr[64],istrm[30000];
  double xlat,xlon;
  /*long opts[5],vals[5];*/

  fdir=opnara(area, 'R');
  if(fdir<=0) {
    sprintf(iout,"ERROR READING IMAGE DATA FILE %s",area);
    xprintf(iout);
    return -1;
  }
  iok=readd(fdir,bufarr);
  if(iok!=0) return -2;

  /* check to make sure image line and element resolutions are the same */
  if(bufarr[11]!=bufarr[12]) {
    sprintf(iout," LINE AND ELEMENT RESOLUTIONS NOT SAME");
    xprintf(iout);
    return -3;
  } else {
    kres=bufarr[11];
  }

  /* check to make sure image contains only one band */
  if(bufarr[13]>1) {
    sprintf(iout,"IMAGE MUST BE SINGLE BAND");
    xprintf(iout);
    return -4;
  }

  /* determine band number of image */
  while((BTEST(bufarr[18],band))!=1) { band++; }

  /* setup various parameters for araopt call */
/*
  strncpy ((char *)&opts[0], "SPAC", 4);
  vals[0] = (long) 4;
  strncpy ((char *)&opts[1], "UNIT", 4);
  strncpy ((char *)&vals[1], "BRIT", 4);
  iok = araopt(fdir,1,2, opts, vals); */  /* set output precision */

  /* convert latitude/longitude to image line/element coordinates */
  if(EATOAR(fdir,(double)cenlat,(double)cenlon,bufarr,&imlin,&imele)!=0) {
    sprintf(iout,"POINT NOT ON PLANET : %f %f",cenlat,cenlon);
    xprintf(iout);
    return -5;
  }

  /* define image box boundaries */
  fstln=imlin-(radius/kres)-5;
  lstln=imlin+(radius/kres)+5;
  fstel=imele-(radius/kres)-5;
  lstel=imele+(radius/kres)+5;

  /* make sure data box is not off upper left part of image */
  if(fstel<0||fstln<0)
  {    /* first element is negative (left of the image) */
    sprintf(iout,"DATA IS OFF EDGE OF IMAGE");
    xprintf(iout);
    return -1;
  } else {
    nele=lstel-fstel;  /* total number of elements in each line */
  }

  /* read nele elements for each line of data */
  ARABOX(fdir,fstln,lstln,fstel,lstel,band,"TEMP",4,nele,istrm);
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
        temps[iyy][ixx]=temps[iyy][ixx-1];
      }
      /* convert line/element to latitude/longitude */
      ARTOEA(fdir,line,ele,(int *)bufarr,&ilx,&iex,&xlat,&xlon);
      /* save latitude/longitude location to arrays */
      lats[iyy][ixx]=(float)xlat;
      lons[iyy][ixx]=(float)xlon;
      ++kk;
    }
    if(badpoint>10) {
      /* data line in image is bad... replace with previous */
      sprintf(iout,"BAD DATA POINTS DETECTED AT LINE %d, REPLACING WITH PREVIOUS LINE\n",line);
      xprintf(iout);
      for(ixx=0;ixx<nele;ixx++) {
        temps[iyy][ixx]=temps[iyy-1][ixx];
      }
    }
  }

  iok = clsara(fdir);
  /* calculate total number of lines/elements */
  *numx=lstel-fstel;   /* # columns in temps array */
  *numy=lstln-fstln;   /* # rows in temps array */

  return 0;
}

int BTEST(int i, int pos)
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

char *getsatname(void)
/* Obtain satellite name via McIDAS routine.
    Inputs  : none
    Outputs : return value will be image name
*/
{
  char *filename,fname[64],*ptr;
  int fdir[64],frame,len;
 
  frame=Mcluc(51);
  getfrm_(&frame, fdir);
  sprintf(fname,"AREA%4d\n",fdir[61]);
  ptr=strrchr(fname,' ');
  while(ptr!=NULL) {
    *ptr='0';
    ptr=strrchr(fname,' ');
  }
  filename=Mcpathname(fname);
  len=strlen(filename)-1;
  filename[len]='\0';

  return filename;

}

int getcursorloc(float *lat,float *lon)
/* Obtain cursor location utilizing McIDAS routine itvll.
    Inputs  : none
    Outputs : lat - latitude of cursor
              lon - longitude of cursor
*/
{
  int iras,ipix,iline,iele,iok;
  int iframe,iscen;

  iras = Mcluc (-11);    /* obtain raster value at cursor */
  ipix = Mcluc (-12);    /* obtain pixel value at cursor */
  iframe=Mcluc(51);
  iok=itvll_(&iframe,&iras,&ipix,&iline,&iele,lat,lon,&iscen);

  return iok;
}

int graphhistoryrec(double starttime,double endtime)
/* Graph the history file between given date/times
    Inputs  : starttime - first date/time of record(s) delete
              endtime   - last date/time of record(s) delete
    Outputs : none
*/
{
  FILE *fp;
  int  maxline=120,lwidth=2;
  int  const0=0,const1=1,const2=2,const3=3,const5=5;
  int  const6=6,const7=7,const9=9,const80=80;
  int  it1,it2,it3,it4,it5;
  int  date,time,scene,r9,rapid,land;
  int  ifrm,ilin,iele,lb,le,eb,ee,ia,ib,ic,npts;
  int  cnt,isub,dlen,yax1,xax1,dlenflag,ihour,id,im,iy,ixx,idraw;
  int  ix[500],iy1[500],iy2[500],iy3[500];
  float gwidth,gheight,twidth,theight,tval,tvalB,tvalE,tvalint;
  float rawt,finalt,ci,eye,cloud,mean,lat,lon,wval,pval,fval;
  float buf[7][500],mint=100.0,maxt=-100.0;
  double xbeg,xend,xtime,xspace;
  double curtime,xtimediff;
  char line[120],ca[6],cb[6],cc[6];
  char ctno[12],chour[12],cdate[12],cd1[12],cdate1[12],cwp[12];
  int  padj[2][11]={ {0,32,30,28,26,24,20,18,16,14,12},
                      {0,42,38,32,26,28,26,24,20,16,14} };
  int  wadj[11]={0,30,30,26,24,26,24,26,24,20,20};
  int  pstart[2]={8900,8580};
  char month[12][4]={ "JAN","FEB","MAR","APR","MAY","JUN",
                      "JUL","AUG","SEP","OCT","NOV","DEC"};

  /* get file information */
  fp=fopen(hfile,"r+");
  if(fp==0) {
    sprintf(iout,"ERROR READING HISTORY FILE %s\n",hfile);
    xprintf(iout);
    return -2;
  }

  /* read history file, quit at EOF */
  npts=-1;
  while(fgets(line,maxline,fp) != NULL) {
    (void)sscanf(line,"%s %d %f %f %f %d %d %d %f %f %f %f %f %d",
                 cdate,&time,&rawt,&finalt,&ci,&scene,&r9,&rapid,
                 &eye,&cloud,&mean,&lat,&lon,&land);
    date=cmonth2julian(cdate);
    curtime=calctime(date,time);
    if((curtime>=starttime)&&(curtime<=endtime)) {
      npts++;
      buf[0][npts]=curtime;
      buf[1][npts]=rawt;
      buf[2][npts]=finalt;
      buf[3][npts]=ci;
      buf[4][npts]=eye;
      buf[5][npts]=cloud;
      buf[6][npts]=mean;
      if(eye>maxt) maxt=eye;
      if(cloud<mint) mint=cloud;
    }
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
     twidth  = width (in time units) of x-axis
     theight = height (in CI or temperature) of y-axis */
  gwidth=(float)(ee-eb);
  gheight=(float)(le-lb);
  twidth=xtimediff;
  if(ographtemp) {
    /* graphing temperature */
    ia=4;
    ib=5;
    ic=6;
    strncpy(ca,"Eye T",5);
    strncpy(cb,"Cld T",5);
    strncpy(cc,"Avg T",5);
    tvalB=(float)(10*(int)((maxt+10.0)/10.0));
    tvalE=(float)(10*(int)((mint-10.0)/10.0));
    theight=tvalB-tvalE;
    tvalint=-(theight/50.0);
  } else {
    /* graphing Tno/CI values */
    ia=1;
    ib=2;
    ic=3;
    strncpy(ca,"T#raw",5);
    strncpy(cb,"T#   ",5);
    strncpy(cc,"CI   ",5);
    tvalB=8.0;
    tvalE=3.0;
    theight=5.0;
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
      } else {
        if(owind) {
          sprintf(cwp,"%6d",(int)wval/10);
          it4=ee-10;
        } else {
          sprintf(cwp,"%6d",(int)pval/10);
          it4=ee-5;
        }
      }
      /* left side value plot */
      wrtext_(&it1,&it2,&const9,ctno,&const6,&iword,(size_t)6);
      /* right side value plot */
      wrtext_(&it1,&it4,&const9,cwp,&const6,&iword,(size_t)6);
    }
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
  dlenflag=0;
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
    sprintf(chour,"%d",ihour);
    if(ihour<10) sprintf(chour,"0%d",ihour);
    it1=le+15;
    it2=xax1-8;
    wrtext_(&it1,&it2,&const9,chour,&const2,&iword,(size_t)2);
    if(dlen==10) {
      dlenflag=1;
      sprintf(cdate,"%d",(int)xtime);
      (void)yddmy((int)xtime,&id,&im,&iy);
      sprintf(cd1,"%d",id);
      sprintf(cdate1,"%s%s",month[im-1],cd1);
      it1=le+30;
      it2=xax1-22;
      it3=lb-20;
      wrtext_(&it1,&it2,&const7,cdate,&const7,&iword,(size_t)7);
      wrtext_(&it3,&it2,&const9,cdate1,&const5,&iword,(size_t)5);
    }
    it1=le+15;
    it2=ee+10;
    wrtext_(&it1,&it2,&const9,"(UTC)",&const5,&const2,(size_t)5);
  }

  /* determine TV plotting location for each data point */
  for(ixx=0;ixx<=npts;ixx++) {
    ix[ixx]=eb+(int)(((buf[0][ixx]-xbeg)/(xend-xbeg))*gwidth);
    iy1[ixx]=lb+(int)(((buf[ia][ixx]-tvalE)/theight)*gheight);
    iy2[ixx]=lb+(int)(((buf[ib][ixx]-tvalE)/theight)*gheight);
    iy3[ixx]=lb+(int)(((buf[ic][ixx]-tvalE)/theight)*gheight);
  }

  /* plot lines of intensity estimates/temperatures */
  for(ixx=0;ixx<npts;ixx++) {
    if(drawTR>0) {
      plot_(&iy1[ixx],&ix[ixx],&const0);
      plot_(&iy1[ixx+1],&ix[ixx+1],&drawTR);
    }
    if(drawTW>0) {
      plot_(&iy2[ixx],&ix[ixx],&const0);
      plot_(&iy2[ixx+1],&ix[ixx+1],&drawTW);
    }
    if(drawCI>0) {
      plot_(&iy3[ixx],&ix[ixx],&const0);
      plot_(&iy3[ixx+1],&ix[ixx+1],&drawCI);
    }
  }

  /* plot points at data point locations */
  for(ixx=0;ixx<=npts;ixx++) {
    if(drawTR>0) plotdot(iy1[ixx],ix[ixx],const7);
    if(drawTW>0) plotdot(iy2[ixx],ix[ixx],const7);
    if(drawCI>0) plotdot(iy3[ixx],ix[ixx],const7);
  }
    
  /* draw legend at top of graph */
  it1=20;
  it2=24;
  wrtext_(&it1,&eb,&const9,"Legend :",&const9,&iword,(size_t)9);
  idraw=1;
  it3=eb+(80*idraw);
  it4=eb+(80*idraw)+20;
  it5=eb+(80*idraw)+25;
  if(drawTR>0) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawTR);
    wrtext_(&it1,&it5,&const9,ca,&const5,&iword,(size_t)5);
  }
  it3=eb+(80*idraw);
  it4=eb+(80*idraw)+20;
  it5=eb+(80*idraw)+25;
  if(drawTW>0) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawTW);
    wrtext_(&it1,&it5,&const9,cb,&const5,&iword,(size_t)5);
  }
  it3=eb+(80*idraw);
  it4=eb+(80*idraw)+20;
  it5=eb+(80*idraw)+25;
  if(drawCI>0) {
    idraw++;
    plot_(&it2,&it3,&const0); 
    plot_(&it2,&it4,&drawCI);
    wrtext_(&it1,&it5,&const9,cc,&const5,&iword,(size_t)5);
  }

  /* stamp history file name on bottom of graph */
  it1=le+60;
  it2=10;
  wrtext_(&it1,&it2,&const9,hfile,&const80,&const1,(size_t)80);

  endplt_();
  exit(0);

  return 0;
}

void plotdot(int y,int x,int color)
{
  int a=y+1,b=y-1;
  int c=x+1,d=x-1;
  int const0=0,const1=color;

  plot_(&a,&c,&const0);
  plot_(&b,&c,&const1);
  plot_(&b,&d,&const1);
  plot_(&a,&d,&const1);
  plot_(&a,&c,&const1);
}

int overrideloc( logical xunix )
/* Utilize mouse button press to toggle YES/NO
   agreement with AUTO FIX position location.
    Inputs  : xunix - UNIX/non-UNIX flag
    Outputs : function return value
              1=use Auto Fix position,0=use User Fix position
*/
{
   int istat,iret,button1,button2,itvlin,itvele;

   sprintf(iout,"Do you agree with this position?\n");xprintf(iout);
   if (xunix) {
     sprintf(iout,"YES: Press RIGHT mouse button\n");xprintf(iout);
     sprintf(iout,"NO : Position cursor at desired location\n");xprintf(iout);
     sprintf(iout,"     and press MIDDLE mouse button\n");xprintf(iout);
   } else {
     sprintf(iout,"YES: Press RIGHT Mouse Button\n");xprintf(iout);
     sprintf(iout,"NO : Position cursor at desired location\n");xprintf(iout);
     sprintf(iout,"     and press LEFT mouse button\n");xprintf(iout);
   }

   istat= Mcmoubtn(3, &button1, &button2, &itvlin, &itvele);
   if(istat!=0) return 1;

   /* buttons : left,center=1, right button=2 */
   if (button2 != 0) {
     sprintf(iout,"YES - will use AUTO FIX Position\n");xprintf(iout);
     iret = 1;
   } else if (button1 != 0) {
     sprintf(iout,"NO  - will use USER FIX Position\n");xprintf(iout);
     iret = 0;
   }

   return iret;
}

int overridescene(logical xunix,int *scenenew)
/* Utilize mouse button press to toggle YES/NO
   agreement with SCENE TYPE determination.
    Inputs  : xunix    - UNIX/non-UNIX flag
    Outputs : scenenew - new scene type flag value 
                         (or old value if unchanged)
*/
{
   int button1,button2,itvlin,itvele;
   int oldscene,istat,sceneinit;

   sceneinit=*scenenew;
   sprintf(iout,"ODT has classified the scene as %s\n",scenetype[sceneinit]);xprintf(iout);
   sprintf(iout,"Do you agree with this scene classification?\n");xprintf(iout);
   if (xunix) {
     sprintf(iout,"TOGGLE : Press MIDDLE mouse button\n");xprintf(iout);
     sprintf(iout,"ACCEPT : Press RIGHT  mouse button\n");xprintf(iout);
   } else {
     sprintf(iout,"TOGGLE : Press LEFT  mouse button\n");xprintf(iout);
     sprintf(iout,"ACCEPT : Press RIGHT mouse button\n");xprintf(iout);
   }

   istat= Mcmoubtn(3, &button1, &button2, &itvlin, &itvele);
   if(istat!=0) return -1;

   oldscene=sceneinit;
   /* buttons : left,center=1, right button=2 */
   while(button2 == 0 ) {
     if(button1 != 0) {
       if(sceneinit <= 4) {
         sceneinit = 5;
       } else if(sceneinit == 5) {
         sceneinit = 6;
       } else if(sceneinit == 6) {
         sceneinit = 9;
       } else {
         sceneinit = 1;
       }
       sprintf(iout,"Change to %s\n",scenetype[sceneinit]);xprintf(iout);
     }
     istat= Mcmoubtn(3, &button1, &button2, &itvlin, &itvele);
     if(istat!=0) return -1;
   }
   if(oldscene != sceneinit) {
     sprintf(iout,"Scene type has been changed to %s\n",scenetype[sceneinit]);xprintf(iout);
   } else {
     sprintf(iout,"Scene type has not been changed\n");xprintf(iout);
   }

   *scenenew=sceneinit;
   return 0;

}

char *odtremote(void)
{
  char name[25],llpos[40],cband[10];
  char command[200],cname[50],carea[100],fname[64];
  char *filename,*ptr;
  int iok,len;
  int x,y,frame,il,ie,temp,band;
  int fdir[64];
  float dlat,dlon;

  /* get line/element info from remote data file */
  y=Mcluc(-12);
  x=Mcluc(-11);

  /* convert line/element to lat/lon */
  frame=Mcluc(51);
  getfrm_(&frame, fdir);
  iok=itvll_(&frame,&x,&y,&il,&ie,&dlat,&dlon,&temp);

  memcpy(name, &fdir[55], 24);
  ptr=strchr(name,' ');
  *ptr='\0';

  /* form IMGCOPY command for Mcskeyin command */
  strcpy (command,"IMGCOPY");
  sprintf(cname," %s.%d",name,fdir[61]);
  strcat (command,cname);
  sprintf (carea," %s.%d",lserver,larea);
  strcat (command,carea);
  strcat (command," LATLON=");
  sprintf(llpos,"%f %f",dlat,dlon);
  strcat (command,llpos);
  sprintf(cband," BAND=%d",fdir[3]);
  strcat (command,cband);
  strcat (command," DEV=NNN");

  /* form local data name string variable */
  sprintf(fname,"AREA%4d\n",larea);
  ptr=strrchr(fname,' ');
  while(ptr!=NULL) {
    *ptr='0';
    ptr=strrchr(fname,' ');
  }
  filename=Mcpathname(fname);
  len=strlen(filename)-1;
  filename[len]='\0';

  sprintf(iout,"DOWNLOADING REMOTE IMAGE%s TO LOCAL IMAGE%s\n",cname,carea);
  xprintf(iout);
  sprintf(iout," WITHIN LOCAL DIRECTORY AT %s\n",filename);
  xprintf(iout);

  /* McIDAS command to download area from remote to local server */
  iok=Mcskeyin(command);

  /* this is needed again because Mcskeyin somehow overwrites the
     definition of filename and replaces it with command call to imgcopy.k */
  filename=Mcpathname(fname);
  len=strlen(filename)-1;
  filename[len]='\0';

  return filename;
}
