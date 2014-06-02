/********************************************************/
/* RADAR_GRID.C						*/
/* Chiz/Unidata	02/01					*/
/*							*/
/* Updates						*/
/* M. James/Unidata	09/10 Updated bounds check for  */
/*			      MAXNEX increase           */
/* M. James/Unidata	01/14 Updates for DVL		*/
/*							*/
/********************************************************/
#include <geminc.h>
#include <gemprm.h>
#include <imgdef.h>
#include "writepng.h"

/* maximum size of a nexrad raster typically 460x464 */
#define GINISZ	14400000
#define MAXNEX	500000
#define MAXSTR  512
#define DEFBAND 25

#ifdef UNDERSCORE
#define	radar_ginit	radar_ginit_
#define	radar_grid	radar_grid_
#define next_radar	next_radar_
#define	radar_bounds	radar_bounds_
#define radar_boundsinit	radar_boundsinit_
/*#define dump_bounds	dump_bounds_*/
#define cgdgddt cgdgddt_
#define gdlgin  gdlgin_
#define gdhgin  gdhgin_
#define freeimg freeimg_
#endif

int   gsize=0;
int ncal;
float *grid=NULL;

extern unsigned char *imdata;
extern unsigned char *imndat;

typedef struct calvals {
        float minpx, maxpx, minval, maxval;
        } calvals;
struct calvals *calptr=NULL;

void freeimg () {
   free(imdata);
   free(imndat);
}





void radar_ginit ( int *kx, int *ky, int *iret )
{
int i;
int newsize = (*kx) * (*ky);

    *iret = 0;

    if ( newsize > gsize ) {
        if ( gsize > 0 )
            grid = (float *)realloc( grid, newsize*sizeof(float) );
        else
            grid = (float *)malloc( newsize*sizeof(float) );

        if ( grid == NULL ) {
            gsize = 0;
            printf ("Error allocating requested grid points ( %d ).\n", newsize );
            *iret = -1;
        }
        else
            gsize = newsize;
    }

    for ( i=0; i < gsize; i++ ) grid[i] = RMISSD;
}


// next_radar is called for each station
// 
// INPUTS	DESC		EXAMPLE
// filpath	file path	$RAD/NIDS/AMA/N0Q
// templ	file template	N0Q_YYYYMMDD_HHNN
// gemtim       current time	100929/2147
//
// OUTPUTS
// filnm	filename	$RAD/NIDS/AMA/N0Q/N0Q_20100929_2137
// numc		number of chars to end of filename string
// idelt
// ier		error
//

void next_radar(char *filpath, char *templ, char *gemtim, char *filnm, 
                int *numc, int *idelt, int *ier)
{
int i, nfile, iret;
int tarr1[5], tarr2[5];
char fname[FILE_FULLSZ],fmnam[80],outtim[20], cgemtim[16];
struct dirent **namelist=NULL;

  *idelt = 0;

  st_null(gemtim,cgemtim,&i,&iret,15,16);

  ti_ctoi(gemtim,tarr1,ier,15);
  if(*ier != 0) return;

  /* create the name for which we will compare against */
  cfl_mnam(cgemtim, templ, fmnam, &iret);

  /* return list of files reverse sorted (newest time first) */
  cfl_scnt(filpath, templ, -1, &namelist, &nfile, &iret);

  /* find the first file whose time is older than our current time */
  i = 0;
  while ( i < nfile ) {
     strcpy(fname, namelist[i]->d_name);
     cfl_mdat(fname, templ, cgemtim, outtim, &iret);

     ti_ctoi(outtim,tarr2,&iret,strlen(outtim));

     ti_mdif(tarr1, tarr2, idelt, &iret);

     if ( iret == 0 && *idelt >= 0 ) {
       sprintf(filnm,"%s/%s\0",filpath,fname);
       *numc = strlen(filnm);
       break;
     }
     i++;
  }

  if ( i >= nfile ) *ier = -1;

  for ( i=0; i < nfile; i++) {
     free(namelist[i]);
  }
  if ( namelist != NULL ) free(namelist);

}

#define	MAXSTATS 400
static int numstats=0;
static int stnums[MAXSTATS];
static float gbounds[MAXSTATS][4];
float XL, YB, XR, YT;

void load_bounds()
{
FILE *fp;
char line[256];
int test,i1;
float i2,i3,i4,i5;
if((fp = fopen("boundstable.dat","r")) != NULL)
   {
   while(fgets(line,255,fp) != NULL)
      {
      test = sscanf(line,"%d %f %f %f %f",&i1,&i2,&i3,&i4,&i5);
      if(test == 5)
         {
	 stnums[numstats] = i1;
	 gbounds[numstats][0] = i2;
	 gbounds[numstats][1] = i3;
	 gbounds[numstats][2] = i4;
	 gbounds[numstats][3] = i5;
	 numstats++;
         }
      }
   }
}

void radar_boundsinit()
{
numstats = 0;
/*load_bounds();*/
}

/*
void dump_bounds()
{
int i;
for(i=0;i<numstats;i++)
   {
   printf("%6d %15.4f %15.4f %15.4f %15.4f\n",stnums[i],
      gbounds[i][0],gbounds[i][1],gbounds[i][2],gbounds[i][3]);
   }
}
*/

void radar_bounds(int *istnm, int *kx, int *ky, int *iret)
{
char gsys[]="G";
int i;
char msys[]="L";
int np=4;
float xin[4], yin[4], xout[4], yout[4];
float mnx,mxx,mny,mxy;

*iret = 0;

/*
 * See if this station is already been used.
 */
for(i=0;i<numstats;i++)
   {
   if(*istnm == stnums[i])
      {
      if(gbounds[i][0] == RMISSD) *iret = -1;
      XL = gbounds[i][0]; YB = gbounds[i][1]; XR = gbounds[i][2]; YT = gbounds[i][3];
      return;
      }
   }

/*
 * Get the grid point locations of the 4 corners of the image.
 * If any portion of the image lies within the grid, store
 * the grid point boundaries.
 */
xin[0] = imleft; yin[0] = imbot;
xin[1] = imrght; yin[1] = imbot;
xin[2] = imleft; yin[2] = imtop;
xin[3] = imrght; yin[3] = imtop;
gtrans(msys, gsys, &np, xin, yin, xout, yout, iret, strlen(gsys), strlen(msys));
//printf("look ier outs %f %f   %f %f   %f %f   %f %f\n",xout[0],yout[0],xout[1],yout[1],xout[2],yout[2],xout[3],yout[3]);

if(*iret == 0)
   {
   mnx = RMISSD; mny = RMISSD; mxy = RMISSD; mxx = RMISSD;
   for(i=0;i<np;i++)
      {
      if((xout[i] < mnx)||(mnx == RMISSD)) mnx = xout[i];
      if((xout[i] > mxx)||(mxx == RMISSD)) mxx = xout[i];
      if((yout[i] < mny)||(mny == RMISSD)) mny = yout[i];
      if((yout[i] > mxy)||(mxy == RMISSD)) mxy = yout[i];
      }

   if((mxx < 0)||(mnx > (*kx+1))||
      (mxy < 0)||(mny > (*ky+1)))
      {
      XL = XR = YB = YT = RMISSD;
      *iret = -1;
      }
   else
      {
      if(mnx < 1)
         XL = 1;
      else
         XL = floor(mnx);
      if(mxx > *kx)
         XR = *kx;
      else
         XR = ceil(mxx);
      if(mny < 1)
         YB = 1;
      else
         YB = floor(mny);
      if(mxy > *ky)
         YT = *ky;
      else
         YT = ceil(mxy);
      }
   }
else
   {
   /* *iret != 0 */
   XL = XR = YB = YT = RMISSD;
   }
   
//printf("look points %f %f %f %f\n",XL,XR,YB,YT);
gbounds[numstats][0] = XL;
gbounds[numstats][1] = YB;
gbounds[numstats][2] = XR;
gbounds[numstats][3] = YT;
   
stnums[numstats] = *istnm;
numstats++;

}

/* radar_grid
 * 
 * INPUTS
 * 	prodflg	product flag 0 or 1
 * 	kx, ky  grid size
 * 	grid   grid
 * 	rlev	data levels
 *
 */

void radar_grid(int *prodflg, int *kx, int *ky, float *rlev)
{
char *radarea;
int lens, ier, i, j, np, ip, x, y, it;

char fname[LLMXLN];
char gsys[]="G", msys[]="L";
float xl, yb, xr, yt, rval, rvalmx;
int xstart,xstop,ystart,ystop;
int imode=0, iarea=0, ipix;
static int xcsiz=0;
static float *xin=NULL,*xout=NULL,*yin=NULL, *yout=NULL;

// Allocate memory
if ( xin == NULL ) {
   xin = (float *)malloc(MAXNEX * sizeof(float));
   xout = (float *)malloc(MAXNEX * sizeof(float));
   yin = (float *)malloc(MAXNEX * sizeof(float));
   yout = (float *)malloc(MAXNEX * sizeof(float));
   if(xin == NULL) printf("failed to malloc xin\n");
   if(xout == NULL) printf("failed to malloc xout\n");
   if(yin == NULL) printf("failed to malloc yin\n");
   if(yout == NULL) printf("failed to malloc your\n");
   xcsiz = MAXNEX;
}

xstart = (int)XL; xstop = (int)XR;
ystart = (int)YB; ystop = (int)YT;

// Reallocate memory if new grid size exceeds MAXNEX
if ( ( ( xstop - xstart + 1 ) * ( ystop - ystart + 1 ) ) > xcsiz ) {
   xcsiz = (xstop - xstart + 1)*(ystop - ystart + 1);
   printf("increasing MAXNEX to %d [%d %d %d %d]\n",xcsiz,xstop,xstart,ystop,ystart);
   xin = (float *)realloc(xin, xcsiz*sizeof(float));
   xout = (float *)realloc(xout, xcsiz*sizeof(float));
   yin = (float *)realloc(yin, xcsiz*sizeof(float));
   yout = (float *)realloc(yout, xcsiz*sizeof(float));
   if(xin == NULL) printf("failed to realloc xin\n");
   if(xout == NULL) printf("failed to realloc xout\n");
   if(yin == NULL) printf("failed to realloc yin\n");
   if(yout == NULL) printf("failed to realloc your\n");
}


np = 0;
for ( i=xstart; i<=xstop; i++ )
   for ( j=ystart; j<=ystop; j++ ) {
      xin[np] = i; yin[np] = j;
      np++;
   }

// Convert grid from G to L
gtrans(gsys, msys, &np, xin, yin, xout, yout, &ier, strlen(gsys), strlen(msys));

np = 0;
rvalmx = RMISSD;
for ( i=xstart; i<=xstop; i++ )
   for ( j=ystart; j<=ystop; j++ ) {
      if ( ( xout[np] >= 1 ) && ( xout[np] <= imnpix ) &&
           ( yout[np] >= 1 ) && ( yout[np] <= imnlin ) ) {
         x = (int) rint(xout[np]);
         y = (int) rint(yout[np]);
         it = ((y - 1) * imnpix) + x - 1;
         ip = ((j - 1) * (*kx)) + i - 1;
         if ( ( imgData[it] >= immnpx ) && ( imgData[it] <= immxpx ) ) {
            if ( *prodflg > 0) {
		rval = rlev[(int)imgData[it]];
            } else {
            	rval = (int)imgData[it];
            }
            	
	    if ( rval > rvalmx ) rvalmx = rval;
            if ( rval > grid[ip] ) grid[ip] = rval;
         } else
            printf("%d %d %d   %d %d %d   %d [%d %d]\n", x,y,np, i,j,ip, imgData[it],immnpx, immxpx);
      }
      np++;
   }

}
void cgdgddt ( const int *iacss, const int *ikx, const int *iky,
        int *ighdr, const char *time1, const char *time2,
        const char *parm, const int *nbits, const int *ipktyp,
        int *iret)
{

char gdattm[2][DTTMSZ], prm[13];
const int ivcord=0;
const int level1=0;
const int level2=-1;
const int rewrit=G_TRUE;

cst_padString(time1, ' ', 1, DTTMSZ-1, gdattm[0]);
cst_padString(time2, ' ', 1, DTTMSZ-1, gdattm[1]);
cst_padString(parm, ' ', 1, 12, prm);

cgd_wpgd ( iacss, (const float *) grid, ikx, iky, ighdr, gdattm[0],
                gdattm[1], &level1, &level2, &ivcord, prm, &rewrit,
                ipktyp, nbits, iret );
}  




void	gdlgin ( int *kx, int *ky, 
                int *compress, int *prodlen, int *iret)
{
int ival,i,j,k,ier,lendat,ioff,imax=IMISSD,imin=IMISSD;
float scale, offset, pxoffset, fvals[4];
int num;
char *cpos;
static int nbytes=1;
unsigned char *tempdat=NULL;

*iret = 0;

lendat = *kx * *ky;

imndat = (unsigned char *) malloc (lendat * sizeof(unsigned char));
imdata = (unsigned char *) malloc (sizeof(unsigned char));


if((imdata == NULL)||(imndat == NULL))
   {
   if(imdata != NULL) free(imdata);
   if(imndat != NULL) free(imndat);
   printf("failed to allocate data block storage\n");
   *iret = -1;
   return;
   }

if(*compress == 128)
   {
   tempdat = (unsigned char *) malloc (*kx * sizeof(unsigned char));
   if(tempdat == NULL)
      {
      printf("failed to allocate compressed line storage\n");
      free(imdata);
      free(imndat);
      *iret = -1;
      return;
      }
   printf("Starting compression\n");
   png_set_memheap((char *)imndat);
   pngout_init(*kx,*ky);
   }


for(j=*ky-1;j>=0;j--)
   {
   for (i=0;i<*kx;i++)
      {
      k = (j * *kx) + i;
      if(grid[k] == RMISSD)
         ival = 0;
      else
         {
	 scale = 1; offset = 0; pxoffset = 0;
	 if(ncal != 0)
            {
	    num = 0;
	    while((num < ncal) && (grid[k] > calptr[num].maxval)) num++;
	    if(num < ncal+1)
	       {
	       pxoffset = calptr[num].minpx;
	       offset = calptr[num].minval;
	       scale = (calptr[num].maxpx - calptr[num].minpx)/(calptr[num].maxval - calptr[num].minval);
	       }
	    else
	       printf("error in cal block\n");
            }
         ival = (int)(((grid[k] - offset)*scale) + pxoffset);
         }

      if((imax == IMISSD)||(ival > imax)) imax = ival;
      if((imin == IMISSD)||(ival < imin)) imin = ival;

      if(*compress == 128)
         {
	 ioff = i;
         mv_itob(&ival,&ioff,&nbytes,tempdat,&ier);
         }
      else
         {
         ioff = ((*ky - 1 - j) * *kx ) + i;
         mv_itob(&ival,&ioff,&nbytes,imndat,&ier);
         }
      }
   if(*compress == 128)
      pngwrite((char *)tempdat);
   }

if(*compress == 128)
   {
   pngout_end();
   free(tempdat);
   *prodlen = png_get_prodlen();
   }
else
   *prodlen = lendat;

/*printf("Product min val %d, max val %d\n",imin,imax);*/
}



void	gdhgin (int *ignhdr, char *lprod, int *lenp, int *tarr, 
                int *compress, int *iret)
{
int i,j,nbytes=4,ival,icnt,ier,boff,ipos;
int negflg=G_FALSE,FOUND;
size_t slen;
unsigned char *barr;
char prodid[80],line[512];
char header[80]; 
int lenhd = 18;
char defstr[]=" ",*cpos;
int iband,numstr;
float fvals[4];
FILE *fp;
static int nstrings=10,init=-1;
static char **arrptr;
char *calblock;

char headstr[24]; /* Gini expects 20 or 24 bytes, eg PDB starts at byte 21 or 25 */
static char trailer[]={'\r','\r','\n','\0','\0','\0'};

if(init == -1)
   {
   init = 0;
   arrptr = (char **) malloc(sizeof(char *) * nstrings);
   for(i=0; i < nstrings; i++)
      arrptr[i] = (char *) malloc(MAXSTR);
   }

iband = DEFBAND;
ncal = 0;
calblock = NULL;

if(calptr != NULL) 
   {
   free(calptr);
   calptr = NULL;
   }

strncpy(prodid,lprod,*lenp);
prodid[*lenp] = '\0';

fp = cfl_tbop ( "nex2gini.tbl", "unidata", &ier);
if(fp == NULL) {
  printf("warning: could not open nex2gini.tbl configuration table\n");
  sprintf(header,"TICZ99 CHIZ %02d%02d%02d\0",tarr[2],tarr[3], tarr[4] );
} else {
   FOUND = 0;
   while((FOUND == 0)&&(fgets(line,512,fp)!= NULL)) {
      if(line[0] == '!') continue;
      slen = strcspn(line," \t"); 
      if(strncmp(prodid,line,slen) == 0) {
         FOUND = 1;
         if(line[strlen(line)-1] == '\n') line[strlen(line)-1] = '\0';
         cst_clst(line, ' ', defstr,nstrings,MAXSTR,arrptr, &numstr, &ier);
         sscanf(arrptr[1],"%d",&iband);
         sprintf(header,"%s %s %02d%02d%02d\0",
	    arrptr[4],arrptr[5], tarr[2],tarr[3], tarr[4] );
   	
         /*printf("look lengths %d %d\n",strlen(arrptr[4]),strlen(arrptr[5]));
         printf("look header %d %s \n",iband,header);*/
	 calblock = arrptr[3];
	 sscanf(arrptr[2],"%d",&ncal);
      }
   }
   fclose(fp);
   if(FOUND == 0) {
      printf("warning: no calibration found in nex2gini configuration table\n"); 
      sprintf(header,"TICZ99 CHIZ %02d%02d%02d\0",tarr[2],tarr[3], tarr[4] );
   }
}

lenhd = strlen(header);
if(lenhd > sizeof(headstr)-6) {
   printf("warning header being truncated to %d characters\n",
      sizeof(headstr)-6);
   lenhd = sizeof(headstr);
}

memset(headstr,' ',sizeof(headstr));
memcpy(headstr,header,lenhd);
memcpy(headstr+sizeof(headstr)-sizeof(trailer),trailer,sizeof(trailer));


/*
 * Use satellite ID=99 and Band as provided (else 25) 
 */
barr = (unsigned char *)ignhdr;
for(i=0;i<sizeof(headstr);i++)
   barr[i] = (unsigned char)headstr[i];

boff = 20; /* assume PDB will start at octet 21 */
barr[boff + 1] = 1;
barr[boff + 2] = 99;
barr[boff + 3] = 1;
barr[boff + 4] = iband;

barr[boff + 9] = tarr[0] % 100;
barr[boff + 10] = tarr[1];
barr[boff + 11] = tarr[2];
barr[boff + 12] = tarr[3];
barr[boff + 13] = tarr[4];
barr[boff + 14] = 0;
barr[boff + 15] = 0;

barr[boff + 38] = 0; /* scanning mode, only 0 known */

barr[boff + 42] = 1; /* image resolution */

barr[boff + 43] = *compress; /* compression */

/*
 * Set the size of header to 512 bytes
 */
ival = 512; i = boff + 45; j = 2;
mv_itob(&ival, &i, &j, barr, &ier);

/*
 * set the Calblock flag to 128
 */
barr[boff + 47] = 128;

/*
 * move the Cal block Units
 */
if(calblock != NULL)
   {
   cpos = strchr(calblock,',');
   if(cpos != NULL)
      {
      slen = cpos - calblock;
      for(i=0;i<8;i++)
         {
         if(i < slen)
	    barr[boff+48+i] = calblock[i];
	 else
	    barr[boff+48+i] = ' ';
         }
      barr[boff+56] = ncal;

      calptr = (struct calvals *)malloc(sizeof(struct calvals)*ncal);
      for(icnt=0;icnt<ncal;icnt++)
         {
         cst_rlst ( cpos+1, ',', IMISSD, 4, fvals, &i, &ier );
         ival = (int)(fvals[0]*10000.);
         i = boff + 57 + (16*icnt); j = 4;
         mv_itob(&ival, &i, &j, barr, &ier);
         ival = (int)(fvals[1]*10000.);
         i = boff + 61 + (16*icnt); j = 4;
         mv_itob(&ival, &i, &j, barr, &ier);
         ival = (int)(fvals[2]*10000.);
         i = boff + 65 + (16*icnt); j = 4;
         mv_itob(&ival, &i, &j, barr, &ier);
         ival = (int)(fvals[3]*10000.);
         i = boff + 69 + (16*icnt); j = 4;
         mv_itob(&ival, &i, &j, barr, &ier);
	 cpos = strchr(cpos+1,';');

	 calptr[icnt].minpx = fvals[0];
	 calptr[icnt].maxpx = fvals[1];
	 calptr[icnt].minval = fvals[2];
	 calptr[icnt].maxval = fvals[3];
	 }
      }
   }

*iret = 0;
}
