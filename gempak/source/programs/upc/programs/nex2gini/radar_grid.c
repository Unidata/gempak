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
#define GINISZ	14400000

/* maximum size of a nexrad raster typically 460x464 */
#define MAXNEX	500000

#ifdef UNDERSCORE
#define	radar_grid	radar_grid_
#define next_radar	next_radar_
#define	radar_bounds	radar_bounds_
#define radar_boundsinit	radar_boundsinit_
/*#define dump_bounds	dump_bounds_*/
#endif

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
 * 	fdata   grid
 * 	rlev	data levels
 *
 */

void radar_grid(int *prodflg, int *kx, int *ky, float *fdata, float *rlev)
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

         // If imgData is between min and max (0 and 16 for 4 bit 0 and 255 for HiRes 8 bit)

         if ( ( imgData[it] >= immnpx ) && ( imgData[it] <= immxpx ) ) {

            // assign rval to level specified by imgData number

            if ( *prodflg > 0) {
		rval = rlev[(int)imgData[it]];
            } else {
            	rval = (int)imgData[it];
            }
            	
	    if ( rval > rvalmx ) {
               rvalmx = rval;
            }

            // if rval is great than existing value, replace

            if ( rval > fdata[ip] ) {
                fdata[ip] = rval;
            }
         } else
            printf("%d %d %d   %d %d %d   %d [%d %d]\n", x,y,np, i,j,ip, imgData[it],immnpx, immxpx);
      }
      np++;
   }

}

