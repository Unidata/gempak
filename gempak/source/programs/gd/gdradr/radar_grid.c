/************************************************************************
 * RADAR_GRID.C								* 
 * This module contains routines used to project each NEXRAD image into *
 * the output grid projection and sample the radar data to the grid.	*
 *									*
 * CONTENTS:								*
 *									*
 * radar_grid()			Called by FORTRAN driver		*
 * next_radar()			Called by FORTRAN driver		*
 * radar_boundsinit()		Called by FORTRAN driver		*
 * radar_bounds()		Called by FORTRAN driver		*
 *									* 
 * Chiz/Unidata	02/01							* 
 * Chiz/Unidata 02/02		Removed gqbnd() call for perfomance	*
 ***********************************************************************/
#include <geminc.h>
#include <gemprm.h>
#include <imgdef.h>

#ifdef UNDERSCORE
#define	radar_ginit	radar_ginit_
#define	radar_grid	radar_grid_
#define next_radar	next_radar_
#define	radar_bounds	radar_bounds_
#define radar_boundsinit	radar_boundsinit_

#define cgdtwdt	cgdtwdt_
#endif

int   gsize=0;
float *grid=NULL;

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

void next_radar(char *filpath, char *templ, char *gemtim, char *filnm, 
                int *numc, int *idelt, int *ier)
{
int i,iret, nfile;
int tarr1[5], tarr2[5];
char fname[FILE_FULLSZ],fmnam[80],outtim[20],cgemtim[16];
struct dirent   **namelist=NULL;

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

void radar_boundsinit()
{
numstats = 0;
}

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

gbounds[numstats][0] = XL;
gbounds[numstats][1] = YB;
gbounds[numstats][2] = XR;
gbounds[numstats][3] = YT;

stnums[numstats] = *istnm;
numstats++;

}

void radar_grid(int *kx, int *ky, float *rlev)
{
int ier, i, j, np, ip, x, y, it;

char gsys[]="G", msys[]="L";
float rval;
int xstart,xstop,ystart,ystop;
static int worksize=0;
static float *xin=NULL,*xout=NULL,*yin=NULL, *yout=NULL;

if ( worksize < gsize )
   {
   if ( xin != NULL ) free(xin);
   if ( xout != NULL ) free(xout);
   if ( yin != NULL ) free(yin);
   if ( yout != NULL ) free(yout);

   xin = (float *)malloc(gsize * sizeof(float));
   xout = (float *)malloc(gsize * sizeof(float));
   yin = (float *)malloc(gsize * sizeof(float));
   yout = (float *)malloc(gsize * sizeof(float));
   worksize = gsize;

   if(xin == NULL) worksize = 0;
   if(xout == NULL) worksize = 0;
   if(yin == NULL) worksize = 0;
   if(yout == NULL) worksize = 0;

   if ( worksize == 0 ) 
      {
      printf("failed to malloc work arrays\n");
      return;
      }
   }

xstart = (int)XL; xstop = (int)XR;
ystart = (int)YB; ystop = (int)YT;

np = 0;
for(i=xstart;i<=xstop;i++)
   for(j=ystart;j<=ystop;j++)
   {
   xin[np] = i; yin[np] = j;
   np++;
   }
gtrans(gsys, msys, &np, xin, yin, xout, yout, &ier, strlen(gsys), strlen(msys));

np = 0;
for(i=xstart;i<=xstop;i++)
   for(j=ystart;j<=ystop;j++)
      {
      if((xout[np] >= 1)&&(xout[np] <= imnpix)&&
         (yout[np] >= 1)&&(yout[np] <= imnlin))
         {
         x = (int) rint(xout[np]); y = (int) rint(yout[np]);
         it = ((y - 1) * imnpix) + x - 1;
         ip = ((j - 1) * (*kx)) + i - 1;

         if((imgData[it] >= immnpx)&&(imgData[it] <= immxpx))
            {
            rval = rlev[(int)imgData[it]];
            if(rval > grid[ip]) grid[ip] = rval;
            }
         else
            printf("%d %d %d   %d %d %d   %d\n", x,y,np, i,j,ip, imgData[np]);
         }
      np++;
      }

}



void cgdtwdt ( const int *iacss, const int *ikx, const int *iky,
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
