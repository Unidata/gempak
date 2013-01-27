/************************************************************************
 * RADAR_GRID.C								* 
 * This module contains routines used to project each NEXRAD image into *
 * the output grid projection and sample the radar data to the grid.	*
 *									*
 * CONTENTS:								*
 *									*
 * radar_grid()			Called by FORTRAN driver		*
 * next_img()			Called by FORTRAN driver		*
 * radar_bounds()		Called by FORTRAN driver		*
 *									* 
 * Chiz/Unidata	02/01							* 
 * Chiz/Unidata 02/02		Removed gqbnd() call for perfomance	*
 ***********************************************************************/
#include <geminc.h>
#include <gemprm.h>
#include <imgdef.h>

#ifdef UNDERSCORE
#define	image_ginit	image_ginit_
#define	radar_grid	radar_grid_
#define next_img	next_img_
#define	radar_bounds	radar_bounds_

#define cgdtwdt	cgdtwdt_
#endif

/* prototypes */
void radar_bounds(int *kx, int *ky, int *iret);

int   gsize=0;
int worksize=0;
float *grid=NULL;
float *xin=NULL,*xout=NULL,*yin=NULL, *yout=NULL;

void image_ginit ( int *kx, int *ky, int *sizx, int *sizy, int *iret )
{
int i, ier;
int newsize = (*kx) * (*ky);
char devstr[40];

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
	 *iret = -1;
         return;
         }
      }


    sprintf(devstr,"gif|/dev/null|%d;%d\0",*sizx,*sizy);
    gg_sdev ( devstr, &ier, strlen(devstr));
    gclear ( &ier );
    im_drop ( &ier );
    radar_bounds(kx, ky, iret);
}

void next_img(char *filpath, char *templ, char *gemtim, char *filnm, 
                int *numc, int *idelt, int *ier)
{
int nfile, i,iret;
int tarr1[5], tarr2[5];
char fname[FILE_FULLSZ],fmnam[80],outtim[20],cgemtim[16];
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

float XL, YB, XR, YT;
float LX1, LY1, LX2, LY2;

void radar_bounds(int *kx, int *ky, int *iret)
{
int i,j, ier;
int np;

*iret = 0;

np = 0;
for(i=1;i<=*kx;i++)
   for(j=1;j<=*ky;j++)
   {
   xin[np] = i; yin[np] = j;
   np++;
   }

gtrans(sys_G, sys_D, &np, xin, yin, xout, yout, &ier, strlen(sys_G), strlen(sys_D));

}

void radar_grid(int *kx, int *ky, float *rlev)
{
int i, j, np, ip, x, y, it;

float rval;

np = 0;
for(i=1;i<=*kx;i++)
   for(j=1;j<=*ky;j++)
      {
      x = (int) rint(xout[np]); y = (int) rint(yout[np]);
      it = ((y - 1) * imnpix) + x - 1;
      ip = ((j - 1) * (*kx) ) + i - 1;
      /*printf("look i,j %d,%d start %d %d // %d %d // %f %f // %f %f\n",i,j,x,y,it,ip,xout[np],yout[np],xin[np],yin[np]);*/

      if((xout[np] >= 1)&&(xout[np] <= imnpix)&&
         (yout[np] >= 1)&&(yout[np] <= imnlin))
         {
         if((imgData[it] >= immnpx)&&(imgData[it] <= immxpx))
            {
            rval = rlev[(int)imgData[it]];
            if(rval > grid[ip]) grid[ip] = rval;
            /*printf("// %f %f %d   %d %d %d   %d %d rval %f\n", 
		xout[np],yout[np],np, i,j,ip,it, imgData[it], rval);*/
            }
         else
            printf("%d %d %d   %d %d %d   %d\n", x,y,np, i,j,ip, imgData[it]);
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
