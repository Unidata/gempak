#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef UNDERSCORE
#define	gd_itoh	gd_itoh_
#endif

void dcchecksub ( int iflno, int center, int processid, 
		int level[], int ivcord, char gdattim[][20], 
		char *parm, int nx, 
		int ny, int *iret)

{
int i, ier, isame;
int idharr[10];

static int idsav[10]={-1}, censav=-1, procsav=-1, nxsav=-1, nysav=-1;

gd_itoh ( gdattim, level, &ivcord, parm, idharr, &ier,
	  20, 12);

isame = -1;
if( (center == censav) && (processid == procsav) &&
    (nx == nxsav) && (ny == nysav) )
    {
    isame = 0; 
    for(i=0; i < 10; i++)
	if( idharr[i] != idsav[i] )
	   {
	   isame = -1;
	   break;
	   }
    }

if ( isame != 0 )
   {
   censav = center;
   procsav = processid;
   nxsav = nx;
   nysav = ny;
   for(i=0; i < 10; i++)
      idsav[i] = idharr[i];
   }

*iret = isame;

}

int sizgrd=0, ngx=0, ngy=0;
float *savgrd=NULL;

void dcsubsav ( float *tmpgrd, int nx, int ny)
{
if ( savgrd == NULL )
   {
   savgrd = (float *)malloc( (nx * ny) * sizeof(float) );
   sizgrd = nx * ny;
   }
else if ( ( nx*ny ) > sizgrd )
   {
   savgrd = (float *)realloc( savgrd, (nx * ny) * sizeof(float) );
   sizgrd = nx * ny;
   }

memcpy ( savgrd, tmpgrd, (nx * ny) * sizeof(float) );
ngx = nx;
ngy = ny;

}

void dclastsub ( float *tmpgrd, int *nx, int *ny, int *iret)
{
*nx = ngx;
*ny = ngy;
if(savgrd == NULL)
   *iret = -1;
else
   *iret = 0;

memcpy ( tmpgrd, savgrd, sizgrd * sizeof(float) );

}
