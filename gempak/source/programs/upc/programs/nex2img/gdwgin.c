#include "geminc.h"
#include "gemprm.h"
#include "gd.h"

#ifdef UNDERSCORE
#define	gdwgin	gdwgin_
#define calinit	calinit_
#endif


int ncal, minpx, maxpx;

typedef struct calvals {
        float minpx, maxpx, minval, maxval;
        } calvals;

struct calvals *calptr=NULL;



void	gdwgin (char *gemfil, float *grid, int *kx, int *ky, 
                char *lutfil, int *iret)
{
FILE *fp;
int isize, i, j, k, invj;
float scale, offset, pxoffset;
int ncol=0, num, ier;
char line[256];
int ir, ig, ib, ix;
unsigned char red[256];
unsigned char green[256];
unsigned char blue[256];
float fpx, ratio, doffset;
gdImagePtr gdim;

*iret = 0;

if((strcmp(lutfil,"gray") == 0)||
   (strcmp(lutfil,"grey") == 0))
   fp = cfl_tbop ( "lingray.tbl", "luts", &ier);
else
   fp = cfl_tbop ( lutfil, "luts", &ier);


if(fp != NULL)
   {
   while(fgets(line,255,fp) != NULL)
      {
      if(line[0] == '!') continue;
      sscanf(line+23,"%d %d %d",&ir, &ig, &ib);
      red[ncol] = ir;
      green[ncol] = ig;
      blue[ncol] = ib;
      ncol++;
      }
   fclose(fp);
   }
else
   {
   printf("could not open %s\n",lutfil);
   ncol = 256;
   for(i=0;i<ncol;i++)
      {
      red[i] = i;
      green[i] = i;
      blue[i] = i;
      }
   }


if((fp = fopen(gemfil,"wb")) != NULL)
    {
    isize = (*kx) * (*ky);

    gdim = gdImageCreate ( *kx, *ky );

    for(j=*ky-1;j>=0;j--)
       {
       for (i=0;i<*kx;i++)
          {
          k = ( j * *kx) + i;
          invj = (*ky - 1 - j);
          if(grid[k] == RMISSD)
	     {
	     ix = 0;
             gdImageSetPixel ( gdim, i, invj, ix);
             }
          else
             {
	     scale = 1; offset = 0; pxoffset = 0;
             if(ncal != 0)
                {
                num = 0;
                while((num < ncal) && (grid[k] > calptr[num].maxval)) num++;
                if(num < ncal)
                   {
                   pxoffset = calptr[num].minpx;
                   offset = calptr[num].minval;
                   scale = (calptr[num].maxpx - calptr[num].minpx)/(calptr[num].maxval -
			calptr[num].minval);
                   }
                else
                   printf("error in cal block\n");
                }
	     fpx = ((grid[k] - offset)*scale) + pxoffset;
	     if(fpx > maxpx)
		ix = ncol - 1;
	     else if( fpx < minpx)
		ix = 0;
	     else
		{
		ratio = (float)(ncol-1) / (float)(maxpx - minpx);
		doffset = minpx * ratio - 0.5; /* offset for data mapping */
		ix = (int)(fpx * ratio  - doffset);
		}
		
             gdImageSetPixel ( gdim, i, invj, ix);
             }
          }
       }
    gdim->interlace = 0;
    gdim->transparent = -1;
    for(i=0;i<ncol;i++)
       gdImageColorAllocate(gdim,red[i],green[i],blue[i]);
    gdImageGif ( gdim, fp);
    gdImageDestroy ( gdim );
    fclose(fp);
    }
else
    *iret = -1;



}

#define MAXSTR  512
#define DEFBAND 25

void calinit(char *prodid, int *iret)
{
size_t slen;
char line[512], defstr[]=" ";
int iband,numstr;
float fvals[4];
FILE *fp;
static int nstrings=10,init=-1;
static char **arrptr;
char *calblock, *cpos;
int FOUND=0, ier;
int i, icnt;

if(init == -1)
   {
   init = 0;
   arrptr = (char **) malloc(sizeof(char *) * nstrings);
   for(i=0; i < nstrings; i++)
      arrptr[i] = (char *) malloc(MAXSTR);
   }

fp = cfl_tbop ( "nex2gini.tbl", "unidata", &ier);
if(fp != NULL)
   {
   while((FOUND == 0)&&(fgets(line,512,fp)!= NULL))
      {
      if(line[0] == '!') continue;
      slen = strcspn(line," \t");
      if(strncmp(prodid,line,slen) == 0)
         {
         FOUND = 1;
         if(line[strlen(line)-1] == '\n') line[strlen(line)-1] = '\0';
         cst_clst(line, ' ', defstr,nstrings,MAXSTR,arrptr, &numstr, &ier);
         sscanf(arrptr[1],"%d",&iband);

         calblock = arrptr[3];
         sscanf(arrptr[2],"%d",&ncal);
         }
      }
   fclose(fp);
   }

if(FOUND == 0)
   printf("warning: no calibration found in nex2gini configuration table\n");

/*
 * move the Cal block Units
 */
if(calblock != NULL)
   {
   cpos = strchr(calblock,',');
   if(cpos != NULL)
      {
      calptr = (struct calvals *)malloc(sizeof(struct calvals)*ncal);
      for(icnt=0;icnt<ncal;icnt++)
         {
         cst_rlst ( cpos+1, ',', IMISSD, 4, fvals, &i, &ier );
         cpos = strchr(cpos+1,';');

         calptr[icnt].minpx = fvals[0];
         calptr[icnt].maxpx = fvals[1];
         calptr[icnt].minval = fvals[2];
         calptr[icnt].maxval = fvals[3];
         if(icnt == 0)
            {
            minpx = fvals[0];
	    maxpx = fvals[1];
	    }
         else
            {
            if(fvals[0] < minpx) minpx = fvals[0];
            if(fvals[1] > maxpx) maxpx = fvals[1];
            }
         }
      }
   }

*iret = 0;

}

