#include "geminc.h"
#include "gemprm.h"


static int dith_pat=0;

void set_dither(int *idither)
{
dith_pat = *idither;
}

void dither(char *sys, int np, float x[], float y[], int isub)
{
int i;
float xstart=99999,xend=-99999,ystart=99999,yend=-99999;
float x1,y1,icnt,jcnt;
int alti;
int imark=15,ihw=2,iwid=1;
float isiz=1.0;
int ipt=1,iret;
float dx,dy;

gsmrkr(&imark,&ihw,&isiz,&iwid,&iret);
for(i=0;i<np;i++)
   {
   if(x[i] < xstart) xstart=x[i];
   if(x[i] > xend) xend=x[i];
   if(y[i] < ystart) ystart=y[i];
   if(y[i] > yend) yend=y[i];
   }
dx = xend - xstart;
dy = yend - ystart;

/* special case of 1 point */
if(isub == 1)
   {
   x1 = (xstart + xend) / 2.0;
   y1 = (ystart + yend) / 2.0;
   gmark(sys,&ipt,&x1,&y1,&iret,strlen(sys));
   return;
   }

alti = 0; 
for(jcnt=0;jcnt<=isub;jcnt++)
   {
   y1 = ystart + (jcnt * (dy / (float)isub));
   for(icnt=0;icnt<=isub;icnt++)
      {
      x1 = xstart + (icnt * (dx / (float)isub)) + (alti * dx / (2.0*(float)isub));
      if(x1 <= xend)
         {
         /*printf("look mark %f %f, %f %f [%d]\n",icnt,jcnt,x1,y1,alti);*/
         gmark(sys,&ipt,&x1,&y1,&iret,strlen(sys));
         }
      }
   alti = (alti + 1) % 2;
   }

}

void box_fill(int color, int np, float x[], float y[])
{
int iret;
static char sys[]="G";

gscolr(&color,&iret);

switch(dith_pat)
   {
   case 0:
      gfill(sys,&np,x,y,&iret,strlen(sys));
      break;
   case 1:
   case 2:
   case 3:
   case 4:
      dither(sys,np,x,y,dith_pat);
      break;
   default:
      gline(sys,&np,x,y,&iret,strlen(sys));
   }
}
