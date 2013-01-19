#include "geminc.h"
#include "gemprm.h"

#include "rcm.h"

void get_xy(float clat, float clon, float *x, float *y)
{
int iret;
static char mapcord[]="M";
static char gridcord[]= "G";
static int numpoint=1;

gtrans  ( mapcord, gridcord, &numpoint, &clat, &clon, x, y, &iret,strlen(mapcord),strlen(gridcord));
}

void get_xy4(float clat, float clon,char *gbox, float *x, float *y)
{
float col,row;
char g1,g2,gsub;

get_xy(clat,clon,x,y);

col = floor(*x); row = floor(*y); row = row + 1; /* row increasing North! */

g1 = gbox[0];
g2 = gbox[1];
gsub = gbox[2];
row = row - (g1 - 'M');
col = col + (g2 - 'M');
switch(gsub - 'A')
   {
   case 0:
          break;
   case 1:
          row = row - .25;
          break;
   case 2:
          row = row - .5;
          break;
   case 3:
          row = row - .75;
          break;
   case 4:
          col = col + .25;
          break;
   case 5:
          col = col + .25;
          row = row - .25;
          break;
   case 6:
          col = col + .25;
          row = row - .5;
          break;
   case 7:
          col = col + .25;
          row = row - .75;
          break;
   case 8:
          col = col + .5;
          break;
   case 9:
          col = col + .5;
          row = row - .25;
          break;
   case 10:
          col = col + .5;
          row = row - .5;
          break;
   case 11:
          col = col + .5;
          row = row - .75;
          break;
   case 12:
          col = col + .75;
          break;
   case 13:
          col = col + .75;
          row = row - .25;
          break;
   case 14:
          col = col + .75;
          row = row - .5;
          break;
   case 15:
          col = col + .75;
          row = row - .75;
          break;
   default:
          printf("unknown subgrid %s\n",gbox);
   }
*x = col;
*y = row;
}


float lastrow=-1.0, lastcol=-1.0;

void plot_repeat(int color, float i, float j)
{
float x[5],y[5];
static int np=5;

if(color != 0)
   {
   y[0] = j; x[0] = i;
   y[1] = j-.25; x[1] = i;
   y[2] = j-.25; x[2] = i+.25;
   y[3] = j; x[3] = i+.25;
   y[4] = j; x[4] = i;
   box_fill(color, np, x, y);
   }

lastcol = i;
lastrow = j;
}



void plot_inten(char *gbox, int color, float i, float j)
{
float row,col,x[5],y[5];
char g1,g2,gsub;
float tmplat,tmplon;
static np=5;

col = floor(i); row = floor(j); row = row + 1; /* row increasing North! */

g1 = gbox[0];
g2 = gbox[1];
gsub = gbox[2];
row = row - (g1 - 'M');
col = col + (g2 - 'M');
switch(gsub - 'A')
   {
   case 0:
          break;
   case 1:
          row = row - .25;
          break;
   case 2:
          row = row - .5;
          break;
   case 3:
          row = row - .75;
          break;
   case 4:
          col = col + .25;
          break;
   case 5:
          col = col + .25;
          row = row - .25;
          break;
   case 6:
          col = col + .25;
          row = row - .5;
          break;
   case 7:
          col = col + .25;
          row = row - .75;
          break;
   case 8:
          col = col + .5;
          break;
   case 9:
          col = col + .5;
          row = row - .25;
          break;
   case 10:
          col = col + .5;
          row = row - .5;
          break;
   case 11:
          col = col + .5;
          row = row - .75;
          break;
   case 12:
          col = col + .75;
          break;
   case 13:
          col = col + .75;
          row = row - .25;
          break;
   case 14:
          col = col + .75;
          row = row - .5;
          break;
   case 15:
          col = col + .75;
          row = row - .75;
          break;
   default:
          printf("unknown subgrid %s\n",gbox);
   }

y[0] = row; x[0] = col;
y[1] = row-.25; x[1] = col;
y[2] = row-.25; x[2] = col+.25;
y[3] = row; x[3] = col+.25;
y[4] = row; x[4] = col;
if(color != 0)
   box_fill(color, np,x, y);

lastrow = y[0]; lastcol = x[0];

}


int COLOR_PRCP[10]={0,23,22,21,20,17,15,0,0,0};
int COLOR_CLAR[10]={0,24,25,26,27,28,29,0,0,0};

void grid_ggg(char *buf, float i, float j, int mode)
{
char gbox[4];
int icnt;
char *cpos;
int intensity,repeat;
int FIRST=0;

int color,np,iret;
float x[5],y[5];

gbox[0] = '\0';
strncat(gbox,buf,3);
cpos = buf + 3;

while(cpos[0] != 0)
   {
   if((cpos[0] >= '0')&&(cpos[0] <= '9'))
      {
      intensity = cpos[0] - '0';

      color = 1;
      if((mode == MDPCPN)&&(intensity >= 0)&&(intensity <= 9))
         color = COLOR_PRCP[intensity];
      else if ((mode == MDCLAR)&&(intensity >= 0)&&(intensity <= 9))
         color = COLOR_CLAR[intensity];

      if(FIRST == 0)
         {
         plot_inten(gbox,color,i,j);
         }
      else
         {
         i = lastcol+.25; j = lastrow;
         plot_repeat(color,i,j);
         }
      FIRST++;
      }
   else
      {
      repeat = (cpos[0] - 'A' + 1);
      for(icnt=0;icnt<repeat;icnt++)
         {
         i = lastcol+.25; j = lastrow;
         plot_repeat(color,i,j);
         }
      }
   cpos++;
   }

}


void	mdr_cbar(char *clrbar)
{
int iret;
static int ncol=6;
static float flevs[7]={ 15., 30., 40., 45., 50., 55., 60.};

gg_cbar(clrbar, &ncol, flevs, COLOR_PRCP, &iret, strlen(clrbar));
}
