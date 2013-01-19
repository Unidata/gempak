#include <stdio.h>
#include <stdlib.h>

void rotate(dp,nx,ny,scan)
float *dp;
int *nx,*ny,scan;
{
int length,i;
int idrct,jdrct,consec;
int ibeg,iinc,jbeg,jinc,kcnt,idxarr,icnt,jcnt;
float *tmpgrd;

length = (*nx) * (*ny);
tmpgrd  = (float *) malloc ( length * sizeof ( float ) );

for ( i = 0; i < length; i++ ) 
   {
   tmpgrd[i] = dp[i];
   dp[i]  = 0;
   }

idrct  = (scan >> 7) & 1;
jdrct  = (scan >> 6) & 1;
consec = (scan >> 5) & 1;

if ( idrct == 0 ) 
   {
   ibeg = 0;
   iinc = 1;
   }
else 
   {
   ibeg = *nx - 1;
   iinc = -1;
   }

if ( jdrct == 1 ) 
   {
   jbeg = 0;
   jinc = 1;
   }
else 
   {
   jbeg = *ny - 1;
   jinc = -1;
   }

kcnt = 0;
if ( consec == 1 ) 
   {
   for ( jcnt=jbeg; (0<=jcnt&&jcnt<*ny); jcnt+=jinc ) 
      {
      for ( icnt=ibeg; (0<=icnt&&icnt<*nx); icnt+=iinc ) 
         {
         idxarr = *ny * icnt + jcnt;
         dp[kcnt] = tmpgrd[idxarr];
         kcnt++;
         }
      }
   }
else 
   {
   for ( jcnt=jbeg; (0<=jcnt&&jcnt<*ny); jcnt+=jinc ) 
      {
      for ( icnt=ibeg; (0<=icnt&&icnt<*nx); icnt+=iinc ) 
         {
         idxarr = *nx * jcnt + icnt;
         dp[kcnt] = tmpgrd[idxarr];
         kcnt++;
         }
      }
   }

free(tmpgrd);
}

void staggerv(dp,fg,gx,gy)
float *dp,*fg;
int gx,gy;
{
int ih,ihv;
int i,j,ioff;
int idim;
int ixp1,ixm1,iyp1,iym1;
int indx,ip1,im1;

idim = gx*gy;

/* load in H's every other point */
ih = 0;
for(ihv=0;ihv<idim;ihv+=2)
   {
   fg[ihv] = dp[ih];
   ih += 1;
   }

/*  Now fill in v points along the bottom and top */
for(j=0;j<2;j++)
   {
   if(j == 0) 
      ioff = 0;
   else
      ioff = gx * (gy - 1);
   for(i=1;i<gx;i+=2)
      {
      ihv = ioff + i;
      fg[ihv] = .5 * (fg[ihv-1] + fg[ihv+1]);
      }
   }

/* Now fill in v points along the sides */
ihv = 0;
for(j=1;j<gy;j+=2)
   {
   for(i=0;i<2;i++)
      {
      if(i == 0)
         ihv = ihv + gx;
      else
         ihv = ihv + gx - 1;
      ip1 = ihv + gx;
      im1 = ihv - gx;
      fg[ihv] = .5 * (fg[im1] + fg[ip1]);
      }
   ihv += 1;
   }

/* Finally fill in v points in the interior */
for(i=1;i<=(gx-2);i++)
   {
   for(j=1;j<=(gy-2);j++)
      {
      indx = j * gx + i;
      if((indx%2) != 0)
         {
         iyp1 = indx + gx;
         iym1 = indx - gx;
         ixp1 = indx + 1;
         ixm1 = indx - 1;
         fg[indx] = .25 * (fg[iyp1] + fg[iym1] +
                           fg[ixp1] + fg[ixm1] );
         }
      }
   }

}

void staggerh(dp,fg,gx,gy)
float *dp,*fg;
int gx,gy;
{
int iv,ivh;
int idim;
int i,j;
int ioff,indx;
int ip1,im1,ixp1,ixm1,iyp1,iym1;

idim = gx*gy;

/* load in V's every other point */
iv = 0;
for(ivh=1;ivh<=(idim-2);ivh+=2)
   {
   fg[ivh] = dp[iv];
   iv += 1;
   }

/* Fill h's along top and bottom */
for(j=0;j<2;j++)
   {
   if(j == 0) 
      ioff = 0;
   else
      ioff = gx * (gy - 1);
   for(i=2;i <= (gx - 3);i+=2)
      {
      ivh = ioff + i;
      fg[ivh] = .5 * (fg[ivh - 1] + fg[ivh + 1]);
      }
   }

/* Fill h's along sides */
ivh = gx;
for(j=2;j<=(gy-3);j+=2)
   {
   for(i=0;i<2;i++)
      {
      if(i == 0)
         ivh  = ivh + gx;
      else
         ivh = ivh + gx - 1;
      ip1 = ivh + gx;
      im1 = ivh - gx;
      fg[ivh] = .5 * (fg[im1] + fg[ip1]);
      }
   ivh += 1;
   }

/* Fill h's in interior */
for(i=1;i<=(gx-2);i++)
   {
   for(j=1;j<=(gy-2);j++)
      {
      indx = j * gx + i;
      if((indx%2) == 0)
         {
         iyp1 = indx + gx;
         iym1 = indx - gx;
         ixp1 = indx + 1;
         ixm1 = indx - 1;
         fg[indx] = .25 * (fg[iyp1] + fg[iym1] + fg[ixp1] + fg[ixm1]);
         }
      }
   }

/* extrapolate corner points */
fg[0] = .5 * (1.5 * (fg[1] + fg[gx]) - .5 * (fg[3] + fg[3*gx]));

fg[gx-1] = .5 * (1.5 * (fg[gx-2] + fg[2*gx-1]) -
              .5 * (fg[gx-4] + fg[4*gx-1]));

indx = gx * (gy - 1);

fg[indx] = .5 * (1.5 * (fg[indx+1] + fg[indx-gx]) -
              .5 * (fg[indx+3] + fg[indx-3*gx]));

indx = idim - 1;

fg[indx] = .5 * (1.5 * (fg[indx-1] + fg[indx-gx]) -
              .5 * (fg[indx-3] + fg[indx-3*gx]));

}

#define	UREL	33
#define VREL	34
float* stagger_fill(dp, gx, gy, parm, grid, scan)
float *dp;
int *gx,*gy;
int parm;
int grid;
unsigned char scan;
{
int ngx,ngy,i;
float *fg;

switch(grid)
   {
   case 94:
            ngx = 361;
            ngy = 271;
            break;
   case 96:
            ngx = 319;
            ngy = 261;
            break;
   case 99:
            ngx = 311;
            ngy = 295;
            break;
   case 192:
   case 196:
            ngx = 301;
            ngy = 305;
            break;
   default:
            printf("unknown stagger grid %d\n",grid);
            return(dp);
   }

fg = (float *)malloc( 2 * (*gx * *gy) * sizeof(float));

if((parm == UREL) || (parm == VREL))
   staggerh(dp,fg,ngx,ngy);
else
   staggerv(dp,fg,ngx,ngy);

*gx = ngx;
*gy = ngy;

return(fg);



}
