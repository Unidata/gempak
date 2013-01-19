#include <stdio.h>
#include "gbcmn.h"

/*

The new 'native' Eta grid representation in GRIB follows the 2-D grid indexing that has been used by the model for some time. This is still staggered
row by row, wind and mass, but the same number of points are in each row. This means that the number of points on the grid is now IM*JM (rather than
IM*JM-JM/2). The "extra" point now included on every other row is not used during integration. 

     
      Staggered, 2-D E-grid indexing
        
   3   H   V   H   V   H   V   H  (V)

J  2   V   H   V   H   V   H   V  (H)

   1   H   V   H   V   H   V   H  (V)      () points are "extra"
        \ /     \ /     \ /     \ /        included in grid but not
         1       2       3       4         used in integration
                     I

      Data Representation Type 203 - Arakawa staggered E-grid on rotated
                                     latitude/longitude grid

       bytes:       definition:
        7-8         Ni -  # points in each row
        9-10        Nj -  # of rows (# of points in Y direction)
       11-13        La1 - latitude of first grid point (millideg)
       14-16        Lo1 - longitude of first grid point (millideg)
         17         Resolution and component flags
       18-20        La2 - central latitude (millideg)
       21-23        Lo2 - central longitude (millideg)
       24-25        Di - Longitudinal direction increment (millideg)
       26-27        Dj - Latitudinal direction increment (millideg)
         28         Scanning mode flags
       29-32        Reserved (zero)

      Grid
     #192  Arakawa staggered E-grid on rotated latitude/longitude
           grid (Used by the 32 km Eta Model)

           Ni=223
           Nj=365
           La1=0.407N
           Lo1=215.906E (144.094W)
           Res & Comp. Flag = 1 0 0 0 1 0 0 0
           La2=50.000N
           Lo2=253.000E (107.000W)
           Di=222 millidegrees (exactly 2/9 deg)
           Dj=205 millidegrees (exactly 8/39 deg)
           Scanning Mode= 01000000

      Grid
     #190  Arakawa staggered E-grid on rotated latitude/longitude
           grid (Used by the 80 km Eta Model, backup if C90 fails)

           Ni=92
           Nj=141
           La1=0.182N
           Lo1=210.113E (149.887W)
           Res & Comp. Flag = 1 0 0 0 1 0 0 0
           La2=52.000N
           Lo2=249.000E (111.000W)
           Di=577 millidegrees (exactly 15/26 deg)
           Dj=538 millidegrees (exactly 14/26 deg)
           Scanning Mode= 01000000
*/

void rotate_e(dp,nx,ny,scan)
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

void staggerv_e(dp,fg,gx,gy)
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

void staggerh_e(dp,fg,gx,gy)
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
float* stagger_fill_e(dp, gx, gy, parm, grid, scan, Ni, Nj,gdsloc)
float *dp;
int *gx,*gy;
int parm;
int grid;
unsigned char scan;
int Ni, Nj;
unsigned char *gdsloc;
{
int ngx,ngy,i,length;
double dx,dy;
float *fg;

ngx = Ni; ngy = Nj;
gds.latll = ((double)gb_btoi(gdsloc, 10, 3, TRUE))/1000.;
gds.lonll = ((double)gb_btoi(gdsloc, 13, 3, TRUE))/1000.;
dx = ((double)gb_btoi(gdsloc, 23, 2, TRUE))/1000.;
dy = ((double)gb_btoi(gdsloc, 25, 2, TRUE))/1000.;

gds.latur = gds.latll + (ngy - 1)*dy;
gds.lonur = gds.lonll + (ngx - 1)*dx;

gds.angle1 = ((double)gb_btoi(gdsloc, 17, 3, TRUE))/1000.;
gds.angle2 = ((double)gb_btoi(gdsloc, 20, 3, TRUE))/1000.;
gds.angle3 = 0;


/*printf("look gds %d %d %d %d %f %f\n",ngx,ngy,*gx,*gy,dx,dy);
printf("grid %d %f %f %f %f %f %f %f\n",
gds.grid_proj,
(float)gds.latll,
(float)gds.latur,
(float)gds.lonll,
(float)gds.lonur,
gds.angle1,
gds.angle2,
gds.angle3);
*/

/*
printf("octet %d\n",gb_btoi(gdsloc, 0, 3, FALSE));
printf("octet %d\n",gb_btoi(gdsloc, 5, 1, FALSE));
printf("octet %d\n",gb_btoi(gdsloc, 6, 2, FALSE));
printf("octet %d\n",gb_btoi(gdsloc, 8, 2, FALSE));
printf("octet %d\n",gb_btoi(gdsloc, 17, 3, TRUE));
printf("octet %d\n",gb_btoi(gdsloc, 20, 3, TRUE));
printf("octet %d\n",gb_btoi(gdsloc, 10, 3, TRUE));
printf("octet %d\n",gb_btoi(gdsloc, 13, 3, TRUE));
*/

if((ngx == *gx)&&(ngy == *gy))
   {
   /* don't have to fill this */
   fg = (float *)malloc((*gx * *gy) * sizeof(float));
   for(i=0;i<(*gx * *gy);i++)
      fg[i] = dp[i];
   return(fg);
   }

printf("oops, haven't done this yet\n");
return(NULL);

if((parm == UREL) || (parm == VREL))
   staggerh_e(dp,fg,ngx,ngy);
else
   staggerv_e(dp,fg,ngx,ngy);

*gx = ngx;
*gy = ngy;

return(fg);



}
