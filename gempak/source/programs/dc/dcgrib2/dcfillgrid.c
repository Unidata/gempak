#include <geminc.h>
#include <gemprm.h>

#include <gbcmn.h>

#include "dcsgrid.h"
#include "dcgrib.h"

extern unsigned char *gdsstart; 
extern int idrct,jdrct,consec;

void dcfillgrid(int iflno, int *xgrid, int lengrd, float rmsval,
	 int iuscal, int ighdr[], char gdattm[][DTTMSZ], int level[2], 
	 int ivcord, char *parm, int *iret)
{
int m,npts;

int gx=gds.kx,gy=gds.ky;
int nbits=bds.num_bits;
float ref = bds.ref_value;
const int rewrit = G_TRUE;
const int ipktyp = MDGGRB;


if ( FGSIZ < gx * gy ) {
  FGRID = (float *) realloc (FGRID, gx * gy * sizeof (float));
  if ( FGRID == NULL ) {
     printf ("FGRID allocation error\n");
     FGSIZ = 0;
     return;
  }
  FGSIZ = gx * gy;
}

if ( TGSIZ < gx * gy ) {
  TMPGRD2 = (float *) realloc (TMPGRD2, gx * gy * sizeof (float));
  if ( TMPGRD2 == NULL ) {
     printf ("TMPGRD2 allocation error\n");
     TMPGRD2 = 0;
     return;
  }
  TGSIZ = gx * gy;
}






npts =  (int) ( floor ( ( ( (float)bds.length - 11.0 ) * 8.0 ) / (float)bds.num_bits) );

/*printf("look lenbytes %d lengrd %d npts %d\n",bds.length,lengrd,npts);*/

gb_unpk(xgrid, npts, bds.num_bits, TMPGRD2, iret );

if(*iret != 0) 
   return;

if(pds.isbms != 0) 
   {
   printf("have to implement bms section for filled grids\n");
   *iret = -1;
   return;
   }

m = 0;
while ( m < npts )
   {
   TMPGRD2[m] = (float) ( (ref +
            (TMPGRD2[m]*pow (2.0,(double)bds.binary_scale))) *
            pow (10.0,(double)(iuscal-pds.dec_scale)) );
   m++;
   }

switch(gds.grid_proj)
   {
   case 201: /* 201 Arakawa unpadded */
   case 202: /* 201 Arakawa filled */
   case 203: /* 203 Arakawa padded */ 
      if ( ( gds.kx * gds.ky ) <= npts ) /* already filled...has to be on word boundary....so npts could exceed im*in */
         for(m=0; m < gds.kx * gds.ky; m++) FGRID[m] = TMPGRD2[m];
      else if ( bds.length == 12 ) /* constant grid */
         for(m=0; m < gds.kx * gds.ky; m++ ) FGRID[m] = TMPGRD2[0];
      else
         {
         if ( gds.grid_proj == 203 )
            stagger_e ( TMPGRD2, FGRID, npts, gds.kx, gds.ky, parm, iret );
         else if ( gds.grid_proj == 201 )
            stagger ( TMPGRD2, FGRID, npts, gds.kx, gds.ky, parm, iret );
         else
            printf("Unknown stagger/fill for %d\n",gds.grid_proj);
         }
      break;
   default:
      printf("%d filling not implemented yet\n",gds.grid_proj);
      *iret = -60;
   }

if ( *iret != 0 )
   return;


if(nbits < 2) nbits = 2;

cgd_wpgd (&iflno, FGRID, &gx, &gy, ighdr, gdattm[0], gdattm[1], &level[0], &level[1],
      &ivcord, parm, &rewrit, &ipktyp, &nbits, iret );
}
