#include <geminc.h>
#include <gemprm.h>

#include <gbcmn.h>
#include "dcsgrid.h"
#include "dcgrib.h"

extern unsigned char *bmsstart;
extern int isquasi,qnumpts,qnumrows,qrowpts[100];
extern int idrct,jdrct,consec;

void dcsubgrid( int iflno, int *xgrid, int lengrd, float rmsval,
	 int iuscal, int ighdr[], char gdattm[][DTTMSZ], int level[2], 
	 int ivcord, char *parm, int subgrid)
{
float ref;
int nbits;
static unsigned int *misarr=NULL;
static size_t	misarr_siz=0;
int misflg;
int iret;
int i,m,n,kxky,npts,maxpts;
int bmshdr,jshft,bitmaplength;
unsigned char *bmptr, *indxb, kbit=0; 
int aghdr[LLGDHD];
int isinternational = FALSE;
int intlstart=0,intlstop=0;

int ip,ipxy,ngx,ngy;
const int rewrit = G_TRUE;
const int ipktyp = MDGGRB;

if(isquasi == 1) 
   kxky = qnumpts;
else
   kxky = gds.kx * gds.ky;

if ( subgrid != 0 ) {
   maxpts = gtype[sgridval].nx * gtype[sgridval].ny;
   if ( kxky > maxpts ) maxpts = kxky;
} 
else
   maxpts = kxky;

if ( FGSIZ < maxpts ) {
  FGRID = (float *) realloc (FGRID, maxpts * sizeof (float));
  if ( FGRID == NULL ) {
     printf ("subgrid %d FGRID allocation error %d %d %d\n",subgrid,kxky,maxpts,FGSIZ);
     FGSIZ = 0;
     return;
  }
  FGSIZ = maxpts;
}


if ( TGSIZ < maxpts ) {
  TMPGRD2 = (float *) realloc (TMPGRD2, maxpts * sizeof (float));
  if ( TMPGRD2 == NULL ) {
     printf ("TMPGRD2 allocation error\n");
     TMPGRD2 = 0;
     return;
  }
  TGSIZ = maxpts;
}


/* SH international */
if((pds.grid_id == 26)||(pds.grid_id == 23)||(pds.grid_id == 24)||
   (pds.grid_id == 63)||(pds.grid_id == 64))
   {
   isinternational = TRUE;
   intlstart = 1; intlstop = gds.kx-1;
   }

/* NH international */
if((pds.grid_id == 25)||(pds.grid_id == 21)||(pds.grid_id == 22)||
   (pds.grid_id == 61)||(pds.grid_id == 62))
   {
   isinternational = TRUE;
   intlstart = kxky-gds.kx+1; intlstop = kxky-1;
   }


ref = bds.ref_value;
nbits = bds.num_bits;

if(pds.isbms != 0)
   {
   misflg = TRUE;
   if ( kxky > misarr_siz ) 
      {
      misarr = (unsigned int *)realloc(misarr, kxky * sizeof(unsigned int));
      if ( misarr == NULL ) 
	 {
         misarr_siz=0;
	 iret = -19;
	 return;
	 }
      misarr_siz = (size_t)kxky; 
      }
   for(i = 0; i < kxky; i++ ) misarr[i] = 1;

   if ( rmsval == RMISSD ) nbits = bds.num_bits + 1;

   bmshdr = 6;
   bitmaplength = bmslength - bmshdr;

   if ( bms.table == 0 ) 
      {
      bmptr = ( unsigned char * )
             malloc ( bitmaplength * sizeof ( unsigned char * ) );
      indxb = bmptr;
      memcpy ( bmptr, bmsstart+bmshdr, bitmaplength );
      }
   else 
      {
      /* copied from gbgubd.c
       *    Load the predetermined bit map provided by the center.
       */
      iret = -18;
      return;
      }

   /*
    *          Convert the bitmap to an integer array.
    */
   npts = 0;
   for ( i = 0; i < kxky; i++ ) 
      {
      jshft = i % 8;
      if(i < (bitmaplength*8))
         {
         /* intnl bit maps have kxky - gds.kx + 1 points */
         if ( jshft == 0 ) 
            {
            kbit = *indxb;
            indxb++;
            }
         misarr[i] = ( ( kbit >> ( 8 - jshft - 1 ) ) & 1 ) ;
         }
      else
         misarr[i] = 0;
      if ( misarr[i] == 1 ) npts++;
      }
   if(bms.table == 0) free ( bmptr );
   }
else
   {
   misflg = FALSE;
   if(isinternational != TRUE)
      npts = kxky; /* changed if bit mapped */
   else
      npts = kxky - gds.kx + 1;
   }


gb_unpk(xgrid, npts, bds.num_bits, TMPGRD2, &iret );

m = 0;
n = 0;
while ( m < kxky ) 
   {
   if(n < npts)
      {
      if ( misflg == TRUE && misarr[m] == 0 )
         {
         FGRID[m] = rmsval;
         }
      else {
         FGRID[m] = (float) ( (ref +
            (TMPGRD2[n]*pow (2.0,(double)bds.binary_scale))) *
            pow (10.0,(double)(iuscal-pds.dec_scale)) );
         n++;
         }
      }
   else
      FGRID[m] = rmsval;
   m++;
   }

if(isinternational == TRUE)
   {
   n = 0;
   for(m=0;m<kxky;m++)
      {
      if((m >= intlstart)&&(m <= intlstop))
         TMPGRD2[m] = FGRID[n-1];
      else
         {
         TMPGRD2[m] = FGRID[n];
         n++;
         }
      }
   for(m=0;m<kxky;m++)
      FGRID[m] = TMPGRD2[m];
   }

/* do stagger */
/* do quasi */
if(isquasi==1)
   {
   /*
   ** Produce a rectangular grid filled from quasi interpolation 
   */
   memcpy(TMPGRD2, FGRID, qnumpts*sizeof(float) );
   qlin(qnumrows,qrowpts,TMPGRD2,gds.kx,gds.ky,FGRID);
   }

/* check scanning mode */
if((idrct != 0)||(jdrct != 1)||(consec != 0))
   {
   int ibeg,iinc,jbeg,jinc,icnt,jcnt,kcnt,idxarr;

   /*printf("have to reindex grid %d %d [%d %d %d]\n",gds.kx,gds.ky,idrct,jdrct,consec);*/

   /* from gb_gubd */
            if ( idrct == 0 ) {
                ibeg = 0;
                iinc = 1;
            }
            else {
                ibeg = gds.kx - 1;
                iinc = -1;
            }
            if ( jdrct == 1 ) {
                jbeg = 0;
                jinc = 1;
            }
            else {
                jbeg = gds.ky - 1;
                jinc = -1;
            }
            kcnt = 0;
            if ( consec == 1 ) {
                for ( jcnt=jbeg; (0<=jcnt&&jcnt<gds.ky); jcnt+=jinc ) {
                    for ( icnt=ibeg; (0<=icnt&&icnt<gds.kx); icnt+=iinc ) {
                        idxarr = gds.ky * icnt + jcnt;
                        TMPGRD2[kcnt] = FGRID[idxarr];
                        kcnt++;
                    }
                }
            }
            else {
                for ( jcnt=jbeg; (0<=jcnt&&jcnt<gds.ky); jcnt+=jinc ) {
                    for ( icnt=ibeg; (0<=icnt&&icnt<gds.kx); icnt+=iinc ) {
                        idxarr = gds.kx * jcnt + icnt;
                        TMPGRD2[kcnt] = FGRID[idxarr];
                        kcnt++;
                    }
                }
            }
   for(m=0;m<kxky;m++) FGRID[m] = TMPGRD2[m];

   }

if(nbits < 2) nbits = 2;

if(subgrid != 0)
   {
   /* see if this is part of the same grid as previously written */
   dcchecksub ( iflno, pds.center, pds.process, level, ivcord, 
	gdattm, parm, gtype[sgridval].nx, gtype[sgridval].ny, &iret);

   if( iret != 0 )
      {
      /* read in complete grid from decoded file */
      cgd_rdat ( (const int *)&iflno, (const char *)gdattm[0], (const char *)gdattm[1],
		(const int *)&level[0], (const int *)&level[1], (const int *)&ivcord,
		(const char *)parm, TMPGRD2, &ngx, &ngy, aghdr, &iret);
      }
   else
      dclastsub ( TMPGRD2, &ngx, &ngy, &iret);

   /* place sub grid in grid*/

   if(iret != 0)
      {
      if(iret != -12)
         printf("look at iret %d %d %d // %d %d// %s\n",iret,ngx,ngy,level[0],level[1],parm);
      for(i=0; i < (gtype[sgridval].nx*gtype[sgridval].ny);i++) TMPGRD2[i] = RMISSD;
      }

   /* fill in grid points N-S/S-N*/
   ip = 0;
   for(m=1; m <= gds.ky; m++)
      {
      ipxy = (m+gtype[sgridval].srow-2)*gtype[sgridval].nx + gtype[sgridval].scol-1;
      for(n=1; n <= gds.kx; n++)
         {
         TMPGRD2[ipxy+n-1] = FGRID[ip];
         ip++;
         } 
      }


   /* write complete grid */
   /*dcwpgd (&iflno, TMPGRD2, &gtype[sgridval].nx, &gtype[sgridval].ny, ighdr,
        gdattm, level, &ivcord, parm, &nbits, &iret,20,12);*/
   cgd_wpgd (&iflno, TMPGRD2, &gtype[sgridval].nx, &gtype[sgridval].ny, ighdr,
        gdattm[0], gdattm[1], &level[0], &level[1], &ivcord, parm, &rewrit,
	&ipktyp, &nbits, &iret);

   /* save grid in cache */
   dcsubsav ( TMPGRD2, gtype[sgridval].nx, gtype[sgridval].ny );
   }
else
   {
   /*dcwpgd (&iflno, FGRID, &gds.kx, &gds.ky, ighdr, gdattm, level,
      &ivcord, parm, &nbits, &iret,20,12);*/
   cgd_wpgd (&iflno, FGRID, &gds.kx, &gds.ky, ighdr, gdattm[0], gdattm[1], 
	&level[0], &level[1], &ivcord, parm, &rewrit, &ipktyp, &nbits, &iret);
   }

}
