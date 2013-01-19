#include <geminc.h>
#include <gemprm.h>

#include <gbcmn.h>
#include <dccmn.h>
#include "dcgrib.h"

extern int idrct,jdrct,consec;
extern int ensext;

void dcwppg ( int iflno, int *xgrid, int subgrid)
{
int lengrd,iret;
int ighdr[LLGDHD];
int itime[3],iaccm,ilevel[2];
char gdattm[2][DTTMSZ],parm[13];
int level[2],ivcord,iuscal;
int pdsextflag;
float rmsval;
int lens,i;
float scale;
float difval=0, ref;
int loglev,numerr;
int nocoord=0, noparm=0;
int _itrue = TRUE, _ifalse = FALSE, packmeth=MDGGRB;
int reindex=0;
int _ihzrmp, _idrct;
static char errgrp[]="DCGRIB";
char errstr[LLMXLN],vcoord[20];


for(i=0;i<LLGDHD;i++) ighdr[i] = 0;

gb_ftim(itime,&iaccm,&iret);

gb_vlev(ilevel,&iret);

/* Default is to add the pdsextflag for grib1 */
if ( ensext != 0 )
   pdsextflag = TRUE;
else
   pdsextflag = FALSE;

/* old fortran call 
na_rhdr ( itime, &iaccm, ilevel, &pds.vcoord, &pds.parameter, &pds.version, &pdsextflag,
          gdattm, level, &ivcord, parm, &iuscal, &rmsval, &pds.pdse, pds.extension, 
	  &_ihzrmp, &_idrct, &iret,
          sizeof(gdattm[0]),sizeof(parm),strlen(pds.extension) );
*/

na_rhdr ( itime, &iaccm, &ilevel[0], &ilevel[1], &pds.vcoord, &pds.parameter, 
	&pds.version, &pdsextflag, gdattm[0], gdattm[1], &level[0], &level[1],
	&ivcord, parm, &iuscal, &rmsval, &pds.pdse, pds.extension,
	&_ihzrmp, &_idrct, &iret);

/* new interpolation flags are placed in first 2 elements of ighdr 3/05 */
ighdr[0] = _ihzrmp;
ighdr[1] = _idrct;


if(iret == 0)
   {
   lengrd = (bds.length - 11) / sizeof(int);
   if(((bds.length - 11) % sizeof(int)) > 0) lengrd = lengrd + 1;

   ref = bds.ref_value * pow ( 10.0, (double) (iuscal - pds.dec_scale) );
   scale  = pow ( 2.0, (double) bds.binary_scale ) * pow ( 10.0, (double) (iuscal - pds.dec_scale) );

   if((idrct != 0)||(jdrct != 1)||(consec != 0)) reindex = 1;

   if(gds.grid_proj == 203)
      dcfillgrid(iflno,xgrid,lengrd,rmsval,iuscal, ighdr, gdattm, level, ivcord, parm, &iret);
   else if((subgrid != 0)||(pds.isbms)||(bds.num_bits<2)||(reindex != 0))
      dcsubgrid(iflno,xgrid,lengrd,rmsval,iuscal, ighdr, gdattm, level, ivcord, parm, subgrid);
   else
      cgd_wppg ( &iflno, xgrid, &lengrd, &gds.kx, &gds.ky, ighdr, gdattm[0], gdattm[1], 
		&level[0], &level[1], &ivcord, parm, &_itrue, &packmeth, &bds.num_bits, 
		&_ifalse, &ref, &scale, &difval, &iret );
   numerr = iret;
   loglev = 1;
   }
else
   {
   /* 3 no parm found in table */
   /* 2 no vert found */
   if(iret == 3) noparm = 1;
   if(iret == 2) nocoord = 1;
   numerr = iret;
   loglev = 0;
   }

errstr[0] = '\0';
if(noparm != 0)
   sprintf(errstr,"%03d\0",pds.parameter);
else
   {
   cst_ncat(errstr, parm, &lens, &iret);
   }

strncat(errstr," ",1);

cst_ncat(errstr, gdattm[0], &lens, &iret);
strncat(errstr," ",1);

if(nocoord != 0)
   sprintf(errstr+strlen(errstr),"%03d\0",pds.vcoord);
else
   {
   lv_ccrd(&ivcord,vcoord,&iret,sizeof(vcoord) - 1);
   vcoord[sizeof(vcoord) - 1] = '\0';
   cst_rmbl(vcoord, vcoord, &lens, &iret);
   cst_ncat(errstr, vcoord, &lens, &iret);
   }

sprintf(errstr+strlen(errstr)," %4d %4d %3d\0",level[0],level[1],pds.grid_id);

dc_wclg(loglev,errgrp,numerr,errstr,&iret);

}

