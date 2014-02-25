#include <geminc.h>
#include <gemprm.h>
#include <dccmn.h>

#ifdef UNDERSCORE
#define dc_fcls	dc_fcls_
#define dc_fint	dc_fint_
#define st_c2i	st_c2i_
#define nldninit	nldninit_
#endif

#include "dcnldn.h"

void    dc_fint ( int *numfil, int *itype, char *parmfl, int *iret, size_t);
void    dc_fcls ( int *iret);
void	st_c2i ( char *string, int *nexp, int *intarr, int *num, int *iret, size_t);

void	decode_nldn (char *curtim, char *gemfil, char *prmfil, char *filetyp, 
		int maxtim, int nhours, int *iret)
{
int maxfil=1, iflsrc=2;
int ier,ichk,nexp,nret,loglev;
nldn_file ltgf;
char errmsg[80];
static char errgrp[] = {"decode_nldn"};

nldninit();

*iret = 0;

ltgf.gemfil = (char *)malloc(strlen(gemfil) + 1);
strcpy(ltgf.gemfil,gemfil);

ltgf.curtim = (char *)malloc(strlen(curtim) + 1);
cst_uclc(curtim, ltgf.curtim, &ier);

ltgf.ifltyp = (char *)malloc(strlen(filetyp) + 1);
cst_uclc(filetyp, ltgf.ifltyp, &ier);

ltgf.maxtim = maxtim;
ltgf.iflsrc = iflsrc;

/*
 * File types can be hour, day, or month, for hourly, daily, or monthly files
 *      OR can be minute stored in xx minute bins.
 */
ltgf.itype = 0; ltgf.ifactor = 0;
if (strncmp(filetyp,"hour",4) == 0) 
   ltgf.itype = 1;
else if (strncmp(filetyp,"day",3) == 0) 
   ltgf.itype = 2;
else if (strncmp(filetyp,"month",5) == 0) 
   ltgf.itype = 3;
else if (strncmp(filetyp,"minute",6) == 0) 
   {
   ltgf.itype = 4; nexp = 1;
   st_c2i ( filetyp+6, &nexp, &ltgf.ifactor, &nret, &ier, 2);
   if ( ier != 0 ) 
      {
      sprintf(errmsg,"Can't decode bin size, use 10 min as default\0");
      loglev = 0;
      dc_wclg ( loglev, errgrp, ier, errmsg, &ichk);
      ltgf.ifactor = 10;
      }
   }
/* use the -b nhours to specify the output file frequency (60 min default)*/
ltgf.ibin = nhours;


dc_fint(&maxfil, &iflsrc, prmfil, &ier, strlen(prmfil) );

while(*iret == 0)
   dcnldn_input(ltgf,iret);

dc_fcls(&ier);
}
