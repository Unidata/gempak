#include <geminc.h>
#include <gemprm.h>

#include <gbcmn.h>
#include <dccmn.h>

#include "dcgrib.h"


unsigned char *bmsstart;
unsigned char *gdsstart;
int isquasi;
int idrct, jdrct, consec;

void
decode_grib1 (unsigned char *gribbul, int lenbul,
	      char *gemfil, char *filnam, int maxgrd, int *iret)
{
/************************************************************************
* void decode_grib ( gribbul, lenbul, gemfil, filnam, maxgrd, iret)	*
*									*
* unsigned char	*gribbul	grib bulletin				*
* int		lenbul		length of input grib bulletin		*
* char		*gemfile	output gempak grid file name		*
* int   	maxgrd		maximum number of grids in output file	*
* int  		*iret		Error code return			*
*									*
* Error Codes								*
*				-51	failed to read grib tables	*
*				-52	grdnav.tbl error		*
*				-53	file template not available	*
*				-55	number of gridpoints too large	*
* Log:									*
* Chiz/Unidata			12/99	Created				*
* Chiz/Unidata			 2/04	Made subroutine for GRIB1	*
*************************************************************************/
  int numgrds = maxgrd;

  int kxky, npts;
  int special_grid;

  int iflno = -1, ier;
  int numerr, loglev;
  char wmotbl[] = "?", ntbl[] = "?", vtbl[] = "?", ctbl[] = "?";
  char errstr[80];
  static char errgrp[] = "DCGRIB";


/*
** Initialize grib parameters for this product
*/
  pdslength = 0;
  gdslength = 0;
  bdslength = 0;
  bmslength = 0;
  isquasi = 0;
  idrct = 0;
  jdrct = 1;
  consec = 0;

  /*
   * decode grib1 bulletins here (todo: move to decode_grib1 subroutine)
   */
  gb_ids (gribbul);
  gb_pds (gribbul + 8);

  pdslength = pds.length;
  if (pds.isgds != 0)
    {
      gdsstart = gribbul + 8 + pdslength;
      gb_gds (gribbul + 8 + pdslength);

      if (gds.grid_proj == 203)	/* call gb_ltln since not done in gb_gds */
	{
	  gb_stage (gribbul + 8 + pdslength);
	}

      idrct = (gds.scan_mode >> 7) & 1;
      jdrct = (gds.scan_mode >> 6) & 1;
      consec = (gds.scan_mode >> 5) & 1;
      gdslength = gds.length;
    }
  else
    {
      /* use grdnav.tbl */
      if ((*iret = dcgnav ()) != 0)
	{
	  loglev = 0;
	  numerr = *iret;
	  sprintf (errstr, "Grid Navigation error\0");
	  dc_wclg (loglev, errgrp, numerr, errstr, &ier);
	  *iret = -52;
	  return;
	}
    }

  if ((pds.grid_id >= 37) && (pds.grid_id <= 44))
    {
      isquasi = 1;
      dcquasi (gds.ky, gribbul + 8 + pdslength + gds.PV - 1, &npts);
      gds.kx = 73;
    }

  if (pds.isbms != 0)
    {
      gb_bms (gribbul + 8 + pdslength + gdslength);
      bmsstart = gribbul + 8 + pdslength + gdslength;
      bmslength = bms.length;
    }

/*
** kxky for quasi thinned grids is npts in bds
*/
  kxky = gds.kx * gds.ky;
  if (!isquasi && kxky > XGSIZ) {
      sprintf (errstr, "Reallocating grid for %d points [nx %d ny %d]\0",
	   kxky, gds.kx, gds.ky);
      loglev = 4;
      numerr = 0;
      dc_wclg (loglev, errgrp, numerr, errstr, &ier);
      XGRID = (int *) realloc (XGRID, kxky * sizeof (int));
      if ( XGRID == NULL ) {
         loglev = 0;
         numerr = -55;
         sprintf (errstr, "Grid too large %d x %d [%d > %d]\0",
	       gds.kx, gds.ky, kxky, XGSIZ);
         dc_wclg (loglev, errgrp, numerr, errstr, &ier);
	 XGSIZ = 0;
         return;
      }
      else
	XGSIZ = kxky;
  }
  else if ( isquasi && npts > XGSIZ) {
    XGRID = (int *) realloc (XGRID, npts * sizeof (int));
    if ( XGRID == NULL ) {
         loglev = 0;
         numerr = -55;
         sprintf (errstr, "Quasi Grid too large %d x %d [%d > %d]\0",
               gds.kx, gds.ky, npts, XGSIZ);
         dc_wclg (loglev, errgrp, numerr, errstr, &ier);
         XGSIZ = 0;
         return;
      }
  }

  gb_bds (gribbul + 8 + pdslength + gdslength + bmslength, kxky, XGRID);
  bdslength = bds.length;

  na_rtbl (&ids.edition, &pds.center, &pds.version, wmotbl, ntbl, vtbl, ctbl, iret);
  if (*iret == 0)
    {
      loglev = 3;
      numerr = *iret;
      sprintf (errstr, "grib tables [cntr %d edtn %d vers %d]\0",
	       pds.center, ids.edition, pds.version);
      dc_wclg (loglev, errgrp, numerr, errstr, &ier);
    }
  else
    {
      loglev = 0;
      *iret = -51;
      numerr = *iret;
      sprintf (errstr, "can't open grib tables [NA %d], set GEMTBL\0", *iret);
      dc_wclg (loglev, errgrp, numerr, errstr, &ier);
      return;
    }

  if (*iret != 0)
    {
      printf ("should never get here with *iret != 0\n");
    }
  /* determine if need to pack? sectors */
  special_grid = dcsgrid ();

  /* get file name */
  dcflnam (gemfil, filnam, &numgrds, &ier);
  if (ier != 0)
    {
      loglev = 1;
      numerr = -53;
      sprintf (errstr, "no file template [%d.%d %d %d]\0", pds.center,
	       pds.izero, pds.process, pds.grid_id);
      dc_wclg (loglev, errgrp, numerr, errstr, &ier);
      return;
    }
  dcogfil (filnam, &iflno, numgrds, special_grid);
  if (iflno > 0)
    dcwppg (iflno, XGRID, special_grid);
}
