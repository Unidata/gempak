#include <geminc.h>
#include <gemprm.h>

#include <gbcmn.h>
#include <dccmn.h>

#include "dcgrib.h"


void
decode_grib (char *gemfil, int maxgrd, int *iret)
{
/************************************************************************
* void decode_grib ( gemfil, maxgrd, iret)				*
*	char *gemfile		output gempak grid file name		*
*	int   maxgrd		maximum number of grids in output file	*
*	int  *iret		Error code return			*
*									*
* Error Codes								*
*				-51	failed to read grib tables	*
*				-52	grdnav.tbl error		*
*				-53	file template not available	*
*				-55	number of gridpoints too large	*
* Log:									*
* Chiz/Unidata			12/99	Created				*
*************************************************************************/
  size_t griblen;

  char filnam[132];
  int ier, gvers;
  int numerr, loglev;
  char errstr[80];
  static char errgrp[] = "DCGRIB";
  unsigned char *gribbul = NULL;

  *iret = 0;

/*
** Initialize the nacmn.cmn common block for grid tables
*/
  na_init (iret);

/*
** Set the GBDIAG variable for verbose output if necessary
*/
  if (ivrblv >= 3)
    {
      GBDIAG_GDS = TRUE;
      GBDIAG_PDS = TRUE;
      GBDIAG_BDS = TRUE;
    }


  while ((*iret == 0) || ( *iret < -10 ) )
    {

/*
** Get a single GRIB product from the input stream.
** If *iret != 0, keep looking for GRIB messages we can handle.
*/
      gribbul = dc_ggrib (&griblen, &gvers, iret);

      if ( (*iret != 0) || ( gribbul == NULL ) )
	continue;

      switch (gvers)
	{
	case 0:
	case 1:		/* send grib1 bulletins to decode_grib1 */
	  decode_grib1 (gribbul, griblen, gemfil, filnam, maxgrd, iret);
	  break;
	case 2:		/* send grib2 bulletins to decode_grib2 */
	  decode_grib2 (gribbul, griblen, gemfil, filnam, maxgrd);
	  break;
	default:
	  loglev = 0;
	  numerr = -70;
	  sprintf (errstr, "Unknown GRIB version %d\0", gvers);
	  dc_wclg (loglev, errgrp, numerr, errstr, &ier);
	}

      nbull++;
    }
/*
** Close all open files
*/
  dcgfcls (&ier);
}
