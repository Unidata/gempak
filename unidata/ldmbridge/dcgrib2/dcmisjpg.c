#include "gb2def.h"
#include "dccmn.h"		/* for ivrblv global variable */

#include "dcgrib.h"

void
dcmisjpg (Gribmsg g2, float *fgrid)
/************************************************************************
* void dmisjpg ( gfld, fgrid)						*
*	unsigned char	*cgrib	Input grib product			*
*	int		lenbul	length of input product			*
*	char		*gemfil	User specified output file name		*
*	char		*filnam	final output filename			*
*	int		maxgrd	Maximum number of grids in new file	*
*                                                                       *
* Error Codes                                                           *
*									*
* Note for JPEG200/PNG specification: missing value specification 	*
* unavailable in template. Assuming that gfld->fld[i] == 0 means 	*
* missing data when a value of 0 is outside the range of the reference 	*
* value and maximum represented by number of bits. Can't use nbits to 	*
* pack if we have values outside the max/min range in our data. Some 	*
* grids such as UREL, VREL can have 0 within the data range as a valid 	*
* value. Problem found that DGEX grids are surrounded with 2 rows/colums*
* of zeros. We could replace these with RMISSD, but not all zero values *
* in general. At least we won't lose precision with packing when 0 	*
* is within data range. 						*
*									*
* Log:                                                                  *
* Chiz/Unidata                  12/03   Created                         *
*************************************************************************/
{
  int i, iret;
  g2float ref_val;
  int nbits;
  float rmaxval;
  char errstr[128];
  static char errgrp[] = "DECODE_GRIB2";

  if ((g2.gfld->idrtnum != 40000) && (g2.gfld->idrtnum != 40)
      && (g2.gfld->idrtnum != 41) && (g2.gfld->idrtnum != 40010))
    {
      printf ("not a jpeg or PNG grid.\n");
      return;
    }

  if (g2.gfld->idrtmpl[3] >= 0 && g2.gfld->idrtmpl[3] <= 32)
    nbits = g2.gfld->idrtmpl[3];
  else
    {
    printf("Using default 32 bits to specify range for JPEG/PNG\n");
    nbits = 32;
    }


  g2_rdieee (&(g2.gfld->idrtmpl[0]), &ref_val, (g2int) 1);
  rmaxval = ref_val + pow (2.0, (float)nbits);

  if ((ref_val > 0.) || (rmaxval < 0.))
    {
      for (i = 0; i < (g2.kx * g2.ky); i++)
	{
	  if (g2.gfld->fld[i] == 0.)
	      g2.gfld->fld[i] = RMISSD;
	}
    }
  else
    {
      sprintf (errstr, "Ref val 0 for DRT %d, not replaced\0",g2.gfld->idrtnum);
      dc_wclg (4, errgrp, 0, errstr, &iret);
    }

}
