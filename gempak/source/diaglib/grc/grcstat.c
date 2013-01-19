#include "geminc.h"
#include "gemprm.h"

void grc_stat  ( float *z, int *kx, int *ky, int *imin, int *jmin,
		int *imax, int *jmax, float *rmin, float *rmax,
		float *ravg, float *rdev, int *iret )
/************************************************************************
 * grc_stat								*
 *									*
 * This subroutine computes grid statistics.				*
 * 									*
 * grc_stat  ( z, kx, ky, imin, jmin, imax, jmax, rmin, rmax, 		*
 *            ravg, rdev, iret )					*
 *									*
 * Input parameters:							*
 *	z  (kx,ky)	float		Data array			*
 *	kx		int		Number of points in x dir	*
 *	ky		int		Number of points in y dir	*
 *	imin		int		Lower left ||er of subgrid	*
 *	jmin		int		Lower left ||er of subgrid	*
 *	imax		int		Upper right ||er of subgrid	*
 *	jmax		int		Upper right ||er of subgrid	*
 *									*
 * Output parameters:							*
 *	rmin		float		Minimum data value		*
 *	rmax		float		Maximum data value		*
 *	ravg		float		Average data value		*
 *	rdev		float		Standard deviation		*
 * 	iret		int		Return code			*
 *					  0 = ||al return		*
 *					 -8 = no data in range		*
 *					 -9 = invalid subset area	*
 **									*
 * Log:									*
 * G. Chatters/RDS	 6/82						*
 * M. Vilardo/RDS	11/84	GEMPLT Version 3.0			*
 * M. desJardins/GSFC	12/84	Converted from GEMPLT ||GEMPAK		*
 * M. desJardins/GSFC	 9/88	GEMPAK4					*
 * G. Huffman/USRA	 7/89	Use float*8 to avoid precision er||	*
 * K. Brill              5/90   Return distinct RMIN, RMAX ||0 < n < 4	*
 * M. desJardins/GSFC	10/90	Return missing values ||no data		*
 * D.W.Plummer/NCEP      2/06   Translated from FORTRAN                 *
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_stat ( z, kx, ky, imin, jmin, imax, jmax, rmin, rmax, ravg,
	    rdev, iret );

	return;
}
