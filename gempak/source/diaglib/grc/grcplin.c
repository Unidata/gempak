#include "geminc.h"
#include "gemprm.h"

void grc_plin  ( char *endpts, const int *npmx, int *npts, float *rgx,
			float *rgy, float *rlat, float *rlon, int *iret )
/************************************************************************
 * grc_plin								*
 *									*
 * This subroutine translates the user input for the end points of a 	*
 * cross-section line through a grid into an array of locations along	*
 * the line segment.  The locations in the output array are evenly 	*
 * spaced, with the spacing being approximately the grid spacing.	*
 *									*
 * grc_plin  ( endpts, npmx, npts, rgx, rgy, rlat, rlon, iret )		*
 *									*
 * Input parameters:							*
 *	*endpts		char		User input for end points	*
 *	*npmx		const int	Max allowed value for NPTS	*
 *									*
 * Output parameters:							*
 *	npts		int		Number of points along line	*
 *	rgx  (npmx)	float		X grid point			*
 *	rgy  (npmx)	float		Y grid point			*
 *	rlat (npmx)	float		Latitude			*
 *	rlon (npmx)	float		Longitude			*
 *	iret		int		Return code			*
 *					  0 = normal return		*
 *					-12 = invalid grid point	*
 *					-18 = endpoints too close	*
 *					-23 = output pts exceeds npmx	*
 **									*
 * Log:									*
 * K. F. Brill/GSC       5/89   Created from GR_PLOC			*
 * K Brill/GSC           4/90   Corrected iret = -11 to -12		*
 * K. Brill/NMC          8/90   Added -18 return code			*
 * K. Brill/NMC		 3/91	Add one to NPTS				*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 * R. Tian/SAIC		10/02	Add npmx				*
 * K. Brill/HPC		 2/03	Check for DINX close to zero		*
 * D.W.Plummer/NCEP      2/06   Translated from FORTRAN         	*
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_plin ( (char *)endpts, (int *)npmx, npts, rgx, rgy, rlat,
	    rlon, iret, strlen(endpts) );

	return;
}
