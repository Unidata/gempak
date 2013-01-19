#include "geminc.h"
#include "gemprm.h"

void grc_dorg ( int *kxin, int *kyin, int *ishft, float *grid, int *iret )
/************************************************************************
 * grc_dorg								*
 *									*
 * This subroutine rearranges a globe wrapping grid on either a CED or	*
 * MER projection.							*
 * 									*
 * This rearrangement creates a continuous grid of data over the map	*
 * area.								*
 * 									*
 * CALL GR_SETR to obtain the value of ISHFT before calling GR_DORG.	*
 * 									*
 * grc_dorg  ( kxin, kyin, ishft, grid, iret )				*
 *									*
 * Input parameters:							*
 *	*kxin		int		Number of input points in x dir	*
 *	*kyin		int		Number of input points in y dir	*
 *	*ishft		int		X index grid shift required	*
 *									*
 * Input and output parameters:						*
 *	*grid(kxin*kyin)float		Grid of data			*
 *									*
 * Output parameters:							*
 * 	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/HPC		08/02	Created from original GR_RARG code	*
 * D.W.Plummer/NCEP     1/05    Translated from FORTRAN         	*
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_dorg ( kxin, kyin, ishft, grid, iret );

	return;
}
