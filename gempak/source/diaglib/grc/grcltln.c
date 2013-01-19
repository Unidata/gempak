#include "geminc.h"
#include "gemprm.h"

void grc_ltln  ( int *kx, int *ky, float *rlat, float *rlon, int *iret )
/************************************************************************
 * grc_ltln								*
 *									*
 * This subroutine computes the latitude and longitude at each grid 	*
 * point.  The grid must be defined in GEMPLT before this subroutine	*
 * is called.								*
 *									*
 * grc_ltln  ( kx, ky, rlat, rlon, iret )				*
 *									*
 * Input parameters:							*
 *	kx		int		Number of points in x dir	*
 *	ky		int		Number of points in y dir	*
 *									*
 * Output parameters:							*
 *	rlat (kx,ky)	float		Latitudes in degrees		*
 *	rlon (kx,ky)	float		Longitudes in degrees		*
 *	iret		int		Return code			*
 *					  0 = normal return		*
 *					 -6 = grid projection error	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	11/88	From DG_LTLN				*
 * K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
 * D.W.Plummer/NCEP      1/05   Translated from FORTRAN                 *
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_ltln ( kx, ky, rlat, rlon, iret );

	return;
}
