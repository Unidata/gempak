#include "geminc.h"
#include "gemprm.h"

void grc_ploc  ( char *gpoint, float *rgx, float *rgy, float *rlat, 
		float *rlon, int *iret )
/************************************************************************
 * grc_ploc								*
 *									*
 * This subroutine translates the user input for a grid point into an	*
 * actual grid point, x and y coordinates, and latitude and longitude.	*
 *									*
 * grc_ploc  ( gpoint, rgx, rgy, rlat, rlon, iret )			*
 *									*
 * Input parameters:							*
 *	gpoint		char*		User input for grid point	*
 *									*
 * Output parameters:							*
 *	rgx		float		X grid point			*
 *	rgy		float		Y grid point			*
 *	rlat		float		Latitude			*
 *	rlon		float		Longitude			*
 *	iret		int		Return code			*
 *					  0 = normal return		*
 *					-12 = invalid grid point	*
 *					-13 = proj not set in GEMPLT	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	11/85						*
 * M. desJardins/GSFC	 9/88	GEMPAK4					*
 * M. desJardins/GSFC	 4/89	Added LC_FLOC to get location		*
 * K. Brill/GSC          3/90   Added DG_KXKY and check againt KX and KY*
 * K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
 * R. Tian/SAIC		10/02	Replace DG_KXKY with GQGPRJ		*
 * K. Brill/HPC		 4/04	Allow .005 slop in chck against grd bnds*
 * D.W.Plummer/NCEP      2/06   Translated from FORTRAN                 *
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_ploc ( gpoint, rgx, rgy, rlat, rlon, iret, strlen(gpoint) );

	return;
}
