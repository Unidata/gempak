#include "geminc.h"
#include "gemprm.h"

void grc_cnav ( const float *rnvblk, const float *gsnvbk, 
		const int *navsz, int *gsflag, int *iret )
/************************************************************************
 * grc_cnav								*
 *									*
 * This subroutine compares two grid navigation blocks.  If the two	*
 * navigation blocks are the same, GSFLAG is true.			*
 *									*
 * grc_cnav  ( rnvblk, gsnvbk, navsz, gsflag, iret )			*
 *									*
 * Input parameters:							*
 *	*rnvblk (navsz)	float		First grid navigation block	*
 *	*gsnvbk (navsz)	float		Second grid navigation block	*
 *	*navsz		int		Navigation length		*
 *									*
 * Output parameters:							*
 *	*gsflag		LOGICAL		Check flag			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC		1/92						*
 * S. Jacobs/EAI	8/93		Added tolerance check		*
 * D.W.Plummer/NCEP     1/05   		Translated from FORTRAN         *
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_cnav ( (float *)rnvblk, (float *)gsnvbk, (int *)navsz,
	    gsflag, iret );

	return;
}
