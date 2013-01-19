#include "geminc.h"
#include "gemprm.h"

void grc_snav  ( int *navsz, float *rnvblk, int *iret )
/************************************************************************
 * grc_snav								*
 *									*
 * This subroutine sets up a grid coordinate system in GEMPLT.  The	*
 * navigation block should be sent as it was received from the grid	*
 * file open subroutine.  Note that the graphics projection and mode	*
 * must be define before GR_SNAV is called.  This subroutine will fail	*
 * if the grid mode is not the same as the current GEMPLT mode.		*
 *									*
 * grc_snav  ( navsz, rnvblk, iret )					*
 *									*
 * Input parameters:							*
 *	navsz		INTEGER		Length of navigation block	*
 *	rnvblk (navsz)	REAL		Navigation block		*
 *									*
 * Output parameters:							*
 *	iret		INTEGER		Return code			*
 *					  0 = normal return		*
 *					 -6 = invalid navigation type	*
 *					 -7 = GEMPLT error		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	12/84						*
 * M. desJardins/GSFC	 8/88	Fixed for GEMPAK4			*
 * K. Brill/NMC         01/92   Replace GERROR with er_wmsg             *
 * D.W.Plummer/NCEP      2/06   Translated from FORTRAN                 *
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_snav ( navsz, rnvblk, iret );

	return;
}
