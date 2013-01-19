#include "geminc.h"
#include "gemprm.h"

void grc_setr  ( int *kxin, int *kyin, int *ishft, int *iret )
/************************************************************************
 * grc_setr								*
 *									*
 * This subroutine sets the shift position needed to rearrange a globe	*
 * wrapping grid on either a CED or MER projection.  The rearrangement	*
 * must be done for any display map projection when the grid discon-	*
 * tinuity occurs within the map area.  The rearrangement creates a	*
 * continuous grid of data over the map	area.				*
 * 									*
 * Warning:  This subroutine resets the grid navigation in GPLT to be	*
 *           that of the re-arranged grid.				*
 * 									*
 * grc_setr  ( kxin, kyin, ishft, iret )				*
 *									*
 * Input parameters:							*
 *	kxin		int		Number of input points in x dir	*
 *	kyin		int		Number of input points in y dir	*
 *									*
 * Output parameters:							*
 *	ishft		int		X index shift number needed by	*
 *					subroutine GR_DORG		*
 *									*
 * Output parameters:							*
 * 	iret		int		Return code			*
 *					  0 = normal return		*
 *					-21 = cannot fix wrap around	*
 *					-22 = no map projection is set	*
 **									*
 * Log:									*
 * K. Brill/HPC		08/02	Created from original GR_RARG code with	*
 *				added longitude buffer zone and	check	*
 *				at +/-5 grid pts from grd discontinuity *
 * K. Brill/HPC		09/02	Do only check at columns KX-5, KX, and 5*
 * S. Jacobs/NCEP	10/02	Removed check for +/-5 grid pt columns	*
 * D.W.Plummer/NCEP      2/06   Translated from FORTRAN                 *
 ***********************************************************************/
{
/*----------------------------------------------------------------------*/
	gr_setr ( kxin, kyin, ishft, iret );

	return;
}
