#include "tiffcmn.h"

void tspan ( int *inspanx, int *inspany, int *iret )
/************************************************************************
 * tspan								*
 *									*
 * This subroutine sets the direction of increasing X and Y for use	*
 * in the raster graphics algorithms.					*
 *									*
 * tspan ( inspanx, inspany, iret )					*
 *									*
 * Input parameters:							*
 *	*inspanx	int		Direction of increasing x	*
 *	*inspany	int		Direction of increasing y	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

        *iret = G_NORMAL;

/*
 *	Set the global values.
 */
        ispanx = *inspanx;
        ispany = *inspany;

}
