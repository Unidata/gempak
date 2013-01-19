#include "faxcmn.h"

void rspan ( int *inspanx, int *inspany, int *iret )
/************************************************************************
 * rspan								*
 *									*
 * This subroutine sets the direction of increasing X and Y for use	*
 * in the raster graphics algorithms.					*
 *									*
 * rspan ( inspanx, inspany, iret )					*
 *									*
 * Input parameters:							*
 *	*inspanx	int		Direction of increasing x	*
 *	*inspany	int		Direction of increasing y	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAI	 3/97	Created					*
 * S. Jacobs/NCEP	 7/97	Cleaned up header			*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
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
