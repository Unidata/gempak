#include "dg.h"

void dg_sare ( int *iret )
/************************************************************************
 * dg_sare								*
 *									*
 * This subroutine computes the area over which diagnostics will be	*
 * included and sets the logical flags in the array SUBA.		*
 *									*
 * dg_sare ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *				  	  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 8/89						*
 * K. Brill/GSC		10/89		New functions			*
 * K. Brill/NMC		10/90		Use full row/col for some funcs	*
 * K. Brill/NMC		05/91		Check grid bounds		*
 * K. Brill/NMC		01/93		Added DVDX and DVDY		*
 * K. Brill/NMC		04/93		Reset use flags if grid changes *
 * L. Sager/NMC		 7/93		Added test on save flag		*
 * R. Tian/SAIC		 2/06		Recoded from Fortran		*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    return;
}
