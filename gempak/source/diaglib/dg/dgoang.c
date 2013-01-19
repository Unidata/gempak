#include "dg.h"

void dg_oang ( const float *orient, int *iret )
/************************************************************************
 * dg_oang								*
 *									*
 * This subroutine sets the orientation angle for the grid diagnostics	*
 * package.  This angle is usually used to determine normal and 	*
 * tangential components of vectors with respect to a cross section.	*
 * The tangential components are along the orientation angle.		*
 *									*
 * dg_oang ( orient, iret )						*
 *									*
 * Input parameters:							*
 *	*orient		const float	Orientation angle in radians	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/GSC		 7/89	Added orientation angle			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret   = 0;

    _dgovec.ornang = *orient;

    return;
}
