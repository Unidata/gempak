#include "dg.h"

void dg_real ( const float *value, const int *num, int *iret )
/************************************************************************
 * dg_real								*
 *									*
 * This subroutine creates a grid of constant value.			*
 *									*
 * dg_real ( value, num, iret )						*
 *									*
 * Input parameters:							*
 *	*value		const float	Constant value for grid		*
 *	*num		const int	Internal grid number		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    for ( i = 0; i < _dgfile.kxyd; i++ ) {
	_dggrid.dgg[(*num)-1].grid[i] = *value;
    }

    return;
}
