#include "dg.h"

void dg_qkxy ( int *kx, int *ky, int *iret )
/************************************************************************
 * dg_qkxy								*
 *									*
 * This subroutine retrieves the number of whole grid points.		*
 *									*
 * dg_qkxy ( kx, ky, iret )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*kx		int		Number of X grid points		*	
 *	*ky		int		Number of Y grid points		*	
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 **									*
 * Log:									*
 * R. Tian/SAIC		 4/04						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    *kx = G_NINT ( _dgsubg.refnav[4] );
    *ky = G_NINT ( _dgsubg.refnav[5] );

    return;
}
