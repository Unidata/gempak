#include "dg.h"

void dg_rstm ( int *iret )
/************************************************************************
 * dg_rstm								*
 *									*
 * This subroutine resets the pointer into the list of times stored in	*
 * the NFILE block of DGCMN.CMN to 0.					*
 *									*
 *									*
 * dg_rstm ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/HPC		 3/04						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    _nfile.itmlst = 0;

    return;
}
