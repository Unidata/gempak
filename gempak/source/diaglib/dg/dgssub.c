#include "dg.h"

void dg_ssub ( int *iret )
/************************************************************************
 * dg_ssub								*
 *									*
 * This subroutine increments the subroutine ID number used to assign	*
 * ownership to internal grids.						*
 *									*
 * dg_ssub ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/HPC		12/01						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    _dggrid.isubid++;

    return;
}
