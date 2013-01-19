#include "dg.h"

void dg_rpls ( const char *gfunc, const int *ignum, int *iret )
/************************************************************************
 * dg_rpls								*
 *									*
 * This subroutine replaces the scalar on the top of the stack.  If	*
 * GFUNC is blank, the grid should be computed already and stored in	*
 * internal grid IGNUM.  If GFUNC is not blank, IGNUM will be ignored.	*
 *									*
 * dg_rpls ( gfunc, ignum, iret )					*
 *									*
 * Input parameters:							*
 *	*gfunc		const char	Grid vector function		*
 *	*ignum		const int	Scalar grid number		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-21 = stack is empty		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 9/88						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check that there is something on the stack.
     */
    if ( _dgstck.itop < 0 ) {
	*iret = -21;
	return;
    }

    /*
     * Replace values of stack and istack.
     */
    strcpy ( _dgstck.stack[_dgstck.itop], gfunc );
    _dgstck.istack[_dgstck.itop] = *ignum;

    return;
}
