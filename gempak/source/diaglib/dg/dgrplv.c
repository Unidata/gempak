#include "dg.h"

void dg_rplv ( const char *gvect, const int *ignumu, const int *ignumv,
              int *iret )
/************************************************************************
 * dg_rplv								*
 *									*
 * This subroutine replaces the vector on the top of the stack.  If	*
 * GVECT is blank, the grids should be computed already and stored in	*
 * internal grids IGNUMU and IGNUMV.  If GVECT is not blank, IGNUMU and *
 * IGNUMV will be ignored.						*
 *									*
 * dg_rplv ( gvect, ignumu, ignumv, iret )				*
 *									*
 * Input parameters:							*
 *	*gvect		const char	Grid vector function		*
 *	*ignumu		const int	Vector u component		*
 *	*ignumv		const int	Vector v component		*
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
    int ignum;
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
    strcpy ( _dgstck.stack[_dgstck.itop], gvect );
    ignum = (*ignumu) * 100 + (*ignumv);
    _dgstck.istack[_dgstck.itop] = ignum;

    return;
}
