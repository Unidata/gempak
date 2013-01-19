#include "dg.h"

void dg_putv ( const int *ignumu, const int *ignumv, int *iret )
/************************************************************************
 * dg_putv								*
 *									*
 * This subroutine puts a vector grid number pair on the top of the	*
 * stack.								*
 *									*
 * dg_putv ( ignumu, ignumv, iret )					*
 *									*
 * Input parameters:							*
 *	*ignumu		const int	U component grid number		*
 *	*ignumv		const int	V component grid number		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-11 = ... must be a vector	*
 *					-20 = stack is full		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 5/88						*
 * G. Huffman/GSC	 9/88	Error messages				*
 * T. Lee/GSC		 9/97	Fixed error messages			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int ignum;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * First check that there is room on the stack.
     */
    if ( _dgstck.itop > LNSTK - 1 ) {
	*iret = -20;
	return;
    }

    /*
     * Check that the grid is a vector.
     */
    if ( ( *ignumu <= 0 ) || ( *ignumu >= 100 ) ||
	 ( *ignumv <= 0 ) || ( *ignumv >= 100 ) ) {
	*iret = -11;
	if ( ( *ignumu > 0 ) && ( *ignumu < 100 ) ) {
	    strcpy ( _dgerr.errst, _dggrid.gparmd[(*ignumu)-1] );
	}
	return;
    }

    /*
     * Add the grid to the stack.
     */
    _dgstck.itop++;
    ignum = (*ignumu) * 100 + (*ignumv);
    _dgstck.stack[_dgstck.itop][0] = '\0';
    _dgstck.istack[_dgstck.itop] = ignum;

    return;
}
