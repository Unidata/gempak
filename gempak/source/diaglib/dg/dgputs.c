#include "dg.h"

void dg_puts ( const int *ignum, int *iret )
/************************************************************************
 * dg_puts								*
 *									*
 * This subroutine puts a scalar grid number on the top of the stack.	*
 *									*
 * dg_puts ( ignum, iret )						*
 *									*
 * Input parameters:							*
 *	*ignum		const int	Grid number			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-12 = ... must be a scalar	*
 *					-20 = stack is full		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	5/88						*
 * G. Huffman/GSC	9/88	Error messages				*
 * R. Tian/SAIC		2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int ierrst;
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
     * Check that the grid is a scalar.
     */
    if ( ( *ignum <= 0 ) || ( *ignum >= 100 ) ) {
	*iret = -12;
	ierrst =(*ignum) % 100;
	if ( ierrst > 0 ) {
	   strcpy ( _dgerr.errst, _dggrid.gparmd[ierrst-1] );
	}
	return;
    }

    /*
     * Add the grid to the stack.
     */
    _dgstck.itop++;
    _dgstck.stack[_dgstck.itop][0] = '\0';
    _dgstck.istack[_dgstck.itop] = *ignum;

    return;
}
