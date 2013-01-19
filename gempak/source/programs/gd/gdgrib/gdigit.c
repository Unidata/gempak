#include "gdgrib.h"

void gdigit ( const int *ival, const int *ibase, int *ndig, int *idigs,
              int *iret )
/************************************************************************
 * gdigit 								*
 *									*
 * This subroutine computes the individual digits of a decimal input	*
 * number in any arbitrary base.					*
 *									*
 * The digits are ordered beginning with the one's digit; so, IDIGS (1) *
 * is multiplied by IBASE**0 = 1, IDIGS (2) by IBASE**1, and so on, in  *
 * recovering the value in the new base.				*
 *									*
 * gdigit ( ival, ibase, ndig, idigs, iret )				*
 *									*
 * Input parameters:							*
 *	*ival		const int	Input decimal value		*
 *	*ibase		const int	Base for the output digits	*
 *									*
 * Input and output parameter:						*
 *	*ndig		int		Input: max # of digits allowed	*
 *					Output: # of digits needed	*
 *									*
 * Output parameters:							*
 *	*idigs		int		Output digits			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = base cannot be < 2	*
 *					 -2 = not enough digits		*
 *					 -3 = input value < 0		*
 **									*
 * Log:									*
 * K. Brill/HPC		 8/99						*
 * R. Tian/SAIC		 9/06		Recoded from Fortran		*
 ************************************************************************/
{
    int num, idx;
    double dnum;
/*----------------------------------------------------------------------*/
    *iret = 0;

    if ( *ibase < 2 ) {
	*iret = -1;
	return;
    }
    if ( *ival < 0 ) {
	*iret = -3;
	return;
    }
    for ( idx = 0; idx < *ndig; idx++ ) idigs[idx] = 0;

    /*
     * Convert number to new base.
     */
    num = *ival;
    idx = 0;
    while ( num > 0 ) {
        idigs[idx++] = num % (*ibase);
	dnum = floor ( (double)num / (*ibase) );
	num = (int)( dnum );
    }
    *ndig = idx;

    return;
}
