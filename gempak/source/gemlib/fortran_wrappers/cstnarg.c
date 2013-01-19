#include "fortran_wrappers.h"

void cst_narg ( const char *strng, int ipos, char argsep, int *nargs,
                int *iret )
/************************************************************************
 * cst_narg								*
 *									*
 * This subroutine counts the number of arguments enclosed by a		*
 * delimiter pair for a function whose argument list begins with	*
 * the opening delimiter at position IPOS in the input string, STRNG.	*
 * The arguments are separated by the single character stored in	*
 * ARGSEP.  The number of arguments is returned as an integer value	* 
 * in NARGS.								*
 *									*
 * cst_narg ( strng, ipos, argsep, nargs, iret )			*
 *									*
 * Input parameters:							*
 *	*strng		const char	Input string of arguments	*
 *	ipos		int		Location of opening delimiter	*
 *	argsep		char		Arguments separator		*
 *									*
 * Output parameters:							*
 *	*nargs		int		Number of arguments in STRNG	*
 *	*iret		int		Return code			*
 *					 0 = normal return 		*
 **									*
 * Log:									*
 * R. Tian/SAIC		 2/06	From ST_NARG				*
 ************************************************************************/
{
    char od, *opnptr, *clsptr, *chrptr;
    int ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *nargs = 0;
    od = strng[ipos];

    /*
     * Find the location of closing delimeter in the input string.
     */
    opnptr = (char *)&strng[ipos];
    cst_opcl ( strng, opnptr, &clsptr, &ier );
    if ( ier != 0 ) {
        *iret = -7;
	return;
    }

    /*
     * Start character-by-character forward search looking for ARGSEP,
     * the opening delimeter, or stoping point.
     */
    for ( chrptr = opnptr + 1; chrptr <= clsptr; chrptr++ ) {
	if ( *chrptr == od ) {
	    cst_opcl ( strng, chrptr, &chrptr, &ier );
	} else if ( *chrptr == argsep ) {
	    (*nargs)++;
	} else if( chrptr == clsptr ) {
	    (*nargs)++;
	}
    }

    return;
}
