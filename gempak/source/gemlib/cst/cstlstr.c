#include "geminc.h"
#include "gemprm.h"

void cst_lstr ( char *str, int *nc, int *iret )
/************************************************************************
 * cst_lstr								*
 *									*
 * This subroutine returns the number of characters in a string 	*
 * disregarding trailing null characters, tabs and spaces.		*
 *									*
 * cst_lstr ( str, nc, iret )						*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *									*
 * Output parameters:							*
 *	*nc		int		Number of characters		*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 ***********************************************************************/
{
int	i, count;

/*---------------------------------------------------------------------*/

	*iret = 0;
	count = 0;
	

	/*
	 * remove trailing spaces from the input string
	 */
	for( i=strlen(str)-1; i >= 0; i-- ) {
	   if ( isspace( str[i] ) ) {
		count++;
	   }
	   else {
		break;
	   }
	}

	*nc = strlen( str ) - count;

}
