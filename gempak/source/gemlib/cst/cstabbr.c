#include "geminc.h"
#include "gemprm.h"


void cst_abbr ( char *str, char *abbr, int *flag, int *iret )
/************************************************************************
 * cst_abbr								*
 *									*
 * This subroutine determines whether the string in abbr is an		*
 * abbreviation (beginning substring) of the input string.  This 	*
 * comparison is case sensitive.					*
 *									*
 * cst_abbr ( str, abbr, flag, iret )					*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *	*abbr		char		Abbreviation			*
 *									*
 * Output parameters:							*
 *	*flag		int		Abbreviation flag		*
 *					  0 = false			*
 *					  1 = true			*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = 0;
	*flag = 0;

	/*
	 * check for the abbreviation
	 */
	if( strncmp( str, abbr, strlen(abbr) ) == 0 )
		*flag = 1;

}
