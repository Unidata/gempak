#include "geminc.h"
#include "gemprm.h"

void cst_uclc ( char *str, char *outstr, int *iret )
/************************************************************************
 * cst_uclc								*
 *									*
 * This routine will convert the input string to all lower case.	*
 *									*
 * cst_uclc ( str, outstr, iret )					*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *									*
 * Output parameters:							*
 *	*outstr		char		Lower case string		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * L. Williams/EAI	 3/96		Added isalpha check		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = 0;

	/*
	 * Loop through all of the characters of the input string.
	 */
	while ( *str ) {

	   /*
	    * Check for alphabetic and lower case characters.
	    */
	   if ( isalpha(*str) && isupper(*str) ) {

		 /*
		  * Convert lower case to upper case,....
		  */
	         *outstr = tolower(*str);
	   }
	   else
		 /*
		  * otherwise use the input character.
		  */
		*outstr = *str;

	   str++;
	   outstr++;
	}

	*outstr = '\0';

}
