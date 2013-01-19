#include "geminc.h"
#include "gemprm.h"

void cst_rnan  ( char *string, char *outstr, int *iret )
/************************************************************************
 * cst_rnan                                    				*
 * 									*
 * This subroutine replaces underscore characters with spaces. 	        *
 * 									*
 * cst_rnan  ( string, outstr, iret )					*
 *									*
 * Input parameters:                                                   	*
 *	*string		char		String 				*
 *									*
 * Output parameters:                                                  	*
 *	*outstr		char		Converted string          	*
 *	*iret		int	 	Return code  			*
 *				   	 0 = normal return 		*
 ** 									*
 * A. Hardy/GSC		 8/99						*
 ***********************************************************************/
{
	int     i; 
	char    strbuf[256]; 
/*---------------------------------------------------------------------*/
	*iret = 0;
	i = 0;
	strcpy (strbuf, string);

       /*
	* Check each character in string.
	*/

	while ( strbuf[i] != '\0'){

	     if (strbuf[i] == '_') strbuf[i] = ' ';
	     i++;
        }
	 strcpy (outstr, strbuf);

}
