#include "geminc.h"
#include "gemprm.h"

void cst_ldsp ( char *str, char *outstr, int *nc, int *iret )
/************************************************************************
 * cst_ldsp								*
 *									*
 * This subroutine deletes the leading spaces in a string.  The input	*
 * and output strings may be the same variable.				*
 *									*
 * cst_ldsp ( str, outstr, nc, iret )					*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *									*
 * Output parameters:							*
 *	*outstr		char		Output string			*
 *	*nc		int		Number of characters		*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 ***********************************************************************/
{
size_t	ii;
char *tmpstr;
/*---------------------------------------------------------------------*/

	*iret = 0;

	/*
	 * remove leading spaces from the input string
	 */
	for ( ii=0; ii < strlen(str); ii++ ) {
	    if ( !isspace( str[ii] ) ) {
		if ( str == outstr ) {
		    if ( ii != (size_t)0 ) {
			G_MALLOC(tmpstr, char, (int)strlen(str), "cst_ldsp");
			strcpy(tmpstr, &str[ii]);
			strcpy(outstr, tmpstr);
			G_FREE(tmpstr, char);
		    }
		}
		else {
		    strcpy(outstr, &str[ii]);
		}
		break;
	    }
	}

	*nc = strlen( outstr );

}
