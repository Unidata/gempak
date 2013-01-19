#include "geminc.h"
#include "gemprm.h"

#define TOKENNB "\t "
#define TOKENNBA "\t \0"

void cst_rxbl ( char *str, char *outstr, int *length, int *iret )
/************************************************************************
 * cst_rxbl								*
 *									*
 * This subroutine removes extra spaces and tabs from a string.  Only	*
 * single blanks will separate substrings. The input and output strings	*
 * may be the same variable.						*
 *									*
 * cst_rxbl ( str, outstr, length, iret )				*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *									*
 * Output parameters:							*
 *	*outstr		char		Output string			*
 *	*length		int		Length of output string		*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 **									*
 * Log:									*
 * G. Krueger/EAI	10/96						*
 * G. Krueger/EAI	10/97	Rewritten to remove MALLOC		*
 * M. Li/GSC		03/00	Modified to assure the input and output	*
				be the same variable if no extra spaces * 	
 ***********************************************************************/
{
	int	inlen, inpos, lennba, lenb, outlen;

/*---------------------------------------------------------------------*/

	*iret = 0;
	inpos = 0;
	outlen = 0;
	inlen = strlen(str);
	while ( str[inpos] != '\0' ) {
/*
 *	    Determine the length of this set of non-blank characters.
 */
	    lennba = strcspn( &str[inpos], TOKENNBA );

	    if ( lennba != 0 ) {
/*
 *		Move the characters to the output string.
 */
		memmove( &outstr[outlen], &str[inpos], lennba );
		inpos += lennba;
		outlen += lennba;
		if ( str[inpos] != '\0' ) {
		    outstr[outlen] = ' ';
		    outlen++;
	            inpos++;
		}
	    } else {
/*
 *		If there are none, identify the next set of non-blank
 *		characters.
 */
		lenb = strspn( &str[inpos], TOKENNB );
		inpos += lenb;
		if ( inpos > inlen ) break;
	    } /* end if */
	} /* end while */

	outstr[outlen] = '\0';
        *length = outlen;
}
