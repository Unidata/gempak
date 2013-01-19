#include "geminc.h"
#include "gemprm.h"

void cst_unpr ( char *string, char *outstr, int *iret )
/************************************************************************
 * cst_unpr								*
 *									*
 * This function will eliminate substrings of unprintable characters.	*
 * Control characters, except for "new line", will be replace by a	*
 * single blank. Characters greater than '}' (ASCII 126) are replaced	*
 * by a tilda (~, ASCII 127). The lengths of the output string will be	*
 * the same as the length of the input string.				*
 *									*
 * cst_unpr ( string, outstr, iret )					*
 *									*
 * Input parameters:							*
 *	*string		char		Input string			*
 *									*
 * Output parameters:							*
 *	*outstr		char		Output string			*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 6/94						*
 * L. Williams/EAI	 7/94		Reformat header			*
 ***********************************************************************/
{
	int		ip;
	size_t		ii;
	unsigned char	c;

/*---------------------------------------------------------------------*/
	*iret = 0;
	ip    = 0;

/*
 *	Check each character to see if it is a control character or
 *	out of range.
 */
	for ( ii = 0; ii < strlen( string ); ii++ ) {

	    c = string[ii];

	    if  ( ( '!' <= c ) && ( c <= '}' ) ) {

/*
 *		Add non-blanks to the output string.
 */
		outstr[ip] = c;
		ip++;
	    }
	    else if ( '}' < c ) {

/*
 *		Replace out of range characters with a tilda.
 */
		outstr[ip] = CHTLDA;
		ip++;
	    }
	    else {

/*
 *		Replace control characters, except a "new line",
 *		with a blank.
 */
		if  ( c == '\n' ) {
		    outstr[ip] = c;
		    ip++;
		}
		else {
		    outstr[ip] = CHSPAC;
		    ip++;
		}
	    }
	}

	outstr[ip] = CHNULL;

}
