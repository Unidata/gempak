#include "geminc.h"
#include "gemprm.h"

void cst_unp2 ( char *string, char *outstr, int *iret );

void cst_unp2 ( char *string, char *outstr, int *iret )
/************************************************************************
 * cst_unp2								*
 *									*
 * This function will eliminate substrings of unprintable characters.	*
 * Control characters, except for "new line", will be replaced by a	*
 * single blank. Characters greater than '}' (ASCII 126) are replaced	*
 * by a tilda (~, ASCII 127). The lengths of the output string will be	*
 * the same as the length of the input string.				*
 *									*
 * New line characters will be ignored.					*
 *									*
 * cst_unp2 ( string, outstr, iret )					*
 *									*
 * Input parameters:							*
 *	*string		char		Input string			*
 *									*
 * Output parameters:							*
 *	*outstr		char		Output string			*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	10/03	Copied from CST_UNPR			*
 ***********************************************************************/
{
	size_t		ii, ip;
	unsigned char	ch;

/*---------------------------------------------------------------------*/
	*iret = 0;
	ip    = 0;

/*
 *	Check each character to see if it is a control character or
 *	out of range.
 */
	for ( ii = 0; ii < strlen ( string ); ii++ ) {

	    ch = string[ii];

	    if  ( ( '!' <= ch ) && ( ch <= '}' ) ) {

/*
 *		Add non-blanks to the output string.
 */
		outstr[ip] = ch;
		ip++;
	    }
	    else if ( '}' < ch ) {

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
		if  ( ch == '\n' ) {
		    /* Do nothing */
		}
		else {
		    outstr[ip] = CHSPAC;
		    ip++;
		}
	    }
	}

	outstr[ip] = CHNULL;

}
