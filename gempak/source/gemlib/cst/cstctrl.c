#include "geminc.h"
#include "gemprm.h"

void cst_ctrl ( char *str, char *outstr, int *outlen, int *iret )
/************************************************************************
 * cst_ctrl								*
 *									*
 * This routine converts all occurrences of literal control characters	*
 * (eg., "\r\r\n") within a string to real control characters.  	*
 * The output string may be the same as the input string.		*
 *									*
 * cst_ctrl ( str, outstr, outlen, iret )				*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 * Output parameters:							*
 *	*outstr		char		Converted string		*
 *	*outlen		int		Length of outstr		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/00						*
 ***********************************************************************/
{
int 	ii, len;

/*---------------------------------------------------------------------*/

	*iret = 0;

	strcpy(outstr, str);

	len = strlen ( outstr );

	*outlen = 0;
	for ( ii = 0; ii < len; ii++ )  {

	    if ( outstr[ii] == '\\' )  {

		if ( outstr[ii+1] == 'r' )  {
		    outstr[*outlen] = '\r';	
		    (*outlen)++;
		}
		else if ( outstr[ii+1] == 'n' )  {
		    outstr[*outlen] = '\n';	
		    (*outlen)++;
		}

		ii++;

	    }
	    else  {

		outstr[*outlen] = outstr[ii]; 	
		(*outlen)++;

	    }

	}

	outstr[*outlen] = '\0';

}
