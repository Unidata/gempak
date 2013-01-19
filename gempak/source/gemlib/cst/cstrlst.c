#include "geminc.h"
#include "gemprm.h"

void cst_rlst ( char *str, char sep, float def, int nexp, 
				float *fltptr, int *num, int *iret )
/************************************************************************
 * cst_rlst								*
 *									*
 * This subroutine breaks a string containing a list of reals into an	*
 * array of real values.  The separator for the reals is input as SEP. 	*
 * If the seperator is a blank, multiple blanks will be treated	as one.	*
 * If null strings are encountered or fewer than NEXP strings are found	*
 * in the string, the appropriate FLTPTR locations are set to DEF.	*
 *									*
 * cst_rlst ( str, sep, def, nexp, fltptr, num, iret )			*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *	sep		char		Separator			*
 *	def		float	 	Default value			*
 *	nexp		int		Number of expected values	*
 *									*
 * Output parameters:							*
 *	*fltptr		float		Pointer to float values		*
 *	*num		int		Number of values returned	*
 *	*iret		int		Return code			*
 *					  1 = more than nexp values	*
 *					  0 = normal			*
 *					 -1 = invalid nexp		*
 *					 -3 = invalid substring		*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 * L. Williams/EAI	 6/96	check for missing data			*
 * S. Jacobs/NCEP	 8/96	Updated header format			*
 * G. Krueger/EAI	10/97	Removed MALLOC, RSPTB			*
 * H. Zeng/SAIC		09/05	fixed a bug when sep=' '		*
 ***********************************************************************/
{
char	strbuf[160];
int	ielt, ichar, jchar, len;

/*---------------------------------------------------------------------*/
	*iret = 0;
	*num = 0;

	/*
	 * check the number of expected values
	 */
	if( nexp <= 0 ) {
	   fltptr = 0;
	   *iret = -1;
	   return;
	}

	/*
	 * initialize output array to default value
	 */
	for( ielt=0; ielt < nexp; ielt++ )
	     fltptr[ielt] = def;

	/*
	 * process the input string into an array of real numbers
	 */
	jchar = 0;
	ielt = 0;
	*strbuf = '\0';
	len = strlen (str);
	for ( ichar = 0; ichar < len; ichar++ )
	{
	    if (str[ichar] == '\t' || str[ichar] == ' ') {
		if (sep != ' ') {
		    continue;
		} else if ( (sep == ' ') &&
			    (str[ichar+1] == '\t' || str[ichar+1] == ' ') ) {
		    continue;
		}
	    }
	    if ( str[ichar] == sep ) {
		/*
		 * if the character is a separator, work on the next
		 * array element.
		 */
		jchar = 0;
		*strbuf = '\0';
		ielt++;
		if ( ielt >= nexp ) {
		    *iret = 1;
		    break;
		}
	    } else {
		/*
		 * otherwise, append the character to the buffer
		 */
		strbuf[jchar] = str[ichar];
		jchar++;
		strbuf[jchar] = '\0';
		fltptr[ielt] = (float)atof(strbuf);
	    }
	}
	*num = ielt + 1;
	if ( *num > nexp ) *num = nexp;
}
