#include "geminc.h"
#include "gemprm.h"

void cst_ilst ( char *str, char sep, int def, int nexp, int *intptr, 
						int *num, int *iret )
/************************************************************************
 * cst_ilst								*
 *									*
 * This subroutine breaks a string containing a list of integers into 	*
 * an array of integers.  The separator for the integers is input as	*
 * SEP.  If the seperator is a blank, multiple blanks will be treated	*
 * as one.  If null strings are	encounteded or fewer than NEXP strings	*
 * are found in the string, the appropriate intptr locations are set to	*
 * DEF.									*
 *									*
 * Range strings (with optional increments) are indicated with a hyphen	*
 * (i.e., 3-9 or 3-12-3) and are processed into the INTPTR array.	*
 *									*
 * cst_ilst ( str, sep, def, nexp, intptr, num, iret )			*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *	sep		char		Separator			*
 *	def		int	 	Default value			*
 *	nexp		int		Number of expected values	*
 *									*
 * Output parameters:							*
 *	*intptr		int		Pointer to integer values	*
 *	*num		int		Number of integers returned	*
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
 ***********************************************************************/
{
char	strbuf[160], alpha[10], omega[10], rinc[10];
int	ielt, ichar, jchar, len, lenss;
int	begin, end, inc, tp, ival, intgr, ier, err;

/*---------------------------------------------------------------------*/
	*iret = 0;
	*num = 0;

	/*
	 * check the number of expected values
	 */
	if( nexp <= 0 ) {
	   intptr = 0;
	   *iret = -1;
	   return;
	}

	/*
	 * initialize output array to default value
	 */
	for( ielt=0; ielt < nexp; ielt++ )
	     intptr[ielt] = def;

	/*
	 * process the input string into an array of integer numbers
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
			    (str[ichar+1] == '\t' && str[ichar+1] == ' ') ) {
		    continue;
		}
	    }
	    if ( str[ichar] == sep ) {
		/*
		 * character is a separator, finish the current array
		 * element
		 */
		strbuf[jchar] = '\0';
		cst_lstr( strbuf, &lenss, &ier );
		if ( lenss != 0 ) {
		    cst_rang( strbuf, alpha, omega, rinc, &tp, &ier );
		    if ( tp ) {
			/*
			 * this is a range
			 */
			cst_numb( alpha, &begin, &ier );
			cst_numb( omega, &end, &err );
			if ( ( ier == -2 ) || ( err == -2 ) ) {
			    *iret = -3;
			} else {
			    inc = 1;
			    if ( tp != 1 ) inc = atoi( rinc );
			    for ( ival=begin; ival <= end; ival += inc ) 
				if ( ielt < nexp ) {
				    intptr[ielt] = ival;
				    ielt++;
				}
				ielt--;
			}
		    } else {
			/*
			 * this is a simple integer
			 */
			cst_numb( strbuf, &intgr, &ier );
			if ( ier == -2 ) {
			    *iret = -3;
			} else {
			    intptr[ielt] = intgr;
			}
		    }
		}
		ielt++;
		/*
		 * work on the next array element
		 */
		jchar = 0;
		*strbuf = '\0';
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
	    }
	}
	/*
	 * finish the last array element
	 */
	strbuf[jchar] = '\0';
	cst_lstr( strbuf, &lenss, &ier );
	if ( lenss != 0 ) {
	    cst_rang( strbuf, alpha, omega, rinc, &tp, &ier );
	    if ( tp ) {
		/*
		 * this is a range
		 */
		cst_numb( alpha, &begin, &ier );
		cst_numb( omega, &end, &err );
		if ( ( ier == -2 ) || ( err == -2 ) ) {
		    *iret = -3;
		} else {
		    inc = 1;
		    if ( tp != 1 ) inc = atoi( rinc );
		    for ( ival=begin; ival <= end; ival += inc ) 
			if ( ielt < nexp ) {
			    intptr[ielt] = ival;
			    ielt++;
			}
			ielt--;
		}
	    } else {
		/*
		 * this is a simple integer
		 */
		cst_numb( strbuf, &intgr, &ier );
		if ( ier == -2 ) {
		    *iret = -3;
		} else {
		    intptr[ielt] = intgr;
		}
	    }
	}
	ielt++;
	*num = ielt;
	if ( *num > nexp ) *num = nexp;
}
