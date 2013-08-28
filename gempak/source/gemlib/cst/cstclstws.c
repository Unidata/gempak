#include "geminc.h"
#include "gemprm.h"

void cst_clstws ( char *instr, char sepr, char *def, int nexpv, 
		int maxchr, char **aryptr, int *numstr, int *iret )
/************************************************************************
 * cst_clstws								*
 *									*
 * This subroutine breaks a string containing a list of strings into 	*
 * an array of strings.  The separator for the strings is input as SEP	*
 * Whitespace will be preserved. Therefore, this function cannot be	*
 * used if the seperator is a blank. If null strings are encountered or *
 * fewer than NEXPV strings are	found in the string, the appropriate	*
 * ARYPTR locations are set to DEF.					*
 *									*
 * cst_clstws ( instr, sepr, def, nexpv, maxchr,  aryptr, numstr, iret )*
 *									*
 * Input parameters:							*
 *	*instr		char		Input string			*
 *	sepr		char		Separator			*
 *	*def		char		Default string			*
 *	nexpv		int		Number of expected values	*
 *	maxchr		int		Maximum characters in strings	*
 *									*
 * Output parameters:							*
 *	**aryptr	char		Pointer to array of strings	*
 *	*numstr		int		Number of strings returned	*
 *	*iret		int		Return code			*
 *					  2 = string too long		*
 *					  1 = more than nexpv values	*
 *					  0 = normal			*
 *					 -1 = invalid nexpv		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Copied from cst_clst			*
 ***********************************************************************/
{
	int	ielt, ichar, jchar, len;
/*---------------------------------------------------------------------*/
	*iret = 0;
	*numstr = 0;
/*
 *	Check the number of expected values
 */
	if( nexpv <= 0 ) {
	   *iret = -1;	
	   return;
	}
/*
 *	Check that the separator is no a blank
 */
	if( sepr == ' ' ) {
	   *iret = -1;	
	   return;
	}
/*
 *	Initialize output array to default value.
 */
	for( ielt=0; ielt < nexpv; ielt++ )
	   strcpy( aryptr[ielt], def );
/*
 *	Process the input string into an array of strings.
 */
	jchar = 0;
	ielt = 0;
	len = strlen (instr);
	for ( ichar = 0; ichar < len; ichar++ )
	{
	    if ( instr[ichar] == sepr ||
		 (instr[ichar] == '\t' && sepr == ' ') ) {
/*
 *		If the character is a separator, work on the next array
 *		element.
 */
		jchar = 0;
		ielt++;
		if ( ielt >= nexpv ) {
		    *iret = 1;
		    break;
		}
	    } else {
/*
 *		Otherwise, append the character to the array element.
 */
		if ( (jchar + 2) >= maxchr ) {
		    *iret = 2;
		    break;
		} else {
		    aryptr[ielt][jchar] = instr[ichar];
		    jchar++;
		    aryptr[ielt][jchar] = '\0';
		}
	    }
	}
	*numstr = ielt + 1;
	if ( *numstr > nexpv ) *numstr = nexpv;
}
