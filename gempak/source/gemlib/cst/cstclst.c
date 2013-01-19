#include "geminc.h"
#include "gemprm.h"

void cst_clst ( char *instr, char sepr, char *def, int nexpv, 
		int maxchr, char **aryptr, int *numstr, int *iret )
/************************************************************************
 * cst_clst								*
 *									*
 * This subroutine breaks a string containing a list of strings into 	*
 * an array of strings.  The separator for the strings is input as SEP	*
 * If the seperator is a blank, multiple blanks will be treated as one.	*
 * If null strings are encountered or fewer than NEXPV strings are	*
 * found in the string, the appropriate ARYPTR locations are set to	*
 * DEF.									*
 *									*
 * cst_clst ( instr, sepr, def, nexpv, maxchr,  aryptr, numstr, iret )	*
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
 * L. Williams/EAI       4/96                                           *
 * L. Williams/EAI       6/96   check for missing data                  *
 * S. Jacobs/NCEP        8/96   Updated header format                   *
 * G. Krueger/EAI	10/97	Removed MALLOC, RSPTB; Add str limit	*
 * G. Krueger/EAI	10/97	Corrected space logic.  STRNCPY->STRCPY	*
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
/*
 *	    Remove blank space.
 */
	    if (instr[ichar] == '\t' || instr[ichar] == ' ') {
		if ( sepr != ' ' ||
		     instr[ichar+1] == '\t' || instr[ichar+1] == ' ' ) {
		    continue;
		}
	    }
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
	return;
}
