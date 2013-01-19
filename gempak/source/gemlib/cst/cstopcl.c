#include "geminc.h"
#include "gemprm.h"

void cst_opcl ( const char *instr, const char *locopn, char **loccls,
                int *iret )
/************************************************************************
 * cst_opcl								*
 *									*
 * This subroutine looks at the character in position locopn of the	*
 * input character string.  It then looks ahead in the string to find	*
 * the occurrence of the closing character matching the opening		*
 * character.								*
 *									*
 * cst_opcl ( instr, locopn, loccls, iret )				*
 *									*
 * Input parameters:							*
 *	*instr		const char	Input character string		*
 *	*locopn		const char	Pointer to open parenthesis	*
 *									*
 * Output parameters:							*
 *	**loccls	char		Pointer to close parenthesis	*
 *	*iret		int		Return code			*
 *				 	 0 = normal return 		*
 *				 	-7 = unbalanced parenthesis	*
 **									*
 * Log:									*
 * R. Tian/SAIC		12/05	From ST_OPCL				*
 ************************************************************************/
{
    char c, rc, *cp;
    int num;
/*----------------------------------------------------------------------*/
    *iret = 0;

    c = *locopn;
    if ( c == '(' ) {
	rc = ')';
    } else if ( c == '[' ) {
	rc = ']';
    } else if ( c == '{' ) {
	rc = '}';
    } else {
	rc = c;
    }

    /*
     * Look for LOCCLS.
     */
    num = 1;
    for ( cp = (char *)locopn + 1; *cp != '\0'; cp++ ) {
	if ( *cp == c ) {
	    num++;
	} else if ( *cp == rc ) {
	    num--;
	}

	if ( num == 0 ) {
	    *loccls = cp;
	    return;
	}
    }

    if ( num != 0 ) *iret = -7;

    return;
}
