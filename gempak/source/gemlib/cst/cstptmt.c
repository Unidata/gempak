#include "geminc.h"
#include "gemprm.h"

#define METACHARS  	"*?[]"


int IsMetaChar ( const char ch )
/************************************************************************
 * IsMetaChar								*
 *									*
 * This function finds Metacharacters.					*
 *									*
 * int IsMetaChar ( ch )						*
 *									*
 * Input parameters:							*
 *									*
 *	ch	const char	Character to be checked			*
 *									*
 * Output parameters:							*
 *	IsMetaChar	int	If meta character = 1			*
 **									*
 ***********************************************************************/
{
    char *p;

/*---------------------------------------------------------------------*/

    p = METACHARS;

    while ( *p ) {
	if ( *p == ch ) return(1);
	p++;
    }

    return(0);

}

/*======================================================================*/

void cst_ptmt ( const char *str, const char *pattern, int *match, int *iret )
/************************************************************************
 * cst_ptmt								*
 *									*
 * This function accepts a string and compares it against a string	*
 * pattern using an ad-hoc regular expression pattern matching		*
 * algorithm.  Supported metacharacters include				*
 *									*
 *	*  -  matches any number of characters				*
 *	?  -  matches any single character				*
 *	[] -  matches any single character of a list within the []	*
 *									*
 * cst_ptmt ( str, pattern, match, iret )				*
 *									*
 * Input parameters:							*
 *	*str		const char	String to be tested		*
 *	*pattern	const char	Pattern that string is tested	*
 *								against	*
 *									*
 * Output parameters:							*
 *	*match		int		Result of pattern match		*
 *					  1 = true			*
 *					  0 = false			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * D.W.Plummer/NCEP	 2/97						*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * F. J. Yen/NCEP	 5/99	Removed extraneous break statement	*
 * D.W.Plummer/NCEP	 7/00	Bug fix for strings shorter than pattrn	*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    while( *pattern ) {

        switch( *pattern ) {

	case '*':

	    while ( *pattern == '*' ) pattern++;
	    if ( *pattern == '\0' )  {
		*match = 1;
		return;
	    }

	    while ( *str ) {
	        while ( (*str != *pattern) && (*str) && (!IsMetaChar(*pattern)) ) str++;
		if ( !(*str) )  {
		    *match = 0;
		    return;
		}
		cst_ptmt ( str, pattern, match, iret );
		if ( *match == 1 )  {
		    return;
		}
		else  {
		    str++;
		}
	    }
	    *match = 0;
	    return;

	case '?':
	    if ( *str == '\0' )  {
		*match = 0;
		return;
	    }
	    pattern++;
	    str++;
	    break;

	case '[':
	    pattern++;
	    *match = 0;
	    while ( (*pattern) && *pattern!= ']' ) {
		if ( ! *match ) {
		    if ( *pattern == *str )
		        *match = 1;
		    else {
			if ( *pattern == '-' && *str >= *(pattern-1) && *str <= *(pattern+1) )
                            *match = 1;
		    }
		}
		pattern++;
	    }
	    if ( ! *match )  {
		*match = 0;
		return;
	    }
	    str++;
	    pattern++;
	    break;

	default:
	    if ( *pattern != *str )  {
		*match = 0;
		return;
	    }
	    pattern++;
	    str++;
	    break;
        }

    }

	if ( *str == '\0' )  {
	    *match = 1;
	    return;
	}
	else  {
	    *match = 0;
	    return;
	}

}
