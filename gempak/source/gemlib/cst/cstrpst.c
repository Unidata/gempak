#include "geminc.h"
#include "gemprm.h"

void cst_rpst ( const char *string, const char *substr, const char *repstr, 
					char *outstr, int *iret )
/************************************************************************
 * cst_rpst								*
 *									*
 * This subroutine finds a substring within a string and replaces the	*
 * substring with a replacement string.  The output string may be the	*
 * same as the input string.  It is the caller's responsibility to 	*
 * ensure that the output string is long enough if repstr > substr.	*
 *									*
 * cst_rpst ( string, substr, repstr, outstr, iret )			*
 *									*
 * Input parameters:							*
 *	*string		const char	Input string			*
 *	*substr		const char	Substring			*
 *	*repstr		const char	Replacement string		*
 *									*
 * Output parameters:							*
 *	*outstr		char		Output string			*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/00						*
 ***********************************************************************/
{
    size_t	lenstr, lensub, lenrep, rembyt;
    char	*cptr, *tptr, *tmpstr;
/*---------------------------------------------------------------------*/
/*
 *  Validate the output string variable.
 */
    if ( outstr == (char *)NULL ) {
	*iret = -1;
	return;
    }
    *iret = G_NORMAL;
/*
 *  Copy the input string to the output string if not already the same.
 */
    if ( string != outstr ) { 
	strcpy( outstr, string );
    }
/*
 *  Validate the input parameters.
 */
    if ( string == (char *)NULL || strlen(string) == 0 ) {
	return;
    }
    else if ( substr == (char *)NULL || strlen(substr) == 0 ) {
	return;
    }
    else if ( repstr == (char *)NULL ) {
	return;
    }
/*
 *  Use strstr to locate the substitute string.
 */
    cptr = strstr ( outstr, substr );
    if ( cptr == (char *)NULL ) {  /*  Substring not found.  */
	return;
    }
    else {  /*  Substring found.  */
	lenrep = strlen(repstr);
	lensub = strlen(substr);
	if ( lenrep == lensub ) {
	    strncpy ( cptr, repstr, lenrep );
	}
	else if ( lenrep < lensub ) {
	    strncpy ( cptr, repstr, lenrep );
	    rembyt = strlen((cptr+lensub)) + 1;
	    memmove ( (cptr+lenrep), (cptr+lensub), rembyt );
	}
	else  {
	    lenstr = strlen(string);
	    tmpstr = (char *)malloc( lenstr-lensub+lenrep+1 * sizeof(char) );

	    strcpy( tmpstr, outstr );
	    tptr = (char *)strstr ( tmpstr, substr );

	    strncpy ( tptr, repstr, lenrep );
	    strcpy ( (tptr+lenrep), (cptr+lensub) );

	    strcpy( outstr, tmpstr );

	    free ( tmpstr );
	}
    }
}
