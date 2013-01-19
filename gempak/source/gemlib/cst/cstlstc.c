#include "geminc.h"
#include "gemprm.h"

void cst_lstc ( char **carr, int num, const char *sep, int maxchr,
                char *string, int *iret )
/************************************************************************
 * cst_lstc                                                             *
 *                                                                      *
 * This subroutine takes an array of strings and builds a single string *
 * consisting of each element of the array, separated by 'sep'.		*
 *                                                                      *
 * cst_lstc ( carr, num, sep, string, iret )                            *
 *                                                                      *
 * Input parameters:                                                    *
 *      **carr		char		Array of strings		*
 *	num		int		Number of strings in the array	*
 *      *sep		const char      Separator			*
 *	maxchr		int		Maximum number of characters in	*
 *					the output variable (string)	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *string		char            String				*
 *      *iret           int             Return code                     *
 *                                       0 = normal return              *
 *                                       2 = Exceeded string size	*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          1/06   From ST_LSTC                            *
 ************************************************************************/
{
    int ier, ii, lens;
    char strtmp[MXFLSZ+1];
/*----------------------------------------------------------------------*/
    *iret = 0;
    string[0] = '\0';

    if ( num > 0 ) {
	for ( ii = 0; ii < num; ii++ ) {
	    cst_ldsp ( carr[ii], strtmp, &lens, &ier );

/*
 * Check if next element will fit.
 */
	    if ( ((int)strlen(string) + lens) >= maxchr ) {
/*
 * Remove the trailing separator.
 */
		string[strlen(string)-1] = '\0';
	        *iret = 2;
		return;
	    }

	    if ( ii == 0 ) {
	        strcpy ( string, strtmp );
	    } else {
	        strcat ( string, strtmp );
	    }

	    if ( ii < num - 1 ) {
	        strcat ( string, sep );
	    }
	}
    }
}
