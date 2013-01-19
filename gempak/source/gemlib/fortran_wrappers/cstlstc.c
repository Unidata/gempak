#include "fortran_wrappers.h"

void cst_lstc ( char **carr, int num, const char *sep, int maxchr,
                char *string, int *iret )
/************************************************************************
 * cst_lstc                                                             *
 *                                                                      *
 * This subroutine takes an array of strings and builds a single string *
 * consisting of each element of the array, separated by SEP.		*
 *                                                                      *
 * cst_lstc ( carr, num, sep, string, iret )                            *
 *                                                                      *
 * Input parameters:                                                    *
 *      **carr		char		Array of strings		*
 *	num		int		Number of strings in the array	*
 *      *sep		const char      Separator			*
 *	maxchr		int		Maximum characters in strings	*
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
    char strtmp[MXFLSZ+1];
    int lens, ier, i;
/*----------------------------------------------------------------------*/
    *iret = 0;
    string[0] = '\0';

    if ( num > 0 ) {
	for ( i = 0; i < num; i++ ) {
	    cst_ldsp ( carr[i], strtmp, &lens, &ier );

	    /*
	     * Check is next element will fit.
	     */
	    if ( (int)strlen ( string ) + lens >= maxchr ) {
	        /*
		 * Remove the trailing separator.
		 */
		string[strlen(string)-1] = '\0';
	        *iret = 2;
		return;
	    }

	    if ( i == 0 ) {
	        strcpy ( string, strtmp );
	    } else {
	        strcat ( string, strtmp );
	    }

	    if ( i < num - 1 ) {
	        strcat ( string, sep );
	    }
	}
    }

    return;
}
