#include "geminc.h"
#include "gemprm.h"


void cst_ncat ( char *str1, char *str2, int *len, int *iret )
/************************************************************************
 * cst_ncat								*
 *									*
 * This subroutine concatinates the contents of str2 into str1 up to    *
 * len characters then appends the NULL at the end of str1.		* 
 *									*
 * cst_ncpy ( str1, str2, len, iret )					*
 *									*
 * Input parameters:							*
 *	*str2		char		Target string			*
 *									*
 * Output parameters:							*
 *	*str1		char		Destination string		*
 *	len		int		Length 				*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					 -1 = invalid length		*
 **									*
 * Log:									*
 * A. Hardy/NCEP	 5/03						*
 ***********************************************************************/
{

    	int 	len1, len2, leng, ier;
/*---------------------------------------------------------------------*/

	*iret = 0;

       /*
        * Find lengths of strings 1 and 2.
	*/

        cst_lstr ( str1, &len1, &ier );
	cst_lstr ( str2, &len2, &ier );

       /*
        * Add two to the length to account for the null.
	*/

	leng =  len1 + len2 + 2;

	strncat( str1, str2, leng );

       /*
        * Find total length of concatinated strings to find
	* end for the null.
	*/

	cst_lstr (str1, &leng, &ier );
	str1[leng] = '\0';
	cst_lstr ( str1, len, &ier );
}
