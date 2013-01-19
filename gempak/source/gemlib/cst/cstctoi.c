#include "geminc.h"
#include "gemprm.h"

#define MAXCHAR 4

void cst_ctoi ( char **str, int nstr, int *intptr, int *iret )
/************************************************************************
 * cst_ctoi								*
 *									*
 * This subroutine stores an array of 4-character strings in an array	*
 * of integers.  Each integer element contains one of the 4-character	*
 * strings.								*
 *									*
 * cst_ctoi ( str, nstr, intptr, iret )					*
 *									*
 * Input parameters:							*
 *	**str		char		Pointer to Character array	*
 *	nstr		int	 	Number of strings 		*
 *									*
 * Output parameters:							*
 *	*intptr		int		Pointer to integer values	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = invalid number of strings *
 *					 -2 = conversion error		*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 * S. Jacobs/NCEP	 8/96	Updated header format			*
 ***********************************************************************/
{
union {
	int	value;
	char 	byte[4];
} data;

int	i;


/*---------------------------------------------------------------------*/

	*iret = 0;

	/*
	 * check the number of strings
	 */
	if( nstr <= 0 ) {
	   *intptr = 0;
	   *iret = -1;
	   return;
	}

	/*
	 * process each string
	 */
	for( i=0; i < nstr; i++ ) {

	   memcpy( data.byte, str[i], MAXCHAR );
	   intptr[i] = data.value;   

	}

}
