#include "geminc.h"
#include "gemprm.h"

#define MAXCHAR 4

void cst_stoi ( char *str, int nchar, int *nval, int *intptr, int *iret )
/************************************************************************
 * cst_stoi								*
 *									*
 * This subroutine stores a character string in an integer array.	*
 * Four characters are written to each integer.				*
 *									*
 * cst_stoi ( str, nchar, nval, intptr, iret )				*
 *									*
 * Input parameters:							*
 *	*str		char		Character string		*
 *	nchar		int		Number of characters		*
 *									*
 * Output parameters:							*
 *	*nval		int		Number of integers		*
 *	*intptr		int		Pointer to integer values	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = invalid number of chars	*
 **									*
 * Log:									*
 * L. Williams/EAI	 5/96						*
 * S. Jacobs/NCEP	 8/96	Updated header format			*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 ***********************************************************************/
{
union {
	int	value;
	char	byte[4];
} data;

int	i, j;

/*---------------------------------------------------------------------*/

	*iret=0;

	/*
	 * check the number of characters
	 */
	if( nchar <= 0 ) {
	   *nval = 0;
	   *intptr = 0;
	   *iret = -1;
	   return;
	}
	/*
	 * process the input string
	 */
	j=0;
	for( i=0; i < nchar; i += MAXCHAR ) {
	   memcpy( data.byte, &(str[i]), MAXCHAR );
	   intptr[j] = data.value;
	   ++j;
	}
	*nval = j;

}
