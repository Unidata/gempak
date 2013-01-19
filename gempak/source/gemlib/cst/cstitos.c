#include "geminc.h"
#include "gemprm.h"

#define MAXCHAR 4

void cst_itos ( int *intptr, int nval, int *nchar, char *str, int *iret )
/************************************************************************
 * cst_itos								*
 *									*
 * This subroutine decodes an array of integers into a single string.	*
 *									*
 * cst_itos ( intptr, nval, nchar, str, iret )				*
 *									*
 * Input parameters:							*
 *	*intptr		int		Pointer to integer values	*
 *	nval		int		Number of integers		*
 *									*
 * Output parameters:							*
 *	*nchar		int	 	Number of characters 		*
 *	*str		char		Character string		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *									*
 **									*
 * Log:									*
 * L. Williams/EAI	 5/96						*
 * S. Jacobs/NCEP	 8/96	Updated header format			*
 * R. Tian/SAIC		09/06	Rewrote.  It is correct now		*
 ***********************************************************************/
{
    int	ii, jj, kk;
    int	clen, indx, endx;
    union {
	int	value;
	char 	byte[MAXCHAR];
    } data;
/*---------------------------------------------------------------------*/
/*
 *  Check the number of integers.
 */
    if ( nval <= 0 ) {
	*nchar = 0;
	*str = '\0';
	*iret = -1;
	return;
    }
    *iret = 0;
/*
 *  Process each integer.
 */
    kk = 0;
    for ( ii = 0; ii < nval; ii++ ) {
	data.value = intptr[ii];
/*
 *  Check for the start of data. 
 */
	indx = 0;
	for ( jj = 0; jj < MAXCHAR; jj++ ) {
	    if ( data.byte[jj] != 0 ) {
                 indx = jj;
		 break;
	    }
	}
/*
 *  Check for the end of data.
 */
	endx = MAXCHAR;
	for ( jj = indx + 1; jj < MAXCHAR; jj++ ) {
	    if ( data.byte[jj] == 0 ) {
                 endx = jj;
		 break;
	    }
	}
/*
 *  Move bytes to char.
 */
	clen = endx - indx;
	memcpy( &(str[kk]), &(data.byte[indx]), clen );
	kk += clen;
    }
/*
 *  NULL terminate the string and set nchar.
 */
    str[kk] = '\0';
    *nchar = kk;
}
