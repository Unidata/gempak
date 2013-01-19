#include "utfcmn.h"

void uwrbuf ( unsigned char in[], int num, int *iret )
/************************************************************************
 * uwrbuf								*
 *									*
 * This subroutine is called to write the array contents into the	*
 * output buffer.  It swaps out AFOS control characters in the array	*
 * before writing it to the buffer.					*
 *									*
 * uwrbuf ( in, num, iret )						*
 *									*
 * Input parameters:							*
 *	in [num]	unsigned char	Array to be written		*
 *	num		int		Number of array elements	*
 *									*
 * Output parameters:							*
 * 	*iret		int 		Return code			*
 **									*
 * Log:									*
 * E. Safford/GSC	11/96	Initial coding				*
 * S. Jacobs/NCEP	 8/97	Updated header and comments		*
 ***********************************************************************/
{

	unsigned char	swapd[MXAFOS];
	int		lensw, i, ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Remove any special values that happen to occur in the data.
 */
	uswap ( in, num, swapd, &lensw, &ier );

/*
 *	Check to see if the swap resulted in an array too large for the 
 *	available space in the buffer, and return an error if so.
 */
	if  ( MXAFOS <= (lensw + numout) )  {
	    *iret = G_NAFSMX;
	    return;
	}

/*
 *	If this is not the first item in the file, increment the
 *	buffer counter.
 */
	if  ( numout != 0 )  numout = numout + 1;
	
/*
 *	Add the "swapped" data to the output buffer.
 */
	for ( i = 0; i < lensw; i++ ) 
	    outbuf[numout + i] = swapd[i];
	
/*
 *	Add the number of bytes of data to the buffer counter.
 */
	numout = numout + (lensw - 1);

}
