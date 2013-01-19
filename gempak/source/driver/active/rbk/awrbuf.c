#include "ardcmn.h"

void awrbuf ( unsigned char in[], int num, int *iret )
/************************************************************************
 * awrbuf								*
 *									*
 * This subroutine is called to write the array contents directly into  *
 * the output file.                                                     *
 *									*
 * awrbuf ( in, num, iret )						*
 *									*
 * Input parameters:							*
 *	in [num]	unsigned char	Array to be written		*
 *	num		int		Number of array elements	*
 *									*
 * Output parameters:							*
 * 	*iret		int 		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC		9/98	        Modified from UWRBUF            *
 ***********************************************************************/
{
        int     ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	If this is not the first item in the file, increment the
 *	file counter.
 */
	if  ( numout != 0 )  numout = numout + 1;
	
/*
 *	Add the data to the output file.
 */
  
        cfl_writ ( flun, num, in, &ier);
	
/*
 *	Add the number of bytes of data to the file counter.
 */
	numout = numout + (num - 1);

}
