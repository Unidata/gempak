#include "geminc.h"
#include "gemprm.h"

void mv_itob ( int *ivalue, int *istart, int *nbytes, 
				unsigned char *barray, int *iret )
/************************************************************************
 * mv_itob								*
 *									*
 * This function converts an integer into the specified number of bytes.*
 * Negative numbers are assumed to be stored as the number with the the	*
 * first bit equal to 1. This is consistent with the GRIB and GINI	*
 * coding practices only.						*
 *									*
 * mv_itob ( ivalue, istart, nbytes, barray, iret )			*
 *									*
 * Input parameters:							*
 *	*ivalue		int		Integer value			*
 *	*istart		int		Start byte in data		*
 *	*nbytes		int		Number of bytes requested	*
 *									*
 * Input/Output parameters:						*
 *	*barray		unsigned char	Data buffer			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/96	Copied from MV_BTOI			*
 ***********************************************************************/
{
	int		i, itemp;

/*---------------------------------------------------------------------*/
	*iret = 0;

/*
 *	Get the absolute value of the integer.
 */
	itemp = G_ABS ( *ivalue );

/*
 *	Loop over the number of bytes for packing the integer.
 */
	for ( i = *nbytes; i > 0; i-- )
	{
	    barray [*istart+i-1] = ( itemp >> (8*(*nbytes-i)) ) & 0xff;
	}

/*
 *	Set the first bit for a negative value.
 */
	if  ( *ivalue < 0 )
	{
	    barray [*istart] |= 0x80;
	}

}
