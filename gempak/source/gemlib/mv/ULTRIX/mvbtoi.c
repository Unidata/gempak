#include "geminc.h"
#include "gemprm.h"

void
mv_btoi ( barray, istart, nbytes, negflg, ivalue, iret )
	unsigned char	*barray;
	int		*istart;
	int		*nbytes;
	int		*negflg;
	int		*ivalue;
	int		*iret;

/************************************************************************
 * mv_btoi								*
 *									*
 * This function converts the specified number of bytes into an		*
 * integer. Negative numbers are assumed to be stored as the number	*
 * with the the first bit equal to 1. This is consistent with the GRIB	*
 * and GINI coding practices only.					*
 *									*
 * mv_btoi ( barray, istart, nbytes, negflg, ivalue, iret )		*
 *									*
 * Input parameters:							*
 *	*barray		unsigned char	Data buffer			*
 *	*istart		int		Start byte in data		*
 *	*nbytes		int		Number of bytes requested	*
 *	*negflg		int		Negative value flag		*
 *									*
 * Output parameters:							*
 *	*ivalue		int		Integer value of the bytes	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * J. Cowie/COMET	12/95	Similar to gb_btoi but uses 0 for index,*
 *				input args are pointers			*
 * S. Jacobs/NCEP	 6/96	Changed calling sequence and func type	*
 ***********************************************************************/

{
	int		i, itemp, index, sign=1;

/*---------------------------------------------------------------------*/
	*iret = 0;

/*
 *	Initialize the variables used in the computation.
 */
	*ivalue = 0;
	index   = *istart;

/*
 *	Loop over the number of bytes in the integer.
 */
	for ( i = 0; i < *nbytes; i++ ) {

/*
 *	    Set the temporary integer and increment the array counter.
 */
	    itemp = (int) barray[index];
	    index++;

/*
 *	    If this is the first byte and this is a negative number,
 *	    pull off the first bit and set the sign multiplier.
 */
	    if  ( ( i == 0 ) && ( *negflg ) )
	    {
		if  ( ( ( itemp & 128 ) >> 7 ) == 1 )
		{
		    itemp &= 127;
		    sign = -1;
		}
	    }

/*
 *	    Shift the integer value, and add the next byte.
 */
	    *ivalue = ( *ivalue << 8 ) | itemp;
	}

/*
 *	Apply the sign multiplier.
 */
	*ivalue *= sign;
}
