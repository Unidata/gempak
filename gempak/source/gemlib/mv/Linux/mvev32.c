#include "geminc.h"
#include "gemprm.h"

int mv_ev32 ( int *n, float *valin, float *valout )
/************************************************************************
 * mv_ev32								*
 *									*
 * This function converts IEEE 32-bit real numbers to VMS 32-bit	*
 * real numbers.  The input and output arrays may be the same.		*
 *									*
 * int mv_ev32  ( n, valin, valout )					*
 *									*
 * Input parameters:							*
 *	*n		int		Number of values to convert	*
 *	*valin		float		Input data			*
 *									*
 * Output parameters:							*
 *	*valout		float		Converted data			*
 *	mv_ev32		int		Return code			*
 *					 0 = normal return		*
 *					>0 = # of invalid inputs	*
 **									*
 * Log:									*
 * Leslie Lait/GSFC							*
 * M. desJardins/GSFC	02/91		Changed calling sequence	*
 * L. Williams/EAI	 7/94		Reformat header and code	*
 ***********************************************************************/
{
	int	i;
	float	*temp, ttemp;
	char	*inn, *out;
	long	expont, *Ltemp;

/*---------------------------------------------------------------------*/
/*
 *	Translates real*4.
 */
	temp = &ttemp;

	for ( i = 0; i < *n; i++ ) {

/*
 *	    Set up to work with indiv bytes in real.
 */
	    inn   = (char *) valin;
	    out   = (char *) temp;
	    Ltemp = (long *) temp;

/*
 *	    Extract and copy sign.
 */
	    out[1] = inn[0] & 128;

/*
 *	    Extract and copy mantissa.
 */
	    out[3] = inn[2];
	    out[2] = inn[3];
	    out[0] = inn[1] & 127;

/*
 *	    Extract exponent
 */
	    expont = ((inn[0]<<1) | ((inn[1]>>7) & 1)) & 0xff;
	    if (expont != 0) {
/*
 *		Convert exp to VAX format...
 */
		if ( (expont += 2) == 0) {
/*
 *		    and, if necessary, force to overflow.
 */
		    expont = 256;
		}
	    }

	    if ((expont == 0) && (*Ltemp != 0)) {
		fprintf ( stderr,
		  "Illegal Floating point value in real conversion\n" );
		return i+1;
	    }
	    else if (expont < 0) {
		fprintf ( stderr,
		  "Floating point underflow in real conversion\n" );
		out[3] = 0;
		out[2] = 0;
		out[1] = 0;
		out[0] = 0;
	    }
	    else if (expont > 255) {
		fprintf ( stderr,
		  "Floating point overflow in real conversion\n" );
		return i+1;
	    }
	    else {
/*
 *		OK to convert.
 */
		out[1] = out[1] | ((expont >> 1) & 0x7f);
		out[0] = out[0] | (expont<<7);
	    }

	    *valout = *temp;
	    valin++;
	    valout++;
	}

	return i;

}
