#include "geminc.h"
#include "gemprm.h"

int mv_ve32 ( int *n, float *valin, float *valout )
/************************************************************************
 * mv_ve32								*
 *									*
 * This function converts an array of VAX 32-bit real numbers to 	*
 * IEEE 32-bit real numbers.  The input and output arrays may be the	*
 * same.								*
 *									*
 * int mv_ve32  ( n, valin, valout )					*
 *									*
 * Input parameters:							*
 *	*n		int		Number of values to convert	*
 *	*valin		float		Input data			*
 *									*
 * Output parameters:							*
 *	*valout		float		Converted data			*
 *	mv_ve32		int		Return code			*
 *					 0 = normal return		*
 *					>0 = # of invalid inputs	*
 **									*
 * Log:									*
 * Leslie Lait/GSFC							*
 * T. Piper/GSC		02/90	Changed calling sequence		*
 * M. desJardins/GSFC	 2/91	Renamed to MV_VE32			*
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
	    inn = (char *) valin;
	    out = (char *) temp;
	    Ltemp = (long *) temp;

/*
 *	    Extract and copy sign.
 */
	    out[0] = inn[1] & 128;

/*
 *	    Extract and copy mantissa.
 */
	    out[3] = inn[2];
	    out[2] = inn[3];
	    out[1] = inn[0] & 127;

/*
 *	    Extract exponent.
 */
	    expont = ((inn[1] & 127)<<1) | ((inn[0] >> 7) & 1);
	    if (expont != 0) {
/*
 *		Convert exp to IEEE format...
 */
		if ( (expont -= 2) == 0) {
/*
 *		    and, if necessary, force to underflow.
 */
		    expont = -1;
		}
	    }

	    if ((expont == 0) && (*Ltemp != 0)) {
		fprintf ( stderr,
		  "Illegal Floating point value in real conversion\n" );
		break;
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
/*
 *		NOTE: this is not possible in this conversion
 */
		fprintf ( stderr,
		  "Floating point overflow in real conversion\n" );
		break;
	    }
	    else {
/*
 *		OK to convert.
 */
		out[0] = out[0] | (expont >> 1);
		out[1] = out[1] | ((expont &  1)<<7);
	    }

	    *valout = *temp;
	    valin++;
	    valout++;
	}

	return i;

}
