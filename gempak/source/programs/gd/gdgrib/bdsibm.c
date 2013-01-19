#include "gdgrib.h"

void bds_ibm ( float *xref, unsigned char *ibm, int *iret )
/************************************************************************
 * bds_ibm								*
 *									*
 * This subroutine converts a floating point number to an IBM single	*
 * precision number.							*
 *									*
 * Note:  this conversion  does not handle subnormal numbers.		*
 *									*
 * bds_ibm ( xref, ibm, iret )						*
 *									*
 * Input parameters:							*
 *	*xref		float	Floating point number to convert	*
 *									*
 * Output parameters:							*
 *	*ibm		unsigned char	Pointer to bytes to hold IBM #	*
 *	*iret		int	Return code				*
 *				   0 = successful conversion		*
 *				  -1 = unsuccessful conversion		*
 * Log:									*
 * Wesley Ebisuzaki							*
 * K. Brill/HPC		 8/99	Documentation				*
 * R. Tian/SAIC		10/06	Added G_DIFF call			*
 ***********************************************************************/
{
	int sign, exp;
	double mant;
	int imant;
	float x;

	*iret = 0;
	x = *xref;
	if ( G_DIFF ( x, 0.0 ) ) {
		ibm[0] = ibm[1] = ibm[2] = ibm[3] = 0;
		return;
	}

	/* sign bit */
	if (x < 0.0) {
		sign = 128;
		x = -x;
	}
	else sign = 0;

	mant = frexp((double) x, &exp);

	/* round up by adding 2**-24 */
	/* mant = mant + 1.0/16777216.0; */

	if (mant >= 1.0) {
		mant = 0.5;
		exp++;
	}
	while (exp & 3) {
		mant *= 0.5;
		exp++;
	}

        imant = floor(mant * 256.0 * 256.0 * 256.0 + 0.5);
        if (imant >= 256 * 256 * 256) {
            /* imant = 256 * 256 * 256 - 1; */
            imant = floor(mant * 16.0 * 256.0 * 256.0 + 0.5);
            exp -= 4;
	}
	
	exp = exp/4 + 64;

	if (exp < 0) {
		fprintf(stderr,"underflow in flt2ibm\n");
		ibm[0] = ibm[1] = ibm[2] = ibm[3] = 0;
		return;
	}
	if (exp > 127) {
		fprintf(stderr,"overflow in flt2ibm\n");
		ibm[0] = sign | 127;
		ibm[1] = ibm[2] = ibm[3] = 255;
		*iret = -1;
		return;
	}

	/* normal number */

	ibm[0] = sign | exp;

        ibm[3] = imant & 255;
        ibm[2] = (imant >> 8) & 255;
        ibm[1] = (imant >> 16) & 255;

	return;
}
