#include "df.h"

void df_bncdf ( int *iret )
/************************************************************************
 * df_bncdf								*
 *									*
 * This subroutine computes the binormal cumulative distribution        *
 * function given a particular value, the mode, left standard deviation *
 * (sigl), and right standard deviation (sigr).  The evaluation first	*
 * determines whether the point of interest is on the left or right side*
 * of the mode.  If it is left of the mode, the integration is done	*
 * using the cumulative normal distribution with mean = mode and	*
 * standard deviation = sigl, scaled by sigl / ( sigl + sigr ).		*
 * If the point is to the right of the mode, the integration is done	*
 * using the cumulative normal distribution with mean = mode and	*
 * standard deviation = sigr, scaled by sigr / ( sigl + sigr ), then the*
 * area to the left of the mode [=sigl/(sigl+sigr)] is added.		*
 *									*
 *     BNCDF (S1, S2, S3, S4) = { INTEGRAL [-inf -> Z] BINORMAL DIST.}  *
 *									*
 * This gives the probability of S <= S1.  For the probability of	*
 * S >= S1, subtract this result from one.  For the probability of S	*
 * between two values, take the absolute value of the difference of	*
 * this result for each of the two values.				*
 *									*
 * The binormal distribution is described by Toth and Szentimrey (1990).*
 *									*
 * The standard normal distribution is described and tabulated in Meyer	*
 * (1970).  The numerical integration algorithm is the Simpson Composite*
 * Algorithm described in Burden et. al (1978).				*
 *									*
 * References:								*
 *									*
 * Burden, R. L., J. D. Faires, and A. C. Reynolds, 1978:  NUMERICAL	*
 *     ANALYSIS.  Prindle, Weber & Schmidt, ISBN 0-87150-243-7, 579 pp. *
 *									*
 * Meyer, P. L., 1970:  INTRODUCTORY PROBABILITY AND STATISTICAL	*
 *     APPLICATIONS.  Addison-Wesley Pub. Co., ISBN 0-201-04710-1,	*
 *     367 pp.								*
 *									*
 * Toth, Z., and T. Szentimrey, 1990:  The binormal distribution:  A	*
 *     distribution for representing asymmetrical but normal-like	*
 *     weather elements.  J. CLIMATE, 3, 128--136.			*
 *									*
 * df_bncdf ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/HPC		 4/08						*
 ************************************************************************/
{
    int num1, num2, num3, num4, num, kxd, kyd, ksub1, ksub2, zero, ier;
    int i, im1, k, kend, m;
    float *gnum1, *gnum2, *gnum3, *gnum, hstep, cnorm, val, mode, sigl;
    float *gnum4, sigr, sig, scale, pm;
    float zstop, azstp, sign, x, h, sum;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the three input grid numbers.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num2, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num3, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num4, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num2, &gnum2, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num3, &gnum3, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num4, &gnum4, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Set up integration parameters.
     */ 
    hstep = .05;
    cnorm = 1. / ( 3. * sqrt ( 2.0 * PI ) );

    /*
     * Begin calculation.
     */
    for ( i = ksub1; i <= ksub2; i++ ) {
        im1 = i - 1;
	val = gnum1[im1];
	mode = gnum2[im1];
	sigl = gnum3[im1];
	sigr = gnum4[im1];
	if ( ERMISS (val) || ERMISS (mode) || ERMISS (sigl) ||
	     ERMISS (sigr) || sigl < 0.00001 || sigr < 0.00001 ) {
	    gnum[im1] = RMISSD;
	} else if ( G_ABS ( val - mode ) < 0.0001 ) {
	    gnum[im1] = sigl / ( sigl + sigr );
	} else {
	    pm = sigl / ( sigl + sigr );
/*
	    Note that 1 - pm = sigr / ( sigl + sigr ).
*/
	    if ( val < mode ) {
		sig = sigl;
		scale = 2.0 * pm;
	    } else {
		sig = sigr;
		scale = 2.0 * ( 1.0 - pm );
	    }
	    zstop = ( val - mode ) / sig;
	    azstp = G_ABS ( zstop );
	    if ( zstop > 7. ) {
		gnum[im1] = 1.0;
	    } else if ( zstop < -7. ) {
		gnum[im1] = 0.0;
	    } else {
		if ( zstop < 0.0 ) {
		    sign = -1.0;
		} else {
		    sign = 1.0;
		}
		m = (int) ( azstp / ( 2. * hstep ) );
		if ( m < 4 ) m = 4;
		h = azstp / ( 2. * m );

		/*
		 * Perform Simpson's Composite Algorithm to integrate.
		 */
		x = 0.;
		sum = 1.;
		kend = 2 * m;
		for ( k = 1; k <= kend; k++ ) {
		    x = x + h;
		    if ( k == kend ) {
			sum += exp ( - x * x / 2. );
		    } else if ( ( k % 2 ) != 0 ) {
			sum += 4. * exp ( - x * x / 2. );
		    } else {
			sum += 2. * exp ( - x * x / 2. );
		    }
		}
		sum *= ( h * cnorm );
		gnum[im1] = pm + sign * sum * scale;
	    }
	}
    }

    /*
     * Make a name of the form 'BNCDF'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "BNCDF", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
