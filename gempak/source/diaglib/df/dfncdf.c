#include "df.h"

void df_ncdf ( int *iret )
/************************************************************************
 * df_ncdf								*
 *									*
 * This subroutine computes the normal cumulative distribution function *
 * given a particular value, the mean, and standard deviation:		*
 *									*
 *     NCDF (S1, S2, S3) = { INTEGRAL [-inf -> Z] EXP ( -u**2 / 2 ) } /	*
 *				SQRT ( 2 * PI )				*
 *									*
 *     where u = (z - S2) / S3 and Z = (S1 - S2) / S3			*
 *									*
 * This gives the probability of S <= S1.  For the probability of	*
 * S >= S1, subtract this result from one.  For the probability of S	*
 * between two values, take the absolute value of the difference of	*
 * this result for each of the two values.				*
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
 * df_ncdf ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/HPC		10/02						*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, num3, num, kxd, kyd, ksub1, ksub2, zero, ier;
    int i, im1, k, kend, m;
    float *gnum1, *gnum2, *gnum3, *gnum, hstep, cnorm, dg1, dg2, dg3;
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
	dg1 = gnum1[im1];
	dg2 = gnum2[im1];
	dg3 = gnum3[im1];
	if ( ERMISS (dg1) || ERMISS (dg2) || ERMISS (dg3) ||
	    dg3 < 0.00001 ) {
	    gnum[im1] = RMISSD;
	} else {
	    zstop = ( dg1 - dg2 ) / dg3;
	    azstp = G_ABS ( zstop );
	    if ( azstp < 0.0001 ) {
		gnum[im1] = .500;
	    } else if ( zstop > 7. ) {
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
		gnum[im1] = .5 + sign * sum;
	    }
	}
    }

    /*
     * Make a name of the form 'NCDF'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "NCDF", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
