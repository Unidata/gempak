#include "df.h"
#include "dfinvnrm.h"

void df_ibncd ( int *iret )
/************************************************************************
 * df_ibncd								*
 *									*
 * This subroutine computes the inverse of the binormal cumulative	*
 * distribution function given a cumulative probability value, the mode,*
 * the left standard deviation (sigl), and the right standard deviation *
 * (sigr).  See the documentation for df_bncdf for details and		*
 * references.								*
 *									*
 * IBNCD (S1, S2, S3, S4) = z such that					*
 * S1 = { INTEGRAL [-inf -> z] BINORMAL DIST. }				*
 *									*
 * This function finds the value, z, such that the cumulative 		*
 * probability integrated to z is the given value S1.			*
 *									*
 * df_ibncd ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/HPC         04/08   Implement Natarajan code for		*
 *				P. J. Acklam's fast inversion algorithm	*
 ************************************************************************/
{
    int num1, num2, num3, num4, num, kxd, kyd, ksub1, ksub2, zero, ier;
    int i, im1;
    float *gnum1, *gnum2, *gnum3, *gnum4, *gnum, cprb, mode, sigl, sigr;
    float area, sig, pm, scale;
    long double x, p;
    long double q, r;
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
     * Begin calculation.
     */
    for ( i = ksub1; i <= ksub2; i++ ) {
        im1 = i - 1;
	cprb = gnum1[im1];
	mode = gnum2[im1];
	sigl = gnum3[im1];
	sigr = gnum4[im1];
	if ( ERMISS (cprb) || ERMISS (mode) || ERMISS (sigl) ||
	     ERMISS (sigr) || sigl < 0.00001 || sigr < 0.00001 ) {
	    gnum[im1] = RMISSD;
        } else if ( cprb <= 0.0 || cprb >= 1.0 ) {
	    gnum[im1] = RMISSD;
	} else {
	    pm = sigl / ( sigl + sigr );
	    area = G_ABS ( cprb - pm );
	    if ( area < 0.0001 ) {
		gnum[im1] = mode;
	    } else {
		if ( cprb < pm ) {
		   sig = sigl;
		   scale = 2. * pm;
		   p = cprb / scale;
		} else {
		   sig = sigr;
		   scale = 2. * ( 1.0 - pm );
		   p = ( cprb - pm ) / scale + .500;
		}
		if ( (0 < p) && (p < P_LOW) ) {
   		    q = sqrt(-2*log(p));
   		    x = (((((C1*q+C2)*q+C3)*q+C4)*q+C5)*q+C6) / ((((D1*q+D2)*q+D3)*q+D4)*q+1);
		} else {
        	    if ( (P_LOW <= p) && (p <= P_HIGH) ){
           		q = p - 0.5;
           		r = q*q;
           		x = (((((A1*r+A2)*r+A3)*r+A4)*r+A5)*r+A6)*q / (((((B1*r+B2)*r+B3)*r+B4)*r+B5)*r+1);
       	            } else {
                        if ( (P_HIGH < p) && (p < 1) ) {
                   	    q = sqrt(-2*log(1-p));
                   	    x = -(((((C1*q+C2)*q+C3)*q+C4)*q+C5)*q+C6) / ((((D1*q+D2)*q+D3)*q+D4)*q+1);
                        }
                    }
		}
/*
		The value x is for the standardized normal distribution.
*/
		gnum[im1] = x * sig + mode;
	    }
	}
    }

    /*
     * Make a name of the form 'IBNCD'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "IBNCD", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
