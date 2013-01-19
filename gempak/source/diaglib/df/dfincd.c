#include "df.h"
#include "dfinvnrm.h"

void df_incd ( int *iret )
/************************************************************************
 * df_incd								*
 *									*
 * This subroutine computes the inverse of the normal cumulative	*
 * distribution function given a cumulative probability value, the mean,*
 * and standard deviation:						*
 *									*
 * INCD (S1, S2, S3) = z such that					*
 * S1 = { INTEGRAL [-inf -> z] EXP ( -u**2 / 2 ) } / SQRT ( 2 * PI )	*
 *									*
 *     where u = (z - S2) / S3 so that z = S3 * Z + S2			*
 *									*
 * This function finds the value, z, such that the cumulative 		*
 * probability integrated to z is the given value S1.			*
 *									*
 * The standard normal distribution is described and tabulated in Meyer	*
 * (1970).								*
 *									*
 * References:								*
 *									*
 * http://home.online.no/~pjacklam/notes/invnorm/			*
 *                                       #An_overview_of_the_algorithm  *
 * C implementation by V. Natarajan (Kanchipuram (near Madras), India)  *
 *									*
 *									*
 * Meyer, P. L., 1970:  INTRODUCTORY PROBABILITY AND STATISTICAL	*
 *     APPLICATIONS.  Addison-Wesley Pub. Co., ISBN 0-201-04710-1,	*
 *     367 pp.								*
 *									*
 * df_incd ( iret )							*
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
    int num1, num2, num3, num, kxd, kyd, ksub1, ksub2, zero, ier;
    int i, im1;
    float *gnum1, *gnum2, *gnum3, *gnum, dg1, dg2, dg3;
    float area;
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
        } else if ( dg1 <= 0.0 || dg1 >= 1.0 ) {
	    gnum[im1] = RMISSD;
	} else {
	    area = G_ABS ( dg1 - .500 );
	    if ( area < 0.0001 ) {
		gnum[im1] = dg2;
	    } else {
		p = dg1;
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
		gnum[im1] = x * dg3 + dg2;
	    }
	}
    }

    /*
     * Make a name of the form 'NCDF'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "INCD", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
