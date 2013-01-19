#include "df.h"

void df_paub ( int *iret )
/************************************************************************
 * df_paub								*
 *									*
 * This subroutine computes the combined probability of S1 OR S2, given	*
 * the dependence parameter S3 as follows:				*
 *									*
 *    PAUB (S1, S2, S3) = S1 + S2 - ( MAX(S1,S2) ** S3 ) * MIN(S1,S2)	*
 *									*
 * The last term uses a statical model of the probability of S1 AND S2  *
 * based on the empirical parameter S3.					*
 *									*
 * This statistical model may apply to any probabilities for which this	*
 * empirical parameter method is appropriate.  If the events associated	*
 * with probabilities S1 AND S2 are statistically independent then	*
 * S3 = 1.  If the the events are completely dependent, S3 = 0.  Note	*
 * that the dependence parameter appears as an exponent on the larger	*
 * of the two probabilities.						*
 *									*
 * All three arguments, S1, S2, and S3, must have a value between 0 and	*
 * 1, inclusive of both bounds.						*
 *									*
 * References:								*
 *									*
 * Hughs, L. A., and W. E. Sangster, 1979:  Combining precipitation	*
 *     probabilities.  Mon. Wea. Rev., 107, 521--524.			*
 *									*
 * Wilks, D. S., 1990:  On the combination of forecast probabilities	*
 *     for consecutive precipitation periods.  Wea. & Forecasting, 5,	*
 *     640--650.							*
 *									*
 * df_paub ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/HPC		 1/03						*
 * K. Brill/HPC		 9/03	Make sure final prob is in [0,1]	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, num3, num, kxd, kyd, ksub1, ksub2, i, im1, zero, ier;
    float *gnum1, *gnum2, *gnum3, *gnum, dg1, dg2, dg3, a, b;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get three grids from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num2, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num3, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number and check the grids.
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

    for ( i = ksub1; i <= ksub2; i++ ) {
	im1 = i - 1;
	dg1 = gnum1[im1];
	dg2 = gnum2[im1];
	dg3 = gnum3[im1];
	if ( ERMISS ( dg1 ) || ERMISS ( dg2 ) || ERMISS ( dg3 ) ) {
	    gnum[im1] = RMISSD;
	} else {
	    if ( dg1 >= dg2 ) {
		a = dg1;
		b = dg2;
	    } else {
		a = dg2;
		b = dg1;
	    }
	    if ( dg3 < 0. ) dg3 = 0.0;
	    if ( dg3 > 1. ) dg3 = 1.0;
	    gnum[im1] = a + b - ( pow ( a, dg3 ) * b );
	    if ( gnum[im1] < 0. ) gnum[im1] = 0.0;
	    if ( gnum[im1] > 1. ) gnum[im1] = 1.0;
	}
    }

    /*
     * Get a name of the form 'PAUB'//S1//S2//S3 and update header;
     * update stack.
     */
    dg_updh ( "PAUB", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
