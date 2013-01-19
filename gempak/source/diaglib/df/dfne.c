#include "df.h"

void df_ne ( int *iret )
/************************************************************************
 * df_ne								*
 *									*
 * This function is invoked as  NE ( S1, S2, S3 ).  It returns 1 	*
 * if |S1-S2| > S3; otherwise 0.					*
 *									*
 * df_ne ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 	*iret		int		Return code			*
 *					0 - normal return 		*
 **									*
 * Log:									*
 * m.gamazaychikov/SAIC	09/05						*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, num3, num, kxd, kyd, ksub1, ksub2, i, im1, zero, ier;
    float *gnum1, *gnum2, *gnum3, *gnum, dg1, dg2, dg3;
/*----------------------------------------------------------------------*/
    *iret = 0;

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
	    if ( G_ABS ( dg1 - dg2 ) > dg3 ) {
		gnum[im1] = 1.0;
	    } else {
		gnum[im1] = 0.0;
	    }
	}
    }

    /*
     * Get a name of the form 'NE'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "NE", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
