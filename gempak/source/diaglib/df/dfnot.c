#include "df.h"

void df_not ( int *iret )
/************************************************************************
 * df_not								*
 *									*
 * This function is invoked as  NOT ( S ).  It returns 1 if S == 0;	*
 * otherwise 0.								*
 *									*
 * df_not ( iret )							*
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
    int num1, num, kxd, kyd, ksub1, ksub2, i, im1, zero, ier;
    float *gnum1, *gnum, dg1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get ONE grid from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number for the output grid.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    for ( i = ksub1; i <= ksub2; i++ ) {
        im1 = i - 1;
	dg1 = gnum1[im1];
	if ( ERMISS ( dg1 ) ) {
	    gnum[im1] = RMISSD;
	} else {
	    if( G_DIFFT(dg1, 0.0F, GDIFFD) ) {
		gnum[im1] = 1.0F;
	    } else {
	  	gnum[im1] = 0.0F;
	    }
	}
    }

    /*
     * Get a name of the form 'NOT'//S1/ and update header;
     * update stack.
     */
    dg_updh ( "NOT", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
