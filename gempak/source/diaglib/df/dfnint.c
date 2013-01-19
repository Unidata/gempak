#include "df.h"

void df_nint ( int *iret )
/************************************************************************
 * df_nint								*
 *									*
 * This subroutine rounds the scalar value to the nearest integer:	* 
 *									*
 *     NINT (S) 							*
 *									*
 * df_nint ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * A. Hardy/NCEP	 8/03						*
 * R. Tian/SAiC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, kxd, kyd, ksub1, ksub2, i, im1, zero, ier;
    float *gnum1, *gnum, dg1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get one grid from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number and compute the absolute value.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    for ( i = ksub1; i <= ksub2; i++ ) {
        im1 = i - 1;
	dg1 = gnum1[im1];
	if ( ERMISS ( dg1 ) ) {
	    gnum[im1] = RMISSD;
	} else {
	    gnum[im1] = G_NINT ( dg1 );
 	}
    }

    /*
     * Get a name of the form 'NINT'//S and update header;
     * update stack.
     */
    dg_updh ( "NINT", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
