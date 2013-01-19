#include "df.h"

void df_sgt ( int *iret )
/************************************************************************
 * df_sgt								*
 *									*
 * This subroutine finds values of S1 which are greater than S2.	*
 *									*
 *     SGT (S1, S2) IF S1 > S2	THEN S1 ELSE RMISSD			*
 *									*
 * df_sgt ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * S. Maxwell/GSC        7/97   Coped from DFADD                        *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, num, kxd, kyd, ksub1, ksub2, i, im1, ier, zero;
    float *gnum1, *gnum2, *gnum, dg1, dg2;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get two grids from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num2, iret );
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
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    for ( i = ksub1; i <= ksub2; i++ ) {
	im1 = i - 1;
	dg1 = gnum1[im1];
	dg2 = gnum2[im1];
	if ( ERMISS ( dg1 ) || ERMISS ( dg2 ) ) {
	    gnum[im1] = RMISSD;
	} else {
	    if ( dg1 > dg2 ) {
		gnum[im1] = dg1;
	    } else {
		gnum[im1] = RMISSD;
	    }
	}
    }

    /*
     * Get a name of the form 'SGT'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "SGT", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
