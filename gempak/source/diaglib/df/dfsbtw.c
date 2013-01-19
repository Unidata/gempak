#include "df.h"

void df_sbtw ( int *iret )
/************************************************************************
 * df_sbtw								*
 *									*
 * This subroutine finds values of S1 which are between the two values	*
 * of S2 and S3.							*
 *									*
 *     SBTW (S1, S2, S3) IF S1 > S2 AND S1 < S3 THEN S1 ELSE RMISSD	*
 *									*
 * df_sbtw ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * S. Maxwell/GSC        7/97   Copied from DFADD                       *
 * S. Maxwell/GSC        8/97   Corrected header documentation;		*
 * 				Expanded gt and lt checking	        *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int kxd, kyd, ksub1, ksub2, i, im1, ier, zero;
    int num1, num2, num3, num;
    float *gnum1, *gnum2, *gnum3, *gnum, dg1, dg2, dg3;
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
	    if ( (( dg1 > dg2 ) && ( dg1 < dg3 )) ||
	         (( dg1 > dg3 ) && ( dg1 < dg2 )) ) {
		gnum[im1] = dg1;
	    } else {
		gnum[im1] = RMISSD;
	    }
	}
    }

    /*
     * Get a name of the form 'SBTW'//S1//S2//S3 and update header;
     * update stack.
     */
    dg_updh ( "SBTW", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
