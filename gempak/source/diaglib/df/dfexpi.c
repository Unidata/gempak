#include "df.h"

void df_expi ( int *iret )
/************************************************************************
 * df_expi								*
 *									*
 * This subroutine computes the exponential value of a scalar grid	*
 * raised to the nearest integer power of the second scalar grid:	*
 *									*
 *     EXPI (S1, S2) = S1 ** NINT ( S2 )				*
 *									*
 * df_expi ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * I. Graffman/RDS	 2/87						*
 * W. Skillman/GSFC	 5/88	Added new stack subroutines		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC		 8/89	Subsetting				*
 * K. Brill/GSC		10/89	Subsetting				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, num, kxd, kyd, ksub1, ksub2, fidx, cidx, zero, ier;
    float *gnum1, *gnum2, *gnum, dg1, dg2;
/*----------------------------------------------------------------------*/
    *iret = 0;

    dg_ssub ( iret );

    /*
     * Get two grid numbers.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;

    dg_gets ( &num2, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number and compute the exponential.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num2, &gnum2, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
        cidx = fidx - 1;
	dg1 = gnum1[cidx];
	dg2 = gnum2[cidx];
	if ( ERMISS (dg1) || ERMISS (dg2) ) {
	    gnum[cidx] = RMISSD;
	} else {
	    gnum[cidx] = pow ( dg1, G_NINT (dg2) );
	}
    }

    /*
     * Make a name of the form EXI//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "EXI", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
