#include "df.h"

void df_atn2 ( int *iret )
/************************************************************************
 * df_atn2								*
 *									*
 * This subroutine computes the arc tangent of the quotient of two 	*
 * scalar grids:							*
 *									*
 *     ATAN2 (S1, S2) = ATAN2 ( S1 / S2 )				*
 *									*
 * df_atn2 ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. Goodman/RDS	11/85						*
 * W. Skillman/GSFC	 5/88	Added new stack subroutines		*
 * G. Huffman/GSC	 8/88	Correct answer at infinity		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * G. Huffman/GSC	 4/89	Correct first infinity test to denom.	*
 * K. Brill/GSC          8/89   Subsetting				*
 * K. Brill/GSC         10/89   Subsetting				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * T. Piper/GSC		11/98	Updated prolog				*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		10/05	Translated from Fortran			*
 ************************************************************************/
{
    int num1, num2, num, kxd, kyd, ksub1, ksub2, fidx, cidx, zero, ier;
    float *gnum1, *gnum2, *gnum, dg1, dg2;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the two grids from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num2, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num2, &gnum2, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Compute the arc tangent.
     */
    for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
	cidx = fidx - 1;
        dg1 = gnum1[cidx];
        dg2 = gnum2[cidx];

	/*
    	 * Cases are error, non-zero denom., zero denom. with neg. 
	 * numerator, zero denom. with non-neg. numerator.
	 */
        if ( ERMISS ( dg1 ) || ERMISS ( dg2 ) ) {
	    gnum[cidx] = RMISSD;
        } else if  ( !G_DIFFT(dg2, 0.0F, GDIFFD) ) {
	    gnum[cidx] = atan2 ( dg1, dg2 );
        } else if ( dg1 < 0.0F ) {
	    gnum[cidx] = -HALFPI;
        } else {
	    gnum[cidx] = HALFPI;
	}
    }

    /*
     * Get a name of the form 'ATAN'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "ATAN", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
