#include "df.h"

void df_exp ( int *iret )
/************************************************************************
 * df_exp								*
 *									*
 * This subroutine computes the exponential value of one scalar grid	*
 * raised to the power of the second scalar grid:			*
 *									*
 *     EXP (S1, S2) = S1 ** S2						*
 *									*
 * If S1 is negative, the output will be the missing data value (integer*
 * powers of negative numbers of ARE allowed in DF_EXPI).		*
 *									*
 * df_exp ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * W. Skillman/GSFC	 5/88	Added new stack functions		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC          8/89   Subsetting				*
 * K. Brill/GSC         10/89   Subsetting				*
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
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the two input grid numbers.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;

    dg_gets ( &num2, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number and subtract the grids.
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
	if ( ERMISS (dg1) || ERMISS (dg2) || ( dg1 < 0. ) ) {
	    gnum[cidx] = RMISSD;
	} else {
	    gnum[cidx] = pow ( dg1, dg2 );
	}
    }

    /*
     * Make a name of the form 'EXP'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "EXP", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;
    
    return;
}
