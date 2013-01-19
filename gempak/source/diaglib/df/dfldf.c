#include "df.h"

void df_ldf ( int *iret )
/************************************************************************
 * df_ldf								*
 *									*
 * This subroutine computes the layer difference of a scalar:		*
 *									*
 *     LDF (S) = S (level1) - S (level2)				*
 *									*
 * df_ldf ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 7/88	Added new stack subroutines		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC  	10/89   Subsetting				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		10/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, num, kx, ky, ksub1, ksub2, fidx, cidx, ier, zero;
    float *gnum1, *gnum2, *gnum, dg1, dg2;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the grids from the stack.
     */
    dg_getl ( &num1, &num2, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kx, &ky, &ksub1, &ksub2, iret );
    dg_getg ( &num2, &gnum2, &kx, &ky, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kx, &ky, &ksub1, &ksub2, iret );

    /*
     * Subtract the grids.
     */
    for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
	cidx = fidx - 1;
        dg1 = gnum1[cidx];
        dg2 = gnum2[cidx];
        if ( ERMISS (dg1) || ERMISS (dg2) ) {
	    gnum[cidx] = RMISSD;
	} else {
	    gnum[cidx] = dg1 - dg2;
	}
    }

    /*
     * Make a name of the form 'LDF'//S and update header;
     * update stack.
     */
    dg_updh ( "LDF", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
