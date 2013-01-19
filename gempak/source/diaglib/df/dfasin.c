#include "df.h"

void df_asin ( int *iret )
/************************************************************************
 * df_asin								*
 *									*
 * This subroutine computes the arc sine of a scalar grid:		*
 *									*
 *     ASIN (S)								*
 *									*
 * where S is in radians.						*
 *									*
 * df_asin ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. Goodman/RDS	11/85						*
 * W. Skillman/GSFC	 5/88	Added new stack subroutines		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89	Subsetting				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		10/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, kxd, kyd, ksub1, ksub2, zero, ier, fidx, cidx;
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
     * Get a new grid.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Compute the arc sine.
     */
    for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
	cidx = fidx - 1;
        dg1 = gnum1[cidx];
        if ( ( dg1 < -1. ) || ( dg1 > 1. ) ) {
	    gnum[cidx] = RMISSD;
	} else {
	    gnum[cidx] = asin ( dg1 );
	}
    }

    /*
     * Get a name of the form 'ASIN'//S and update header;
     * update stack.
     */
    dg_updh ( "ASIN", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
