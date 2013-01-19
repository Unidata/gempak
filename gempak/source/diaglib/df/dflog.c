#include "df.h"

void df_log ( int *iret )
/************************************************************************
 * df_log								*
 *									*
 * This subroutine computes the logarithm to the base 10 of a scalar 	*
 * grid:								*
 *									*
 *     LOG (S) = LOG10 (S)						*
 *									*
 * using the standard FORTRAN function LOG10.				*
 *									*
 * df_log ( iret )							*
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
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, kxd, kyd, ksub1, ksub2, zero, ier, i, im1;
    float *gnum1, *gnum, dg1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the first grid number.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 )  return;

    /*
     * Get a new grid number and compute the log base 10.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 )  return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    for ( i = ksub1; i <= ksub2; i++ ) {
	im1 = i - 1;      
	dg1 = gnum1[im1];
	if ( ( dg1 <= 0. ) || ERMISS (dg1) ) {
	    gnum[im1] = RMISSD;
	} else {
	    gnum[im1] = log10 ( dg1 );
	}
    }

    /*
     * Make a name of the form 'LOG'//S and update header;
     * update stack.
     */
    dg_updh ( "LOG", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
