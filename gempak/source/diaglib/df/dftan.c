#include "df.h"

void df_tan ( int *iret )
/************************************************************************
 * df_tan								*
 *									*
 * This subroutine computes the tangent of a scalar grid:		*
 *									*
 *     TAN  ( S )							*
 *									*
 * The angles in the scalar grid must be in radians.			*
 *									*
 * df_tan ( iret )							*
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
 * K. Brill/GSC		10/89   Subsetting				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, kxd, kyd, ksub1, ksub2, i, im1, ier, zero;
    float *gnum1, *gnum, dg1, pi3ov2;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the input grid number.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number and compute the tangent.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    pi3ov2 = TWOPI - HALFPI;
    for ( i = ksub1; i <= ksub2; i++ ) {
	im1 = i - 1;
	dg1 = gnum1[im1];
	if ( dg1 > TWOPI )  dg1 = dg1 - TWOPI;
	if ( ( ! ERMISS (dg1)) && ( dg1 < 0. ) ) dg1 = dg1 + TWOPI;
	if ( ( !G_DIFF(dg1, HALFPI) ) && ( !G_DIFF(dg1, pi3ov2) ) && ( ! ERMISS (dg1) ) ) {
	    gnum[im1] = tan ( dg1 );
	} else {
	    gnum[im1] = RMISSD;
	}
    }

    /*
     * Make a name of the form 'TAN'//S and update header;
     * update stack.
     */
    dg_updh ( "TAN", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
