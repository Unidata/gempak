#include "df.h"

void df_avg ( int *iret )
/************************************************************************
 * df_avg								*
 *									*
 * This subroutine averages two scalar grids: 				*
 *									*
 *     AVG (S1, S2) = ( S1 + S2 ) / 2					*
 *									*
 * df_avg ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * G. Huffman/GSC	 8/88	Adapted from DF_ADD			*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		10/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, num, kxd, kyd, ksub1, ksub2, fidx, cidx, zero, ier;
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
     * Get a new grid.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num2, &gnum2, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num , &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Compute the average.
     */
    for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
	cidx = fidx - 1;
        dg1 = gnum1[cidx];
        dg2 = gnum2[cidx];
        if ( ERMISS ( dg1 ) || ERMISS ( dg2 ) ) {
	    gnum[cidx] = RMISSD;
        } else {
	    gnum[cidx] = ( dg1 + dg2 ) / 2.;
        }
    }

    /*
     * Get a name of the form 'AVG'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "AVG", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
