#include "df.h"

void df_abs ( int *iret )
/************************************************************************
 * df_abs								*
 *									*
 * This subroutine computes the absolute value of a scalar grid:	*
 *									*
 *     ABS (S) 								*
 *									*
 * df_abs ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * I. Graffman/RDS	 1/87						*
 * M. desJardins/GSFC	 5/88	Added new stack functions		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC         10/89   Subsetting				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC         10/05   Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, kx, ky, ksub1, ksub2, fidx, cidx, zero, ier;
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
    dg_getg ( &num1, &gnum1, &kx, &ky, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kx, &ky, &ksub1, &ksub2, iret );

    /*
     * Compute the absolute value.
     */
    for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
        cidx = fidx - 1;
	dg1 = gnum1[cidx];
	if ( ERMISS ( dg1 ) ) {
	    gnum[cidx] = RMISSD;
	} else {
	    gnum[cidx] = G_ABS ( gnum1[cidx] );
	}
    }

    /*
     * Get a name of the form 'ABS'//S and update header;
     * update stack.
     */
    dg_updh ( "ABS", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
