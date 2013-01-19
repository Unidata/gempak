#include "df.h"

void df_ysum ( int *iret )
/************************************************************************
 * df_ysum								*
 *									*
 * This subroutine computes the sum of a scalar internal grid at all	*
 * valid points along a column:						*
 *									*
 *     YSUM (S) = [ S (Y1) + S (Y2) + ... + S (KYD) ] 			*
 *									*
 *                Where: KYD = number of points in column		*
 *									*
 * The YSUM for a column is stored at every point in that column.	*
 *									*
 * df_ysum ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * I. Graffman/RDS	 1/87						*
 * M. desJardins/GSFC	 7/88	Added new stack subroutines		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Avg only on JGX/YMIN -> JGX/YMAX	*
 * R. Tian/SAIC         12/02   Try to make loop more clear             *
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, jgymin, jgymax, jgxmin, jgxmax, kxd, kyd, ksub1, ksub2,
        knt, iy, ix, ii, ier, zero;
    float *gnum1, *gnum, sums, sum;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the grid from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Compute the sum for each column.
     */
    dg_qbnd ( &jgxmin, &jgxmax, &jgymin, &jgymax, iret );
    for ( ix = jgxmin; ix <= jgxmax; ix++ ) {
	sums = 0.0;
	knt = 0;
	for ( iy = jgymin; iy <= jgymax; iy++ ) {
	    ii = ( iy - 1 ) * kxd + ix;
	    if ( ! ERMISS ( gnum1[ii-1] ) ) {
		knt++;
		sums += gnum1[ii-1];
	    }
	}
	if ( knt == 0 ) {
	    sum = RMISSD;
	} else {
	    sum = sums;
	}
	for ( iy = jgymin; iy <= jgymax; iy++ ) {
	    ii = ( iy - 1 ) * kxd + ix;
	    if ( ! ERMISS ( gnum1[ii-1] ) ) {
		gnum[ii-1] = sum;
	    } else {
		gnum[ii-1] = RMISSD;
	    }
	}
    }

    /*
     * Make a name of the form 'YSUM'//S and update header;
     * update stack.
     */
    dg_updh ( "YSUM", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
