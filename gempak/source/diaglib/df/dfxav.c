#include "df.h"

void df_xav ( int *iret )
/************************************************************************
 * df_xav								*
 *									*
 * This subroutine computes the average of a scalar using all valid	*
 * points along an internal grid row:					*
 *									*
 *     XAV (S) = [ S (X1) + S (X2) + ... + S (KXD) ] / KNT		*
 *									*
 *               Where: KXD = number of points in row			*
 *                      KNT = number of non-missing points in row	*
 *									*
 * The XAV for a row is stored at every point in that row.		*
 *									*
 * df_xav ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * I. Graffman/RDS	 1/87						*
 * G. Huffman/GSC	 9/88	Error messages				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Compute only between JGX/YMIN & JGX/YMAX*
 * R. Tian/SAIC		12/02	Try to make loop more clear		*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, jgymin, jgymax, jgxmin, jgxmax, kxd, kyd, ksub1, ksub2,
        knt, iy, ix, ii, ier, zero;
    float *gnum1, *gnum, sum, avg;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the scalar grid.
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
     * Compute the average for each row.
     */
    dg_qbnd ( &jgxmin, &jgxmax, &jgymin, &jgymax, iret );
    for ( iy = jgymin; iy <= jgymax; iy++ ) {
	sum = 0.0;
	knt = 0;
	for ( ix = jgxmin; ix <= jgxmax; ix++ ) {
	    ii = ( iy - 1 ) * kxd + ix;
	    if ( ! ERMISS ( gnum1[ii-1] ) ) {
		knt++;
		sum += gnum1[ii-1];
	    }
	}
	if ( knt == 0 ) {
	    avg = RMISSD;
	} else {
	    avg = sum / knt;
	}
	for ( ix = jgxmin; ix <= jgxmax; ix++ ) {
	    ii = ( iy - 1 ) * kxd + ix;
	    if ( ! ERMISS ( gnum1[ii-1] ) ) {
		gnum[ii-1] = avg;
	    } else {
		gnum[ii-1] = RMISSD;
	    }
	}
    }

    /*
     * Make a name of the form 'XAV'//S and update header;
     * update stack.
     */
    dg_updh ( "XAV", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
