#include "df.h"

void df_ddy ( int *iret )
/************************************************************************
 * df_ddy								*
 *									*
 * This subroutine computes DDY (S), the derivative of a scalar with	*
 * respect to Y.  The partial derivative is computed using centered	*
 * finite differences.  Scale factors are used.				*
 *									*
 *									*
 * df_ddy ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M.Goodman/RDS 	10/85						*
 * M. desJardins/GSFC	 7/88	Added new stack subroutines		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. F. Brill/GSC	 4/89   Added scale factor code			*
 * K. Brill/GSC	         8/89   Subsetting				*
 * K. Brill/GSC         10/89   Subsetting				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Tyle/GSC		 6/96	Compute only within sub-area		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids for y scl fctr	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		10/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, ier, i, j, indx, im, ip, zero;
    int ixmscl, iymscl, jgymin, jgymax, jgxmin, jgxmax, kxd, kyd, ksub1,
        ksub2;
    float gddx, gddy, dy2, rmscl;
    float *gnum1, *gscl, *gnum;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Compute map scale factors.
     */
    dg_mscl ( iret );
    if ( *iret != 0 ) return;

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
     * Query DGCMN.CMN gddy, iymscl, jgymin, jgymax, jgxmin, jgxmax.
     */
    dg_qmsl ( &ixmscl, &iymscl, &gddx, &gddy, &ier );
    dg_qbnd ( &jgxmin, &jgxmax, &jgymin, &jgymax, &ier );

    /*
     * Compute the partial derivative and place it in the new grid.
     */
    dy2 = 2. * gddy;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1,   &gnum1, &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &num,    &gnum,  &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &iymscl, &gscl,  &kxd, &kyd, &ksub1, &ksub2, &ier );

    /*
     * Compute derivative on internal rows.
     */
    for ( j = jgymin + 1; j <= jgymax -1; j++ ) {
	for ( i = jgxmin; i <= jgxmax; i++ ) {
	    indx = i + ( j - 1 ) * kxd;
     	    im = indx - kxd;
	    ip = im + 2 * kxd;
	    if ( ERMISS ( gnum1[ip-1] ) || ERMISS ( gnum1[im-1] ) ) {
	        gnum[indx-1] = RMISSD;
	    } else {
	        gnum[indx-1] = ( gnum1[ip-1] - gnum1[im-1] ) * gscl[indx-1] / dy2;
	   }
	}
    }

    /*
     * Compute one-sided derivatives along bottom row.
     */
    for ( i = jgxmin; i <= jgxmax; i++ ) {
	indx = i + ( jgymin - 1 ) * kxd;
	im = indx;
	ip = im + kxd;
	if ( ERMISS ( gnum1[ip-1] ) || ERMISS ( gnum1[im-1] ) ) {
	    gnum[indx-1] = RMISSD;
	} else {
	    rmscl = .5 * ( gscl[ip-1] + gscl[im-1] );
	    gnum[indx-1] = ( gnum1[ip-1] - gnum1[im-1] ) * rmscl / gddy;
	}

        /*
         * Compute one-sided derivative along top row.
	 */
	indx = i + ( jgymax - 1 ) * kxd;
  	im = indx - kxd;
  	ip = indx  ;
	if ( ERMISS ( gnum1[ip-1] ) || ERMISS ( gnum1[im-1] ) ) {
	    gnum[indx-1] = RMISSD;
	} else {
      	    rmscl = .5 * ( gscl[ip-1] + gscl[im-1] );
      	    gnum[indx-1] = ( gnum1[ip-1] - gnum1[im-1] ) * rmscl / gddy;
	}
    }

    /*
     * Make a name of the form 'DDY'//S and update header;
     * update stack.
     */
    dg_updh ( "DDY", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
