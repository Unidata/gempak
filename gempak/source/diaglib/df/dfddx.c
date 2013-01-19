#include "df.h"

void df_ddx ( int *iret )
/************************************************************************
 * df_ddx								*
 *									*
 * This subroutine computes DDX (S), the partial derivative of S with	*
 * respect to X.  DDX is computed using centered finite differences.	*
 * Scale factors are used.						*
 *									*
 *									*
 * df_ddx ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. Goodman/RDS	10/85						*
 * M. desJardins/GSFC	 7/88	Added new stack subroutines		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. F. Brill/GSC       4/89   Added scale factor code			*
 * K. Brill/GSC          8/89   Subsetting				*
 * K. Brill/GSC         10/89   Subsetting				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Tyle/GSC           6/96   Compute only within sub-area            *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids for x scl fctr	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, kxd, kyd, ksub1, ksub2, zero, i, j, indx, ip1, im1, ier;
    int ixmscl, iymscl, jgymin, jgymax, jgxmin, jgxmax;
    float dx2, gddx, gddy, rmscl;
    float *gnum1st, *gnumst, *gkxms;
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
     * Get the grid from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    dg_qmsl ( &ixmscl, &iymscl, &gddx, &gddy, &ier );
    dg_qbnd ( &jgxmin, &jgxmax, &jgymin, &jgymax, &ier ); 
    dg_getg ( &num1, &gnum1st, &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &num,  &gnumst,  &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &ixmscl, &gkxms, &kxd, &kyd, &ksub1, &ksub2, &ier );

    /*
     * Compute the partial derivative and place it in the new grid.
     */
    dx2 = 2. * gddx;

    /*
     * Loop over all grid rows.
     */
    for ( j = jgymin; j <= jgymax; j++ ) {
	/*  
	 * Loop over interior grid points in row j.
	 */
	for ( i = jgxmin + 1; i <= jgxmax - 1; i++ ) {
	    indx = ( j - 1 ) * kxd + i;
	    ip1 = indx + 1;
	    im1 = ip1 - 2;
      	    if ( ERMISS ( gnum1st[ip1-1] ) || ERMISS ( gnum1st[im1-1] ) ) {
		gnumst[indx-1] = RMISSD;
	    } else {
		gnumst[indx-1] = ( gnum1st[ip1-1] - gnum1st[im1-1] ) *
		    gkxms[indx-1] / dx2;
	    }
	}

	/*
	 * Compute one-sided difference at the beginning of row j.
	 */
	im1 = jgxmin + ( j - 1 ) * kxd;
	ip1 = im1 + 1;
	if ( ERMISS ( gnum1st[ip1-1] ) || ERMISS ( gnum1st[im1-1] ) ) {
	    gnumst[im1-1] = RMISSD;
	} else {
	    rmscl = .5 * ( gkxms[im1-1] + gkxms[ip1-1] );
	    gnumst[im1-1] = ( gnum1st[ip1-1] - gnum1st[im1-1] ) * rmscl / gddx;
	}

	/*
	 * Compute one-sided difference at the end of row j.
	 */
	ip1 = jgxmax + ( j - 1 ) * kxd;
	im1 = ip1 - 1;
	if ( ERMISS ( gnum1st[ip1-1] ) || ERMISS ( gnum1st[im1-1] ) ) {
	    gnumst[ip1-1] = RMISSD;
	} else {
	    rmscl = .5 * ( gkxms[im1-1] + gkxms[ip1-1] );
	    gnumst[ip1-1] = ( gnum1st[ip1-1] - gnum1st[im1-1] ) * rmscl / gddx;
	}
    }

    /*
     * Make a name of the form 'DDX'//S and update header;
     * update stack.
     */
    dg_updh ( "DDX", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
} 
