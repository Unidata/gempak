#include "df.h"

void df_jcbn ( int *iret )
/************************************************************************
 * df_jcbn								*
 *									*
 * This subroutine computes the Jacobian determinant of two scalar	*
 * grids:								*
 *									*
 *     JCBN ( S1, S2 ) = DDX(S1) * DDY(S2) - DDY(S1) * DDX(S2)		*
 *									*
 * Map scale factors are included implicitly.				*
 *									*
 *									*
 * df_jcbn ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/GSC    	10/89						*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int ns1, ns1dx, ns1dy, ns2, ns2dx, ns2dy, njcbn;
    int kxd, kyd, ksub1, ksub2, i, im1, zero, ier;
    float *gns1dx, *gns1dy, *gns2dx, *gns2dy, *gnjcbn;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get two scalar grids from the stack.
     */
    dg_gets ( &ns1, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &ns2, iret );
    if ( *iret != 0 ) return;

    /*
     * Compute the needed derivatives.
     */
    dg_puts ( &ns1, iret );
    if ( *iret != 0 ) return;
    df_ddx ( iret );
    if ( *iret != 0 ) return;
    dg_gets ( &ns1dx, iret );
    if ( *iret != 0 ) return;
    dg_puts ( &ns1, iret );
    if ( *iret != 0 ) return;
    df_ddy ( iret );
    if ( *iret != 0 ) return;
    dg_gets ( &ns1dy, iret );
    if ( *iret != 0 ) return;

    dg_puts ( &ns2, iret );
    if ( *iret != 0 ) return;
    df_ddx ( iret );
    if ( *iret != 0 ) return;
    dg_gets ( &ns2dx, iret );
    if ( *iret != 0 ) return;
    dg_puts ( &ns2, iret );
    if ( *iret != 0 ) return;
    df_ddy ( iret );
    if ( *iret != 0 ) return;
    dg_gets ( &ns2dy, iret );
    if ( *iret != 0 ) return;

    /*
     * Compute the Jacobian.
     */
    dg_nxts ( &njcbn, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &ns1dx, &gns1dx, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &ns1dy, &gns1dy, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &ns2dx, &gns2dx, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &ns2dy, &gns2dy, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &njcbn, &gnjcbn, &kxd, &kyd, &ksub1, &ksub2, iret );

    for ( i = ksub1; i <= ksub2; i++ ) {
        im1 = i - 1;
	if ( ERMISS ( gns1dx[im1] ) || ERMISS ( gns1dy[im1] ) ||
             ERMISS ( gns2dx[im1] ) || ERMISS ( gns2dy[im1] ) ) {
	    gnjcbn[im1] = RMISSD;
	} else {
	    gnjcbn[im1] = gns1dx[im1] * gns2dy[im1] - 
	                  gns1dy[im1] * gns2dx[im1];
	}
    }

    /*
     * Make a name of the form 'JCBN'//S and update header;
     * update stack.
     */
    dg_updh ( "JCBN", &njcbn, &ns1, &ns2, iret );
    dg_puts ( &njcbn, iret );
    dg_esub ( &njcbn, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
