#include "dv.h"

void dv_vor ( int *iret )
/************************************************************************
 * dv_vor								*
 *									*
 * This subroutine computes the vorticity of a vector:			*
 *									*
 *     VOR ( V ) = DDX(v) - DDY(u) - v*{(mx/my)*[d(my)/dx]} +		*
 *				   + u*{(my/mx)*[d(mx)/dy]}		*
 *									*
 * where mx and my are scale factors along x and y, respectively.  The	*
 * quantities in braces are assumed to be in common arrays YMSDX and    *
 * XMSDY, respectively.  VOR generates a scalar grid.			*
 *									*
 * dv_vor ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * I. Graffman/RDS	 7/88	Call to DG_UPDH				*
 * G. Huffman/GSC	 9/88	Additional stack functions		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC          4/89   Added scale factor code			*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * T. Lee/GSC            4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				using int grds for scl fctr derivatives *
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEp	11/05	Translation from Fortran                *
 ************************************************************************/
{
	const int	zero = 0;
	int		i, ier, nval, kxd, kyd, ksub1, ksub2;
	int		numu, numv, numout, nvddx, nuddy;
	float		*gru, *grv, *grout, *grvddx, *gruddy;
	int		ixmsdy, iymsdx;
	float		*grxmdy, *grymdx;

/*---------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the (wind) vector.
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Get a new grid number.
         */
	dg_nxts  ( &numout, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute x derivative of v component.
         */
	dg_puts ( &numv, iret );
	if ( *iret != 0 ) return;
	df_ddx ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &nvddx, iret );
	if ( *iret != 0 ) return;

        /*
         *	Compute y derivative of u component.
         */
	dg_puts ( &numu, iret );
	if ( *iret != 0 ) return;
	df_ddy ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &nuddy, iret );
	if ( *iret != 0 ) return;

        /*
         *	Compute map scale factor derivative coefficients.
         */
	dg_dmsf ( iret );
	if ( *iret != 0 ) return;

        nval = 1;
        dg_iget ( "IXMSDY", &nval, &ixmsdy, iret );
        dg_iget ( "IYMSDX", &nval, &iymsdx, iret );
        dg_getg ( &ixmsdy, &grxmdy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &iymsdx, &grymdx, &kxd, &kyd, &ksub1, &ksub2, iret );

        /*
         *	Combine terms to compute vorticity.
         */
        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nvddx, &grvddx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nuddy, &gruddy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numout, &grout, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if ( ERMISS ( grvddx[i] ) || ERMISS ( gruddy[i] ) ||
     		 ERMISS ( gru[i] ) || ERMISS ( grv[i] ) )
	       grout[i] = RMISSD;
	    else
	       grout[i] = grvddx[i] - gruddy[i] -
     		 	  grv[i] * grymdx[i] +
     			  gru[i] * grxmdy[i];
	    
	}

        /*
         *	Make a name of the form 'VOR'//u and update header;
         *	update stack.
         */
	dg_updh  ( "VOR", &numout, &numu, &zero, iret );
	dg_puts  ( &numout, iret );
	dg_esub  ( &numout, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
