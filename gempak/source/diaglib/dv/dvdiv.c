#include "dv.h"

void dv_div ( int *iret )
/************************************************************************
 * dv_div								*
 *									*
 * This subroutine computes the divergence of a vector:			*
 *									*
 *     DIV ( V ) = DDX ( u ) + DDY ( v ) - u * {(mx/my)*[d(my)/dx]}   	*
 *					 - v * {(my/mx)*[d(mx)/dy]}	*
 *									*
 * where my and mx are scale factors.  The quantities in braces are	*
 * assumed to exist in common arrays YMSDX and XMSDY, respectively. 	*
 * Divergence is a scalar field.					*
 *									*
 * dv_div ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * I. Graffman/RDS	 7/88	Call to DG_UPDH				*
 * G. Huffman/GSC	 9/88	New stack functions			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. F. Brill/GSC	 4/89   Added scale factor code			*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96	Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				using int grds for scl fctr derivatives *
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translate from Fortran                  *
 ************************************************************************/
{
        int             i, zero=0, nval, kxd, kyd, ksub1, ksub2, ier;
	int		numu, numv, numout, nddx, nddy, ixmsdy, iymsdx;
        float           *gru, *grv, *grout, *grddx, *grddy, *grxmsdy, *grymsdx;

/*------------------------------------------------------------------------*/
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
         *	Compute x derivative of u component.
         */
	dg_puts ( &numu, iret );
	if ( *iret != 0 ) return;
	df_ddx ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &nddx, iret );
	if ( *iret != 0 ) return;

        /*
         *	Compute y derivative of v component.
         */
	dg_puts ( &numv, iret );
	if ( *iret != 0 ) return;
	df_ddy ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &nddy, iret );
	if ( *iret != 0 ) return;

        /*
         *	Combine terms to compute divergence.
         */
        dg_getg( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &numout, &grout, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &nddx, &grddx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &nddy, &grddy, &kxd, &kyd, &ksub1, &ksub2, iret );

        /*
         *	Compute map scale factor derivative coefficients.
         */
	dg_dmsf ( iret );
	if ( *iret != 0 ) return;

        nval = 1;
        dg_iget ("IXMSDY", &nval, &ixmsdy, iret );
        dg_iget ("IYMSDX", &nval, &iymsdx, iret );
        dg_getg( &ixmsdy, &grxmsdy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &iymsdx, &grymsdx, &kxd, &kyd, &ksub1, &ksub2, iret );


	 for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if ( ERMISS ( grddx[i] ) || ERMISS ( grddy[i] ) ||
     	       	 ERMISS ( gru[i] )  || ERMISS ( grv[i] ) ) 
	       grout[i] = RMISSD;
	    else
	       grout[i] = grddx[i] + grddy[i] -
     			  gru[i] * grymsdx[i] - 
     		  	  grv[i] * grxmsdy[i];
	    
	}

        /*
         *	Make a name of the form 'DIV'//u and update header; update stack
         */
	dg_updh  ( "DIV", &numout, &numu, &zero, iret );
	dg_puts  ( &numout, iret );
	dg_esub  ( &numout, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
