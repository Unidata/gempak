#include "dv.h"

void dv_str ( int *iret )
/************************************************************************
 * dv_str								*
 *									*
 * This subroutine computes the stretching deformation of a vector:	*
 *									*
 *     STR ( V ) = DDX ( u ) - DDY ( v ) + u * {(mx/my)*[d(my)/dx]}	*
 *					 - v * {(my/mx)*[d(mx)/dy]}	*
 *									*
 * where mx and my are scale factors along x and y, respectively.	*
 * The quantities in braces are assumed to exist in common arrays	*
 * YMSDX and XMSDY, respectively.  STR generates a scalar grid.		*
 *									*
 * dv_str ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * I. Graffman/RDS	 7/88	Call to DG_UPDH				*
 * G. Huffman/GSC	 9/88	New stack routines			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. F. Brill/GSC       4/89   Added map scale factor code 		*
 * K. Brill/GSC	 	 8/89   Subsetting				*
 * K. Brill/GSC	        10/89   Subsetting 				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				using int grds for scl fctr derivatives *
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        const int       zero=0;
	int		i, ier, kxd, kyd, ksub1, ksub2, nval;

        int             numu, numv, nddx, nddy, ixmsdy, iymsdx, numout;
        float           *gru, *grv, *grddx, *grddy, *grxmdy, *grymdx, *grout;
        float           dx, dy, dd, uu;

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the vector.
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the u component on the stack, compute DDX, and get the result.
         */
	dg_puts  ( &numu, iret );
	if  ( *iret != 0 ) return;
	df_ddx  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &nddx, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the v component on the stack, compute DDY, and get the result.
         */
	dg_puts  ( &numv, iret );
	if  ( *iret != 0 ) return;
	df_ddy  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &nddy, iret );
	if  ( *iret != 0 ) return;

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
         *	Get a new grid and compute the stretching deformation.
         */
	dg_nxts  ( &numout, iret );
	if  ( *iret != 0 )  return;

        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nddx, &grddx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nddy, &grddy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numout, &grout, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i< ksub2; i++ ) {
		dx =  grddx[i];
		dy =  grddy[i];
		dd =  grv[i];
	 	uu =  gru[i];
		if  ( ERMISS (dx) || ERMISS (dy) ||
     		      ERMISS (dd) || ERMISS (uu) )
		    grout[i] = RMISSD;
		else
		    grout[i] = dx - dy - dd * grxmdy[i] + uu * grymdx[i];
		
	}

        /*
         *	Make a name of the form 'STR'//u and update header;
         *	update the stack.
         */
	dg_updh  ( "STR", &numout, &numu, &zero, iret );
	dg_puts  ( &numout, iret );
	dg_esub  ( &numout, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
