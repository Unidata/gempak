#include "dv.h"

void dv_divt  ( int *iret )
/************************************************************************
 * dv_divt								*
 *									*
 * This subroutine computes the divergence tendency of a vector:	*
 *									*
 *     DIVT ( S, V ) = DIV ( INAD ( V, V) ) - LAP ( S )			*
 *                     - DIV ( SMUL ( CORL, KCRS ( V ) ) )		*
 *									*
 * Map scale factors are included implicitly.				*
 *									*
 *									*
 * dv_divt  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * D. McCann/AWC   	 4/01						*
 * A. Hardy/GSC          7/01	Modified div. calc. for var. map projs. *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             i, ier, zero=0, kxd, kyd, ksub1, ksub2;
	int		nums, numu, numv, ncorl;
        int             ndiv, nlap, nddiv, ndivt;
        float           *grdiv, *grlap, *grddiv, *grdivt;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *  	Get scaler and (wind) vectors and put them back on the stack.
         */
	dg_gets ( &nums, iret );
	if ( *iret != 0 ) return;
	dg_getv ( &numu, &numv, iret );
	if ( *iret != 0 ) return;

        /*
         *      Compute the inertial advective wind vector.
         */
	dg_putv ( &numu, &numv, iret );
	if ( *iret != 0 ) return;
	dg_putv ( &numu, &numv, iret );
	if ( *iret != 0 ) return;
	dv_inad  ( iret );
	if ( *iret != 0 ) return;
	dv_div  ( iret );
	if ( *iret != 0 ) return;
	dg_gets  ( &ndiv, iret  );
	if ( *iret != 0 ) return;

        /*
         *	Put scaler on the stack and compute Laplacian.
         */
	dg_puts  ( &nums, iret  );
	if ( *iret != 0 ) return;
	df_lap ( iret );
	if ( *iret != 0 ) return;
	dg_gets  ( &nlap, iret  );
	if ( *iret != 0 ) return;

        /*
         *      Compute the coriolis grid.
         */
	dg_nxts ( &ncorl, iret );
	if ( *iret != 0 ) return;
	df_corl  ( &ncorl, iret );
	if ( *iret != 0 ) return;

        /*
         *      Compute the curl of the wind.
         */
	dg_putv ( &numu, &numv, iret );
	if ( *iret != 0 ) return;
	dv_kcrs ( iret );
	if ( *iret != 0 ) return;
	dg_puts ( &ncorl, iret );
	if ( *iret != 0 ) return;
	dv_smul ( iret );
	if ( *iret != 0 ) return;
	dv_div  ( iret );
	if ( *iret != 0 ) return;
	dg_gets  ( &nddiv, iret  );
	if ( *iret != 0 ) return;

        /*
         *	Compute divergence tendency.
         */	
	dg_nxts ( &ndivt, iret );
	if ( *iret != 0 ) return;

        dg_getg ( &ndiv, &grdiv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nlap, &grlap, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nddiv, &grddiv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &ndivt, &grdivt, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if ( ERMISS ( grdiv[i] ) || ERMISS ( grlap[i] ) ||
     		 ERMISS ( grddiv[i] ) )
	      grdivt[i] = RMISSD;
	    else
	      grdivt[i] = - grdiv[i] - grlap[i] - grddiv[i];
	    
	}

        /*
         *	Make a name of the form 'DIVT'//S//u and update header;
         *	update stack.
         */
	dg_updh  ( "DIVT", &ndivt, &nums, &numu, iret );
	dg_puts  ( &ndivt, iret  );
	dg_esub  ( &ndivt, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
