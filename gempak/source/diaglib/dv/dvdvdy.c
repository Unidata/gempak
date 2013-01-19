#include "dv.h"

void dv_dvdy ( int *iret )
/************************************************************************
 * dv_dvdy								*
 *									*
 * This subroutine computes the y-derivative of a vector:		*
 *									*
 *     DVDY ( V ) = [ DDY (u) + v * ( (mx/my) * d(my)/dy ),		*
 *			 DDY (v) - u * ( (mx/my) * d(my)/dy ) ]		*
 *									*
 * where mx and my are scale factors along x and y, respectively.  	*
 *									*
 * dv_dvdy ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * K. Brill/NMC	         1/93						*
 * S. Jacobs/NMC	 4/94	Clean up				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * T. Piper/GSC		11/98	Updated prolog				*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				using int grds for scl fctr derivatives *
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             i, ier, kxd, kyd, ksub1, ksub2, nval, zero=0;
	int		nu, nv, numu, numv;
	float		*grnu, *grnv, *grnumu, *grnumv; 
        int             nuddy, nvddy, iymsdx;
        float           *gruddy, *grvddy, *grymsdx;

/*-----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the (wind) vector.
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Get a new vector grid number.
         */
	dg_nxtv  ( &nu, &nv, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute y derivatives of u and v components.
         */
	dg_puts ( &numu, iret );
	if ( *iret != 0 ) return;
	df_ddy ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &nuddy, iret );
	if ( *iret != 0 ) return;
	dg_puts ( &numv, iret );
	if ( *iret != 0 ) return;
	df_ddy ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &nvddy, iret );
	if ( *iret != 0 ) return;

        /*
         *	Compute map scale factor derivative coefficients.
         */
	dg_dmsf ( iret );
	if ( *iret != 0 ) return;

        nval = 1;
        dg_iget ( "IYMSDX", &nval, &iymsdx, iret );
        dg_getg ( &iymsdx, &grymsdx, &kxd, &kyd, &ksub1, &ksub2, iret );

        /*
         *	Compute the output vector components.
         */
        dg_getg ( &nu, &grnu, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nv, &grnv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu, &grnumu, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grnumv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nuddy, &gruddy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nvddy, &grvddy, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if ( ERMISS ( grvddy[i] ) || ERMISS ( gruddy[i] ) ||
     		 ERMISS ( grnumu[i] ) || ERMISS ( grnumv[i] ) ) {
	       grnu[i] = RMISSD;
	       grnv[i] = RMISSD;
            }
	    else {
	       grnu[i] = gruddy[i] + grnumv[i] * grymsdx[i];
	       grnv[i] = grvddy[i] - grnumu[i] * grymsdx[i];
	    }
	}

        /*
         *	Make a name of the form 'DVDY'//u and update header;
         *	update stack.
         */
	dg_updv  ( "DVDY", &nu, &nv, &numu, &zero, iret );
	dg_putv  ( &nu, &nv, iret );
	dg_esub  ( &nu, &nv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
