#include "dv.h"

void dv_dvdx ( int *iret )
/************************************************************************
 * dv_dvdx								*
 *									*
 * This subroutine computes the x-derivative of a vector:		*
 *									*
 *     DVDX ( V ) = [ DDX (u) - v * ( (my/mx) * d(mx)/dy ),		*
 *			 DDX (v) + u * ( (my/mx) * d(mx)/dy ) ]		*
 *									*
 * where mx and my are scale factors along x and y, respectively.  	*
 *									*
 * dv_dvdx ( iret )							*
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
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				using int grds for scl fctr derivatives *
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		i, kxd, kyd, ksub1, ksub2, ier, nval, zero=0;
	int		nu, nv, numu, numv;
	float		*grnu, *grnv, *grnumu, *grnumv;
        int             nuddx, nvddx, ixmsdy;
        float           *gruddx, *grvddx, *grxmsdy;

/*------------------------------------------------------------------------*/
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
         *	Compute x derivatives of u and v components.
         */
	dg_puts ( &numu, iret );
	if ( *iret != 0 ) return;
	df_ddx ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &nuddx, iret );
	if ( *iret != 0 ) return;
	dg_puts ( &numv, iret );
	if ( *iret != 0 ) return;
	df_ddx ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &nvddx, iret );
	if ( *iret != 0 ) return;

        /*
         *	Compute map scale factor derivative coefficients.
         */
	dg_dmsf ( iret );
	if ( *iret != 0 ) return;

        nval = 1;
        dg_iget ( "IXMSDY", &nval, &ixmsdy, iret);
        dg_getg ( &ixmsdy, &grxmsdy, &kxd, &kyd, &ksub1, &ksub2, iret );

        /*
         *	Compute the output vector components.
         */
        dg_getg ( &nu, &grnu, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nv, &grnv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu, &grnumu, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grnumv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nuddx, &gruddx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nvddx, &grvddx, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if ( ERMISS ( grvddx[i] )  || ERMISS ( gruddx[i] )  ||
     		 ERMISS ( grnumu[i] )  || ERMISS ( grnumv[i] ) ) {
	       grnu[i] = RMISSD;
	       grnv[i] = RMISSD;
            }
	    else {
	       grnu[i] = gruddx[i] - grnumv[i] * grxmsdy[i];
	       grnv[i] = grvddx[i] + grnumu[i] * grxmsdy[i];
	    }
	}

        /*
         *	Make a name of the form 'DVDX'//u and update header;
         *	update stack.
         */
	dg_updv  ( "DVDX", &nu, &nv, &numu, &zero, iret );
	dg_putv  ( &nu, &nv, iret );
	dg_esub  ( &nu, &nv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
