#include "dv.h"

void dv_vasv ( int *iret )
/************************************************************************
 * dv_vasv								*
 *									*
 * This subroutine computes the vector component of the first vector	*
 * along the second vector.						*
 *									*
 *     VASV ( V1, V2 ) = [ DOT (V1,V2) / MAG (V2) ** 2 ] V2		*
 *									*
 * VASV generates a vector field.					*
 *									*
 * dv_vasv  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * K. Brill/NMC		 1/93 						* 
 * S. Chiswell/Unidata	 2/96	Redefined mag as REAL rmg		*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        const int       zero=0;
        int             i, ier, kxd, kyd, ksub1, ksub2 ;
	int		numu1, numv1, numu2, numv2, nu, nv;
	float		*gru1, *grv1, *gru2, *grv2, *gru, *grv;
        float           du1, dv1, du2, dv2, dot, rmg;

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the two vectors.
         */
	dg_getv  ( &numu1, &numv1, iret );
	if  ( *iret != 0 ) return;
	dg_getv  ( &numu2, &numv2, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Get new grid numbers and compute the along stream vector.
         */
	dg_nxtv ( &nu, &nv, iret );
	if  ( *iret != 0 ) return;

        dg_getg ( &nu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu1, &gru1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv1, &grv1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu2, &gru2, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv2, &grv2, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    du1 = gru1[i];
	    dv1 = grv1[i];
	    du2 = gru2[i];
	    dv2 = grv2[i];
	    if  ( ERMISS (du1) || ERMISS (dv1) ||
     		  ERMISS (du2) || ERMISS (dv2) ) {
		gru[i] = RMISSD;
		grv[i] = RMISSD;
            }
	    else {
		dot = du1 * du2  +  dv1 * dv2;
		rmg = du2 * du2  +  dv2 * dv2;
		if ( rmg < 1.e-20 ) {
		    gru[i] = RMISSD;
		    grv[i] = RMISSD;
		}
		else {
		    gru[i] = ( dot / rmg ) * du2;
		    grv[i] = ( dot / rmg ) * dv2;
		}
            }
	}

        /*
         *	Make a name of the form 'VASV'//u1//u2 and update header;
         *	update stack.
         */
	dg_updv  ( "VASV", &nu, &nv, &numu1, &numu2, iret );
	dg_putv  ( &nu, &nv, iret );
	dg_esub  ( &nu, &nv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
