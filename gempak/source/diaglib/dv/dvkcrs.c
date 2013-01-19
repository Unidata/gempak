#include "dv.h"

void dv_kcrs ( int *iret )
/************************************************************************
 * dv_kcrs								*
 *									*
 * This subroutine computes the cross product of a vector with the	*
 * unit vertical vector.						*
 *									*
 *     KCRS = [ -v, u ]							*
 *									*
 * KCRS generates a vector field with the same magnitude as the	input	*
 * vector, but directed 90 degrees to the left of the input vector.	*
 *									*
 * dv_kcrs  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV 			*
 **									*
 * Log:									*
 * K. Brill/NMC		10/92						*
 * S. Jacobs/EAI	 2/93	Reworded header comment			*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		i, ier, kxd, kyd, ksub1, ksub2, zero=0;
	int		numu, numv, numu1, numv1;
        float           *gru, *grv, *gru1, *grv1;
        float           du1, dv1;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

	dg_nxtv ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Get the vector.
         */
	dg_getv  ( &numu1, &numv1, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Compute k x V.
         */
        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu1, &gru1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv1, &grv1, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    du1 = gru1[i];
	    dv1 = grv1[i];
	    if  ( ERMISS (du1) || ERMISS (dv1) ) {
		gru[i] = RMISSD;
	        grv[i] = RMISSD;
	    }
	    else {
		gru[i] = - dv1;
		grv[i] =   du1;
	    }
	}

        /*
         *	Make a name of the form 'KCRS'//u,v and update header;
         *	update stack.
         */
	dg_updv  ( "KCRS", &numu, &numv, &numu1, &numv1, iret );
	dg_putv  ( &numu, &numv, iret );
	dg_esub  ( &numu, &numv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
