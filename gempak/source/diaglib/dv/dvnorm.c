#include "dv.h"

void dv_norm ( int *iret )
/************************************************************************
 * dv_norm								*
 *									*
 * This subroutine computes the scalar component of a vector field (V)	*
 * normal to the orientation vector whose direction is specified in	*
 * COMMON / DGOVEC /.							*
 *									*
 *     NORM = ( -k cross V ) dot ( normalized orientation vector )	*
 *									*
 * NORM generates a scalar field.					*
 *									*
 * dv_norm  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV or DG_MSCL	*
 *					-28 = no orientation vector	*
 **									*
 * Log:									*
 * K. F. Brill/GSC	 7/89    					*
 * K. Brill/GSC		 8/89	Subsetting				*
 * K. Brill/GSC	        10/89	Subsetting				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids for scl fctrs	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		i, ier, nval, kxd, kyd, ksub1, ksub2, zero=0;
	int		num, numu1, numv1, ixmscl, iymscl;
        float           *grnum, *gru1, *grv1, *grxms, *gryms;

	float 		orntv[2], ornang, du1, dv1, rnm, du2, dv2;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

	dg_nxts ( &num, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Compute the unit tangent vector components.
         */
        dg_getg( &num, &grnum, &kxd, &kyd, &ksub1, &ksub2, iret );

        nval = 1;
        dg_fget( "ORNANG", &nval, &ornang, iret );

	if ( ERMISS ( ornang ) ) {
	  for ( i = ksub1 - 1; i < ksub2; i++ ) {
	      grnum[i] = RMISSD;
	  }
	  *iret = -28;
	  return;
	}
	  
	orntv [ 0 ] = - sin ( ornang );
	orntv [ 1 ] = - cos ( ornang );

        /*
         *	Compute the map scale factors just in case the grid is not
         *	conformal.
         */
	dg_mscl ( iret );
	if ( *iret != 0 ) return;

        /*
         *	Get the vector.
         */
	dg_getv  ( &numu1, &numv1, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Compute the normal component.
         */
        dg_getg( &numu1, &gru1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &numv1, &grv1, &kxd, &kyd, &ksub1, &ksub2, iret );

        nval = 1;
        dg_iget( "IXMSCL", &nval, &ixmscl, iret );
        dg_iget( "IYMSCL", &nval, &iymscl, iret );
        dg_getg( &ixmscl, &grxms, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &iymscl, &gryms, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    du1 = gru1[i];
	    dv1 = grv1[i];
	    if  ( ERMISS (du1) || ERMISS (dv1) )
		grnum[i] = RMISSD;
	    else if ( G_DIFF(grxms[i], gryms[i]) )
		grnum[i] =  - du1 * orntv [ 1 ] + dv1 * orntv [ 0 ]; 
            else {

                /*
                 *	Treat the case when the grid map projection is
                 *  	nonconformal.
                 *
                 *	Scale the grid relative orientation vector and normalize
                 *	it.
                 */
                du2 = orntv [ 0 ] / grxms[i];
	        dv2 = orntv [ 1 ] / gryms[i];
		rnm =  sqrt ( du2 * du2 + dv2 * dv2 );
		grnum[i] = ( -du1 * dv2  + dv1 * du2 ) / rnm;
	    }
	}

        /*
         *	Make a name of the form 'NORM'//u and update header;
         *	update stack.
         */
	dg_updh  ( "NORM", &num, &numu1, &zero, iret );
	dg_puts  ( &num, iret );
	dg_esub  ( &num, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
