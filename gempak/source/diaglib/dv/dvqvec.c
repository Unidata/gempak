#include "dv.h"

void dv_qvec ( int *iret )
/************************************************************************
 * dv_qvec								*
 *									*
 * This subroutine computes the Q-vector using a scalar (usually THTA)	*
 * and a vector (usually GEO) in the formula:				*
 *									*
 *  QVEC ( S, V ) = [ - ( DOT ( DVDX (V), GRAD (S) ) ),			*
 *  		      - ( DOT ( DVDY (V), GRAD (S) ) ) ]		*
 *									*
 * A scale factor of 10 or 11 is appropriate.				*
 *									*
 * dv_qvec  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * J. Nielsen/TAMU	12/92	GEMPAK51 version			*
 * S. Jacobs/NMC	 3/94	Clean up				*
 * S. Jacobs/NMC         4/94	Removed unused variables		*
 * S. Jacobs/NMC	 5/95	Updated header for documentation	*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB, DG_ESUB; CHK iret & RTRN	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             i, ier, kxd, kyd, ksub1, ksub2, zero=0;
	int		nu, nv, nthta; 
        int             nqx, nqy, nthx, nthy;
	float		*grqx, *grqy, *grthx, *grthy;

        int             nux, nuy, nvx, nvy;
        float           *grux, *gruy, *grvx, *grvy;

        float           dux, duy, dvx, dvy, dthx, dthy;

/*-----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the raw grids.
         */
	dg_gets  ( &nthta, iret );
	if  ( *iret == 0 )  dg_getv  ( &nu, &nv, iret );
	if  ( *iret == 0 )  dg_nxtv  ( &nqx, &nqy, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute the gradient of the scalar.
         */
	dg_puts  ( &nthta, iret );
	if  ( *iret == 0 )  dv_grad  ( iret );
	if  ( *iret == 0 )  dg_getv  ( &nthx, &nthy, iret );

        /*
         *	Compute the gradient of the u-component of the vector.
         */
	if  ( *iret == 0 )  dg_putv  ( &nu, &nv, iret );
	if  ( *iret == 0 )  dv_dvdx  ( iret );
	if  ( *iret == 0 )  dg_getv  ( &nux, &nvx, iret );

        /*
         *	Compute the gradient of the v-component of the vector.
         */	
	if  ( *iret == 0 )  dg_putv  ( &nu, &nv, iret );
	if  ( *iret == 0 )  dv_dvdy  ( iret );
	if  ( *iret == 0 )  dg_getv  ( &nuy, &nvy, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute the Q-vector.
         */
        dg_getg ( &nux, &grux, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nuy, &gruy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nvx, &grvx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nvy, &grvy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nqx, &grqx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nqy, &grqy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nthx, &grthx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nthy, &grthy, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    dthx = grthx[i];
	    dthy = grthy[i];
	    dux  = grux[i];
	    duy  = gruy[i];
	    dvx  = grvx[i];
	    dvy  = grvy[i];
	    if  ( ERMISS ( dthx ) || ERMISS ( dthy ) || 
     		  ERMISS ( dux  ) || ERMISS ( duy  ) ||
     		  ERMISS ( dvx  ) || ERMISS ( dvy  ) )  {
		grqx[i] = RMISSD;
		grqy[i] = RMISSD;
            }
	    else {
		grqx[i] = -( dux * dthx + dvx * dthy );
		grqy[i] = -( duy * dthx + dvy * dthy );
            }
	}

        /*
         *	Load the Q-vector into the stack.
         */
	dg_updv  ( "QVEC", &nqx, &nqy, &nthta, &nu, iret );
	dg_putv  ( &nqx, &nqy, iret );
	dg_esub  ( &nqx, &nqy, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
