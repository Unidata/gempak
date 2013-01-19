#include "dv.h"

void dv_vecn  ( int *iret )
/************************************************************************
 * dv_vecn								*
 *									*
 * This subroutine creates a vector grid from two scalar grids:		*
 *									*
 *     VECN ( S1, S2 ) = [ S1, S2 ]					*
 *									*
 * The grid components must be north relative.				*
 *									*
 * dv_vecn  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS, plus		*
 *					-11 = ... must be a vector	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 4/89	North-relative				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        const int       zero=0;
	int		numu, numv, ier, kxd, kyd, ksub1, ksub2;
	float		*gru, *grv;

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the two scalar grids.
         */
	dg_gets  ( &numu, iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &numv, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute the grid relative components.
         */
        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_grel  ( gru, grv, gru, grv, &ier );

        /*
         *	Return them as a vector.  Make a name of the form 'VEC'//S1//S2
         *	and update the headers; update the stack.
         */
	dg_updv  ( "VEC", &numu, &numv, &numu, &numv, iret );
	dg_putv  ( &numu, &numv, iret );
	dg_esub  ( &numu, &numv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
