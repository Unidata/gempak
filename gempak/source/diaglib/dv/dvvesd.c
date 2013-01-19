#include "dv.h"

void dv_vesd  ( int *iret )
/************************************************************************
 * dv_vesd								*
 *									*
 * This subroutine creates a vector grid from two scalar grids:		*
 *									*
 *     VESD ( SPD, DIR ) = [ UREL, VREL ]				*
 *									*
 * The direction is assumed to be conventional north relative.		*
 *									*
 * dv_vesd  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS, plus		*
 *					-11 = ... must be a vector	*
 **									*
 * Log:									*
 * K. Brill/HPC      	12/10	Created from VECN			*
 ************************************************************************/
{
        const int       zero=0;
	int		numu, numv, ier, kxd, kyd, kxyd, ksub1, ksub2;
	float		*gru, *grv;

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the two scalar grids (speed & direction).
         */
	dg_gets  ( &numu, iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &numv, iret );
	if  ( *iret != 0 )  return;

        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
	/*
	 *      Convert speed and direction to north relative components.
	 */
        kxyd = kxd * kyd;
        pd_sduv  ( gru, grv, &kxyd, gru, grv, iret );
        /*
         *	Compute the grid relative components.
         */
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
