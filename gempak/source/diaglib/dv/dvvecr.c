#include "dv.h"

void dv_vecr  ( int *iret )
/************************************************************************
 * dv_vecr								*
 *									*
 * This subroutine creates a vector grid from two scalar grids:		*
 *									*
 *     VECR ( S1, S2 ) = [ S1, S2 ]					*
 *									*
 * The grid components must be grid relative.				*
 *									*
 * dv_vecr  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS, plus		*
 *					-11 = ... must be a vector	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 4/89	From DV_VEC				*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        const int       zero=0;
	int		numu, numv, ier;
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
         *	Return them as a vector.  Make a name of the form 'VEC'//S1//S2
         *	and update the headers; update the stack.
         */
	dg_updv  ( "VEC", &numu, &numv, &numu, &numv, iret );
	dg_putv  ( &numu, &numv, iret );
	dg_esub  ( &numu, &numv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
