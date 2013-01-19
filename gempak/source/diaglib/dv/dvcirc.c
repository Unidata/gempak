#include "dv.h"

void dv_circ  ( int *iret )
/************************************************************************
 * dv_circ								*
 *									*
 * This subroutine forms the two components of a vertical circulation	*
 * and returns them as a vector.  The u component is the tangential	*
 * component of V and the v component is the vertical motion W.		*
 *									*
 *     CIRC ( V, W ) = [ TANG (V), W ]					*
 *									*
 * CIRC generates a vector grid.					*
 *									*
 * dv_circ  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV, DG_GETS, DV_TANG*
 **									*
 * Log:									*
 * K. Brill/NMC 	 9/90						*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		ntan, nvrt, ier, zero=0;
/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the tangential component of the vector grid at the top of
         *      the stack.
         */
	dv_tang ( iret );
	if ( *iret != 0 ) return;

        /*
         *	Pull the scalar tangential component off of the stack.
         */
	dg_gets  ( &ntan, iret );
	if  ( *iret != 0 )  return;

        /*
         *	The next grid on the stack is the vertical motion.
         */
	dg_gets  ( &nvrt, iret );
	if  ( *iret != 0 )  return;

        /*
         *      Make a name of the form 'CIRC'//TANG(V)//W and update both grid
         *	headers; update the stack.
         */
	dg_updv  ("CIRC", &ntan, &nvrt, &ntan, &nvrt, iret );
	dg_putv  ( &ntan, &nvrt, iret );
	dg_esub  ( &ntan, &nvrt, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
