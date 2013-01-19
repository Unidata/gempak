#include "dv.h"

void dv_smul  ( int *iret )
/************************************************************************
 * dv_smul								*
 *									*
 * This subroutine multiplies a scalar with each component of a vector:	*
 *									*
 *     SMUL ( S, V ) = [ S*u, S*v ]					*
 *									*
 * SMUL generates a vector grid.					*
 *									*
 * dv_smul  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret 		int		Returh code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * G. Huffman/GSC	9/88	Adapted from DV_VMUL			*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        const int       zero=0;
	int		num, nvecu, nvecv, noutu, noutv, ier;
/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the scalar and vector from the stack (grid numbers are used
         *	in name generation).
         */
	dg_gets  ( &num, iret );
	if  ( *iret != 0 )  return;
	dg_getv  ( &nvecu, &nvecv, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put S and the u-component on the stack.
         */
	dg_puts  ( &nvecu, iret );
	if  ( *iret != 0 )  return;
	dg_puts  ( &num, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Multiply and get the grid off the stack.
         */
	df_mul ( iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &noutu, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put S and the v-component on the stack.
         */
	dg_puts  ( &nvecv, iret );
	if  ( *iret != 0 )  return;
	dg_puts  ( &num, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Multiply and get the grid off the stack.
         */
	df_mul  ( iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &noutv, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Make a name of the form 'SMUL'//S//u2 and update both grid
         *	headers; update the stack.
         */
	dg_updv  ("SMUL", &noutu, &noutv, &num, &nvecu, iret );
	dg_putv  ( &noutu, &noutv, iret );
	dg_esub  ( &noutu, &noutv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
