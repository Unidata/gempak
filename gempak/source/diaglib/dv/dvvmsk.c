#include "dv.h"

void dv_vmsk  ( int *iret )
/************************************************************************
 * dv_vmsk								*
 *									*
 * This subroutine masks the components of a vector. 			*
 *									*
 * VMSK generates a vector grid.					*
 *									*
 * dv_vmsk  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/98	Copied from DV_VMUL			*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        const int       zero = 0;
	int		ier, num, nvecu, nvecv, noutu, noutv;
/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the scalar and vector from the stack (grid numbers are used
         *	for name generation).
         */
	dg_getv  ( &nvecu, &nvecv, iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &num, iret );
	if  ( *iret != 0 )  return;

        /*
         *      Put S and the u-component on the stack.
         */
        dg_puts  ( &num, iret );
        if  ( *iret != 0 )  return;
        dg_puts  ( &nvecu, iret );
        if  ( *iret != 0 )  return;

        /*
         *      Multiply and get the grid off the stack.
         */
        df_mask ( iret );
        if  ( *iret != 0 )  return;
        dg_gets  ( &noutu, iret );
        if  ( *iret != 0 )  return;

        /*
         *      Put S and the v-component on the stack.
         */
        dg_puts  ( &num, iret );
        if  ( *iret != 0 )  return;
        dg_puts  ( &nvecv, iret );
        if  ( *iret != 0 )  return;

        /*
         *      Multiply and get the grid off the stack.
         */
        df_mask  ( iret );
        if  ( *iret != 0 )  return;
        dg_gets  ( &noutv, iret );
        if  ( *iret != 0 )  return;

        /*
         *      Make a name of the form 'SMSK'//S//u2 and update both grid
         *      headers; update the stack.
         */
        dg_updv  ("SMSK", &noutu, &noutv, &num, &nvecu, iret );
        dg_putv  ( &noutu, &noutv, iret );
	dg_esub  ( &noutu, &noutv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
