#include "dv.h"

void dv_sm5v ( int *iret )
/************************************************************************
 * dv_sm5v								*
 *									*
 * This subroutine smoothes a vector field.				*
 *									*
 * dv_sm5v  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * K. Brill/GSC	        10/89    					*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        const int       zero=0;
	int		nui, nvi, nuo, nvo, ier;
/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the grid numbers for the input vector.
         */
	dg_getv  ( &nui, &nvi, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the u-component of the wind on the stack and smooth.
         */
	dg_puts  ( &nui, iret );
	if  ( *iret != 0 )  return;
	df_sm5s ( iret );
	if  ( *iret != 0 )  return;
	dg_gets ( &nuo, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the v-component of the wind on the stack and smooth.
         */
	dg_puts  ( &nvi, iret );
	if  ( *iret != 0 )  return;
	df_sm5s ( iret );
	if  ( *iret != 0 )  return;
	dg_gets ( &nvo, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Make a name of the form 'SM5'//u1//u2 and update header;
         *	update the stack.
         */
	dg_updv  ( "SM5", &nuo, &nvo, &nui, &nvi, iret );
	dg_putv  ( &nuo, &nvo, iret );
	dg_esub  ( &nuo, &nvo, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
