#include "dv.h"

void dv_gwfv ( int *iret )
/************************************************************************
 * dv_gwfv								*
 *									*
 * This subroutine smoothes a vector field using a moving average	*
 * low-pass filter with normally distributed (Gaussian) weights.  For	*
 * more information, see the documentation for DF_GWFS.			*
 *									*
 *	GWFV ( V, N )							*
 *									*
 * dv_gwfv  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * K. Brill/NMC	         2/95    					*
 * K. Brill/NMC		 4/95	Documentation				*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		ier, zero=0;
	int		nui, nvi, nuo, nvo, nwl;
/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the grid numbers for the input vector.
         */
	dg_getv  ( &nui, &nvi, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Get the multiple of wavelength having 1/e response.
         */
	dg_gets ( &nwl, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the u-component of the wind on the stack and smooth.
         */
	dg_puts  ( &nwl, iret );
	if  ( *iret != 0 )  return;
	dg_puts  ( &nui, iret );
	if  ( *iret != 0 )  return;
	df_gwfs ( iret );
	if  ( *iret != 0 )  return;
	dg_gets ( &nuo, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the v-component of the wind on the stack and smooth.
         */
	dg_puts  ( &nwl, iret );
	if  ( *iret != 0 )  return;
	dg_puts  ( &nvi, iret );
	if  ( *iret != 0 )  return;
	df_gwfs ( iret );
	if  ( *iret != 0 )  return;
	dg_gets ( &nvo, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Make a name of the form 'GWF'//u1//u2 and update header;
         *	update the stack.
         */
	dg_updv  ( "GWF", &nuo, &nvo, &nui, &nvi, iret );
	dg_putv  ( &nuo, &nvo, iret );
	dg_esub  ( &nuo, &nvo, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
