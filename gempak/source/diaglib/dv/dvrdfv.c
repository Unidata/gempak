#include "dv.h"

void dv_rdfv ( int *iret )
/************************************************************************
 * dv_rdfv								*
 *									*
 * This subroutine smoothes a vector field using a moving average	*
 * low-pass filter with normally distributed (Gaussian) weights.  For	*
 * more information, see the documentation for DF_RDFS.			*
 *									*
 *	RDFV ( V, dx )							*
 *									*
 * dv_rdfv  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * K. Brill/NMC	         5/12   Created form dv_gwfv			*
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
         *	Get the required effective resolution (km).
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
	df_rdfs ( iret );
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
	df_rdfs ( iret );
	if  ( *iret != 0 )  return;
	dg_gets ( &nvo, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Make a name of the form 'RDF'//u1//u2 and update header;
         *	update the stack.
         */
	dg_updv  ( "RDF", &nuo, &nvo, &nui, &nvi, iret );
	dg_putv  ( &nuo, &nvo, iret );
	dg_esub  ( &nuo, &nvo, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
