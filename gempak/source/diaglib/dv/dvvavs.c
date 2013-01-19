#include "dv.h"

void dv_vavs  ( int *iret )
/************************************************************************
 * dv_vavs								*
 *									*
 * This subroutine computes the average vector for a vector field but	*
 * only over the subset area.  VAVS generates a vector with the		*
 * average at each grid point in the subset area.			*
 *									*
 * dv_vavs  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * K. Brill/NMC		10/90						*
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
         *	Put the u-component of the wind on the stack and average.
         */
	dg_puts  ( &nui, iret );
	if  ( *iret != 0 )  return;
	df_savs ( iret );
	if  ( *iret != 0 )  return;
	dg_gets ( &nuo, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the v-component of the wind on the stack and average.
         */
	dg_puts  ( &nvi, iret );
	if  ( *iret != 0 )  return;
	df_savs ( iret );
	if  ( *iret != 0 )  return;
	dg_gets ( &nvo, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Make a name of the form 'AVS'//u1//u2 and update header;
         *	update the stack.
         */
	dg_updv  ( "AVS", &nuo, &nvo, &nui, &nvi, iret );
	dg_putv  ( &nuo, &nvo, iret );
	dg_esub  ( &nuo, &nvo, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
