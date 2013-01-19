#include "dv.h"

void dv_vquo  ( int *iret )
/************************************************************************
 * dv_vquo								*
 *									*
 * This subroutine divides the components of two vectors:		*
 *									*
 *     VQUO ( V1, V2 ) = [ u1/u2, v1/v2 ]				*
 *									*
 * VQUO generates a vector grid.					*
 *									*
 * dv_vquo  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	10/85						*
 * M. desJardins/GSFC	5/88	Added new stack functions		*
 * G. Huffman/GSC	9/88	Error messages				*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	const int	zero = 0;
	int		ier;
	int		nvec1u, nvec1v, nvec2u, nvec2v, nvec3u, nvec3v;
/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the two vectors from the stack (grid numbers are used for
         *	name generation).
         */
	dg_getv  ( &nvec1u, &nvec1v, iret );
	if  ( *iret != 0 )  return;
	dg_getv  ( &nvec2u, &nvec2v, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the u-component of vector 2 and vector 1 on the stack.
         */
	dg_puts  ( &nvec2u, iret );
	if  ( *iret != 0 )  return;
	dg_puts  ( &nvec1u, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Divide the u-components and get the grid number.
         */
	df_quo  ( iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &nvec3u, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the v-component of vector 2 and vector 1 on the stack.
         */
	dg_puts  ( &nvec2v, iret );
	if  ( *iret != 0 )  return;
	dg_puts  ( &nvec1v, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Divide the v-components and get the grid number.
         */
	df_quo  ( iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &nvec3v, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Make a name of the form 'VQUO'//u1//u2 and update both grid
         *	headers; update the stack.
         */
	dg_updv  ( "VQUO", &nvec3u, &nvec3v, &nvec1u, &nvec2u, iret );
	dg_putv  ( &nvec3u, &nvec3v, iret );
	dg_esub  ( &nvec3u, &nvec3v, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
