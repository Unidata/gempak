#include "dv.h"

void dv_grad ( int *iret )
/************************************************************************
 * dv_grad								*
 *									*
 * This subroutine computes the gradient of a scalar field:		*
 *									*
 *     GRAD ( S ) = [ DDX ( S ), DDY ( S ) ]				*
 *									*
 * GRAD generates a vector field.					*
 *									*
 * dv_grad  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * I. Graffman/RDS	7/88	Call to DG_UPDH				*
 * G. Huffman/GSC	9/88	New stack routines			*
 * G. Huffman/GSC	9/88	Error messages				*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             ier, zero=0;
	int		num, numu, numv;
/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the scalar grid into grid table.
         */
	dg_gets  ( &num, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the scalar field on the stack, compute DDX, and get the
         *	result.
         */
	dg_puts  ( &num, iret );
	if  ( *iret != 0 ) return;
	df_ddx  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &numu, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the scalar field back on the stack, compute DDY, and
         *	get the result.
         */
	dg_puts  ( &num, iret );
	if  ( *iret != 0 ) return;
	df_ddy  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &numv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Make a name of the form 'GRAD'//S and update the header;
         *	update the stack.
         */
	dg_updv  ( "GRAD", &numu, &numv, &num, &zero, iret );
	dg_putv  ( &numu, &numv, iret );
	dg_esub  ( &numu, &numv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
