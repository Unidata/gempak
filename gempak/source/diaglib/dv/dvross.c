#include "dv.h"

void dv_ross ( int *iret )
/************************************************************************
 * dv_ross								*
 *									*
 * This subroutine computes the Rossby number from two winds:		*
 *									*
 *     ROSS ( V1, V2 ) = MAG ( INAD ( V1, V2 ) ) / ( CORL * MAG ( V1 ) )*
 *									*
 * ROSS generates a scalar grid.					*
 *									*
 * dv_ross ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * I. Graffman/RDS	7/88	Call to DG_UPDH				*
 * G. Huffman/GSC	9/88	New stack functions			*
 * G. Huffman/GSC	9/88	Error messages				*
 * K. Brill/NMC        11/90    Pass grid number to DF_CORL		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             iret1, iret2, ier, zero=0;
	int		num1u, num1v, num2u, num2v, numcor, nross;
/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the two vectors.
         */
	dg_getv  ( &num1u, &num1v, iret );
	if  ( *iret != 0 ) return;
	dg_getv  ( &num2u, &num2v, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put V1 on the stack and compute MAG; compute CORL and multiply
         *	(creating the denominator).  Leave the result on the stack.
         */
	dg_putv  ( &num1u, &num1v, iret );
	if  ( *iret != 0 ) return;
	dv_mag  ( iret );
	if  ( *iret != 0 ) return;

	dg_nxts  ( &numcor, &iret1 );
	df_corl  ( &numcor, &iret2 );
        dg_puts  ( &numcor, iret  );
        *iret = *iret + iret1 + iret2;
	if  ( *iret != 0 ) return;
	df_mul  ( iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the winds on the stack (LIFO order), compute the inertial
         *	advective wind, and take its MAG.
         */
	dg_putv  ( &num2u, &num2v, iret );
	if  ( *iret != 0 ) return;
	dg_putv  ( &num1u, &num1v, iret );
	if  ( *iret != 0 ) return;
	dv_inad  ( iret );
	if  ( *iret != 0 ) return;

	dv_mag  (iret);
	if  ( *iret != 0 ) return;
	/*magiad = istack (itop)*/

        /*
         *	Complete the calculation and get the result.
         */
	df_quo  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &nross, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Make a name of the form 'ROSS'//u1//u2 and update header;
         *	update the stack.
         */
	dg_updh  ( "ROSS", &nross, &num1u, &num2u, iret );
	dg_puts  ( &nross, iret );
	dg_esub  ( &nross, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
