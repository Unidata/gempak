#include "dv.h"

void dv_rot  ( int *iret )
/************************************************************************
 * dv_rot								*
 *									*
 * This subroutine rotates each vector in a grid by an angle which is	*
 * specified in degrees (a clockwise rotation is positive):		*
 *									*
 *     ROT ( angle, V ) = [ u * COS (angle) + v * SIN (angle),		*
 *                         -u * SIN (angle) + v * COS (angle) ]		*
 *									*
 * ROT generates a vector grid.						*
 *									*
 * dv_rot  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * M. desJardins/GSFC	5/88	Added new stack functions & rewrote	*
 * M. desJardins/GSFC	8/88	Fixed problems				*
 * G. Huffman/GSC	9/88	Error messages				*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             ier, zero=0;
	int		nangl, ndtr, nanglr, ncos, nsin;
        int             numu, numv, nucos, nvcos, nurot, nvrot;
        float           dtr;

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the scalar operand which is the angle.
         */
	dg_gets  ( &nangl, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Convert the angle from degrees to radians by multiplying by DTR.
         */
	dg_nxts  ( &ndtr, iret );
	if  ( *iret != 0 )  return;
        dtr = DTR;
	dg_real  ( &dtr, &ndtr, iret );
	if  ( *iret == 0 )  dg_puts  ( &ndtr, iret );

	if  ( *iret == 0 )  dg_puts  ( &nangl, iret );
	if  ( *iret == 0 )  df_mul   ( iret );
	if  ( *iret == 0 )  dg_gets  ( &nanglr, iret );

        /*
         *	Put the rotation angle on the stack and compute the cosine.
         */
	if  ( *iret == 0 )  dg_puts  ( &nanglr, iret );
	if  ( *iret == 0 )  df_cos   ( iret );
	if  ( *iret == 0 )  dg_gets  ( &ncos, iret );

        /*
         *	Put the rotation angle back on the stack and compute the sine.
         */
	if  ( *iret == 0 )  dg_puts  ( &nanglr, iret );
	if  ( *iret == 0 )  df_sin  ( iret );
	if  ( *iret == 0 )  dg_gets  ( &nsin, iret );

        /*
         *	Get the vector which is to be rotated.
         */
	if  ( *iret == 0 )  dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the u-component and the cosine on stack and multiply.
         */
	dg_puts  ( &numu, iret );
	if  ( *iret == 0 )  dg_puts  ( &ncos, iret );
	if  ( *iret == 0 )  df_mul   ( iret );
	if  ( *iret == 0 )  dg_gets  ( &nucos, iret );

        /*
         *	Put the v component and the sine on the stack and multiply.
         */
	if  ( *iret == 0 )  dg_puts  ( &numv, iret );
	if  ( *iret == 0 )  dg_puts  ( &nsin, iret );
	if  ( *iret == 0 )  df_mul   ( iret );

        /*
         *	Put u*cos back on stack and multiply to compute rotated u-comp.
         */
	if  ( *iret == 0 )  dg_puts  ( &nucos, iret );
	if  ( *iret == 0 )  df_add   ( iret );
	if  ( *iret == 0 )  dg_gets  ( &nurot, iret );

        /*
         *	Put the v-component and cos on stack and multiply.
         */
	if  ( *iret == 0 )  dg_puts  ( &numv, iret );
	if  ( *iret == 0 )  dg_puts  ( &ncos, iret );
	if  ( *iret == 0 )  df_mul   ( iret );
	if  ( *iret == 0 )  dg_gets  ( &nvcos, iret );

        /*
         *	Put the u-component and sine on stack and multiply.  Leave
         *	on stack so that it can be subtracted.
         */
	if  ( *iret == 0 )  dg_puts  ( &numu, iret );
	if  ( *iret == 0 )  dg_puts  ( &nsin, iret );
	if  ( *iret == 0 )  df_mul   ( iret );

        /*
         *	Put v*cos back on stack.  This will become first term in
         *	subtraction which will compute the v-component of the rotated
         *	vector.
         */
	if  ( *iret == 0 )  dg_puts  ( &nvcos, iret );
	if  ( *iret == 0 )  df_sub   ( iret );
	if  ( *iret == 0 )  dg_gets  ( &nvrot, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Make a name of the form 'ROT//angle//u' and update header;
         *	update the stack.
         */
	dg_updv  ( "ROT", &nurot, &nvrot, &nangl, &numu, iret );
	dg_putv  ( &nurot, &nvrot, iret );
	dg_esub  ( &nurot, &nvrot, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
