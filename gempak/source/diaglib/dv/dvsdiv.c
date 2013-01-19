#include "dv.h"

void dv_sdiv ( int *iret )
/************************************************************************
 * dv_sdiv								*
 *									*
 * This subroutine computes the flux divergence of a scalar:		*
 *									*
 *     SDIV ( S, V ) = S * DIV ( V ) + DOT ( V, GRAD ( S ) )		*
 *									*
 * This form of the equation is preferred over DIV ( [ S*u, S*v ] )	*
 * because the DIV form has up to twice the error for centered		*
 * differencing:							*
 *									*
 * If:   S = a * SIN ( 2 * PI * x / ( m * DELX ) )			*
 *       u = b * SIN ( 2 * PI * x / ( n * DELX ) )			*
 *       v = 0								*
 * And:  Expand values at (i-1), (i+1) in a 3d order Taylor series	*
 * Then: Err ( DIV ... ) ~ ( SINm * COSn * ( 1 / (n*n*n) + 1 / (m*m*n) )*
 *                       + ( COSm * SINn * ( 1 / (m*m*m) + 1 / (m*n*n) )*
 *       Err ( SDIV... ) ~ ( SINm * COSn * ( 1 / (n*n*n) )		*
 *                       + ( COSm * SINn * ( 1 / (m*m*m) )		*
 *									*
 * SDIV generates a scalar field.					*
 *									*
 * dv_sdiv  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * G. Huffman/GSC	9/88	New stack functions			*
 * G. Huffman/GSC	9/88	Error messages				*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	char     	gdum[13], pdum[13], time1[21], time2[21];
	int		level1, level2, ivcord, ier, zero=0;

	int		ns, nu, nv, ndiv;
/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the scalar and vector.
         */
	dg_gets  ( &ns, iret );
	if  ( *iret != 0 ) return;
	dg_getv  ( &nu, &nv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the scalar on the stack and compute GRAD; put the vector
         *	on the stack and take the dot product.  Leave the result.
         */
	dg_puts  ( &ns, iret );
	if  ( *iret == 0 )  dv_grad  ( iret );
	if  ( *iret == 0 )  dg_putv  ( &nu, &nv, iret );
	if  ( *iret == 0 )  dv_dot  ( iret );

        /*
         *	Put the vector on the stack and compute DIV; put the scalar
         *	on the stack and multiply.  Leave the result.
         */
	if  ( *iret == 0 )  dg_putv  ( &nu, &nv, iret );
	if  ( *iret == 0 )  dv_div  ( iret );
	if  ( *iret == 0 )  dg_puts  ( &ns, iret );
	if  ( *iret == 0 )  df_mul  ( iret );

        /*
         *	Add the two parts and read the grid number of the result.
         */
	if  ( *iret == 0 )  df_add  ( iret );
	if  ( *iret == 0 )  dg_tops  ( gdum, &ndiv, time1, time2, 
                                              &level1, &level2, &ivcord, 
                                              pdum, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Make a name of the form 'SDIV'//S//u and update the header.
         *	The stack is current.
         */
	dg_updh  ( "SDIV", &ndiv, &ns, &nu, iret );
	dg_esub  ( &ndiv, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
