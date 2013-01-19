#include "dv.h"

void dv_inad ( int *iret )
/************************************************************************
 * dv_inad								*
 *									*
 * This subroutine computes the inertial advective wind from two	*
 * vectors:								*
 *									*
 *  INAD (V1, V2) = [ DOT(V1,GRAD(u2)) + v1*v2*{(mx/my)*[d(my)/dx]} 	*
 *			   	       - u1*v2*{(my/mx)*[d(mx)/dy]},	*
 *									*
 *                    DOT(V1,GRAD(v2)) - v1*u2*{(mx/my)*[d(my)/dx]}	*
 *				       + u1*u2*{(my/mx)*[d(mx)/dy]} ]	*
 *									*
 * where mx and my are scale factors along x and y, respectively.	*
 * The quantities in braces are assumed to exist in common arrays	*
 * YMSDX and XMSDY, respectively.  INAD generates a vector grid.	*
 *									*
 * dv_inad  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * M. desJardins/GSFC	 5/88	Added new stack functions & rewrote	*
 * I. Graffman/RDS	 7/88	Call to DG_UPDH				*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. F. Brill/GSC	 4/89   Map scale factors			*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				using int grds for scl fctr derivatives *
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		i, ier, nval, kxd, kyd, ksub1, ksub2, zero=0;
	int		num1u, num1v, num2u, num2v, niadu, niadv;
	float		*gr1u, *gr1v, *gr2u, *gr2v, *griadu, *griadv;
	int		ixmsdy, iymsdx;
	float		*grxmsdy, *grymsdx;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the grid numbers for the two input vectors.
         */
	dg_getv  ( &num1u, &num1v, iret );
	if  ( *iret != 0 )  return;
	dg_getv  ( &num2u, &num2v, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the u-component of the second wind on the stack and compute
         *	the gradient.  The result is left on the stack.
         */
	dg_puts  ( &num2u, iret );
	if  ( *iret != 0 )  return;
	dv_grad  ( iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the first vector on the stack and compute the dot product.
         *	Get the result, part of the u-component of the inertial 
         *      advective wind.
         */
	dg_putv  ( &num1u, &num1v, iret );
	if  ( *iret != 0 )  return;
	dv_dot  ( iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &niadu, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the v-component of 2nd wind on stack and compute gradient;
         *	put the first vector on the stack and compute the dot product.
         *	Get the result, part of the v-component of the inertial 
         *      advective wind.
         */
	dg_puts  ( &num2v, iret );
	if  ( *iret != 0 )  return;
	dv_grad  ( iret );
	if  ( *iret != 0 )  return;

	dg_putv  ( &num1u, &num1v, iret );
	if  ( *iret != 0 )  return;
	dv_dot  ( iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &niadv, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute map scale factor derivative coefficients.
         */
	dg_dmsf ( iret );
	if ( *iret != 0 ) return;

        nval = 1;
        dg_iget ( "IXMSDY", &nval, &ixmsdy, iret );
        dg_iget ( "IYMSDX", &nval, &iymsdx, iret );
        dg_getg ( &ixmsdy, &grxmsdy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &iymsdx, &grymsdx, &kxd, &kyd, &ksub1, &ksub2, iret );

        /*
         *	To each component add the map scale factor terms.
         */
        dg_getg ( &niadu, &griadu, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &niadv, &griadv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &num1u, &gr1u, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &num1v, &gr1v, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &num2u, &gr2u, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &num2v, &gr2v, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++) {
	   if ( ERMISS ( griadu[i] ) || ERMISS ( griadv[i] ) || 
     		ERMISS ( gr1u[i] ) || ERMISS ( gr1v[i] ) || 
     		ERMISS ( gr2u[i] ) || ERMISS ( gr2v[i] ) ) {
	      griadu[i] = RMISSD;
	      griadv[i] = RMISSD;
           }
	   else {
	      griadu[i] = griadu[i] + gr2v[i] *  
                         ( gr1v[i] * grymsdx[i] - gr1u[i] * grxmsdy[i] );

	      griadv[i] = griadv[i] + gr2u[i] *
                         ( gr1u[i] * grxmsdy[i] - gr1v[i] * grymsdx[i] );
           }
	}

        /*
         *	Make a name of the form 'INAD'//u1//u2 and update header;
         *	update the stack.
         */
	dg_updv  ( "INAD", &niadu, &niadv, &num1u, &num2u, iret );
	dg_putv  ( &niadu, &niadv, iret );
	dg_esub  ( &niadu, &niadv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
