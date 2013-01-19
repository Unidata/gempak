#include "dv.h"

void dv_adv ( int *iret )
/************************************************************************
 * DV_ADV								*
 *									*
 * This subroutine computes the advection of a scalar:			*
 *									*
 *     ADV ( S, V ) = - ( u * DDX (S) + v * DDY (S) )			*
 *									*
 * ADV generates a scalar grid.						*
 *									*
 * dv_adv  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * G. Huffman/GSC	 9/88	New stack functions			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC          8/89   Subsetting				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * T. Lee/GSC		 5/96	Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translated from Fortran                 *
 ************************************************************************/
{
        const int       zero = 0;
        int             num1, numu, numv, ier;
        int             num, kxd, kyd, ksub1, ksub2, i;
	int		level1, level2, ivcord;
	char	        gfunc[13], parm[13], time1[21], time2[21];
        float           *gnum;

/*----------------------------------------------------------------------*/
	*iret = 0;

	dg_ssub ( iret );

        /*
         *	Get the scalar and vector grids (grid numbers used for name
         *	generation).
         */
	dg_gets  ( &num1, iret );
	if  ( *iret != 0 ) return;
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the scalar on the stack, compute the gradient, and
         *	leave the answer for the dot product (below).
         */
	dg_puts  ( &num1, iret );
	if  ( *iret != 0 ) return;
	dv_grad  ( iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put the vector field on the stack, compute the dot product,
         *	and read the output grid numbers.
         */
	dg_putv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;
	dv_dot  ( iret );
	if  ( *iret != 0 ) return;
	dg_tops ( gfunc, &num, time1, time2, &level1, &level2, 
                          &ivcord, parm, iret );
	if  ( *iret != 0 ) return;

        /*
         *	The advection is the negative of the dot product of the vector
         *	and the gradient of the scalar.
         */
        dg_getg( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++) {
	    if ( ! ERMISS ( gnum[i] ) ) gnum[i] = -gnum[i];
	}

        /*
         *	Make a name of the form 'ADV'//S//u and update header;
         *	the stack is current.
         */
	dg_updh  ( "ADV", &num, &num1, &numu, iret );
	dg_esub  ( &num, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
