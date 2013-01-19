#include "df.h"

void df_beta ( const int *num, int *iret )
/************************************************************************
 * df_beta								*
 *									*
 * This subroutine computes the Coriolis acceleration at each grid	*
 * point.  The following equation is used:				*
 *									*
 *     BETA = d (CORL) / dy						*
 * 									*
 * This computation has no operand.					*
 *									*
 * df_beta ( num, iret )						*
 *									*
 * Input parameter:							*
 *	*num      	const int	Grid number			*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-10 = Internal grid list is full*
 *					-12 = ... must be a scalar	*
 *					-16 = Map proj. ... is invalid	*
 *					-20 = Stack is full		*
 **									*
 * Log:									*
 * D. McCann/AWC	 4/01						*
 * K. Brill/HPC		 1/02	CALL DG_SSUB, DG_ESUB; RTRN after NXTS	*
 * R. Tian/SAIC		10/05	Recoded from Fortran			* 
 ************************************************************************/
{
    int ncorl, zero, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Check if navigation parameters have been computed.
     */
    dg_ltln ( iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number for CORL.
     */
    dg_nxts ( &ncorl, iret );
    if ( *iret != 0 ) return;

    /*
     * Compute beta..
     */
    df_corl ( &ncorl, iret );
    dg_puts ( &ncorl, iret );
    df_ddy ( iret );
    dg_gets ( (int *)num, iret );
    dg_esub ( (int *)num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
