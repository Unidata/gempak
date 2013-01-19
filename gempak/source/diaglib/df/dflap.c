#include "df.h"

void df_lap ( int *iret )
/************************************************************************
 * df_lap								*
 *									*
 * This subroutine computes LAP (S), the Laplacian of a scalar grid:	*
 *									*
 *     LAP (S) = DIV ( GRAD (S) )					*
 *									*
 * The calculation is done by computing the divergence of the		*
 * gradient of S.  Map scale factors are included implicitly.		*
 *									*
 *									*
 * df_lap ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * M. desJardins/GSFC	 7/88	Added new stack functions		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. F. Brill/GSC       4/89   Recoded for map scale factors		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, nlap, zero, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_puts ( &num1, iret );
    dv_grad ( iret );
    if ( *iret != 0 ) return;
    dv_div ( iret );
    if ( *iret != 0 ) return;
    dg_gets ( &nlap, iret );
    if ( *iret != 0 ) return;

    /*
     * Make a name of the form 'LAP'//S and update header;
     * update stack.
     */
    dg_updh ( "LAP", &nlap, &num1, &zero, iret );
    dg_puts ( &nlap, iret );
    dg_esub ( &nlap, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
