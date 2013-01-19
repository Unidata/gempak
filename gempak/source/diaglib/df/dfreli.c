#include "df.h"

void df_reli ( int *iret )
/************************************************************************
 * df_reli								*
 *									*
 * This subroutine computes the relative humidity with respect to ice	*
 * from the temperature	and dewpoint temperature (temperature must be 	*
 * less than 0.01C, otherwise RMISSD is assigned).			*
 *									*
 *     RELI  ( TEMP, DWPT ) = PD_RELI ( TEMP, DWPT )			*
 *									*
 * df_reli ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * S. Chiswell/Unidata		1/07					*
 ************************************************************************/
{
    int num1, num2, num, kxd, kyd, kxyd, ksub1, ksub2, zero, ier;
    float *gnum1, *gnum2, *gnum;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the two input grid numbers.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num2, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number and compute the relative huminity.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num2, &gnum2, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    kxyd = kxd * kyd;
    pd_reli ( gnum1, gnum2, &kxyd, gnum, &ier );

    /*    
     * Make a name of the form 'RELI'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "RELI", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
