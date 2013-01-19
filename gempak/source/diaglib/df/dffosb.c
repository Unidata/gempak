#include "df.h"

void df_fosb ( int *iret )
/************************************************************************
 * df_fosb								*
 *									*
 * This subroutine computes the Fosberb index from temperature,		*
 * relative humidity and wind speed:					* 
 *									*
 *     FOSB (TMPC, RELH, SPED) = PD_FOSB (TMPC, RELH, SPED)		*
 *									*
 * df_fosb ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * T. Lee/SAIC		 6/03	Created					*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, num3, num, kxyd, kxd, kyd, ksub1, ksub2, zero, ier;
    float *gnum1, *gnum2, *gnum3, *gnum;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the three grids from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num2, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num3, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num2, &gnum2, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num3, &gnum3, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    kxyd = kxd * kyd;
    pd_fosb ( gnum1, gnum2, gnum3, &kxyd, gnum, &ier );

    /*
     * Make a name of the form 'FOSB'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "FOSB", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
