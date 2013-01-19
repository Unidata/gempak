#include "df.h"

void df_thes ( int *iret )
/************************************************************************
 * df_thes								*
 *									*
 * This subroutine computes the saturated equivalent potential  	*
 * temperature in Kelvin from the pressure and temperature:		*
 *									*
 *     THES (PRES, TMPC) = PD_THTE (PRES, TMPC, TMPC)   		*
 *									*
 * df_thes ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * D. McCann/NSSFC	11/94						*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, num, kxd, kyd, kxyd, ksub1, ksub2, ier, zero;
    float *gnum1, *gnum2, *gnum;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the two grids from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num2, iret );
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
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Compute THES.
     */
    kxyd = kxd * kyd;
    pd_thte ( gnum1, gnum2, gnum2, &kxyd, gnum, &ier );

    /*
     * Make a name of the form 'THES'//S1//S2 and update header.
     */
    dg_updh ( "THES", &num, &num1, &num2, iret );

    /*
     * Update stack.
     */
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
