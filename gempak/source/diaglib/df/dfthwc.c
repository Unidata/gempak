#include "df.h"

void df_thwc ( int *iret )
/************************************************************************
 * df_thwc								*
 *									*
 * This subroutine computes the wet bulb potential temperature in	*
 * Celsius from the pressure, temperature and dewpoint:			*
 *									*
 *     THWC (PRES, TMPC, DWPC) = PD_THWC (PRES, TMPC, DWPC)             *
 *									*
 * df_thwc ( iret )						        *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * P. Bothwell/SPC	 9/97	Orig THTE changed to calculate THTW     *
 * T. Lee/GSC		11/97	Cleaned up; renamed THTW to THWC	*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, num3, num, kxd, kyd, kxyd, ksub1, ksub2, ier, zero;
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
     * Get a new grid number and compute THWC.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num2, &gnum2, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num3, &gnum3, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, iret );

    kxyd = kxd * kyd;
    pd_thwc ( gnum1, gnum2, gnum3, &kxyd, gnum, &ier );

    /*
     * Make a name of the form 'THWC'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "THWC", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
