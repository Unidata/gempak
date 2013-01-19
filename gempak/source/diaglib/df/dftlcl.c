#include "df.h"

void df_tlcl ( int *iret )
/************************************************************************
 * df_tlcl								*
 *									*
 * This subroutine computes the temperature of the Lifting Condensation	*
 * Level from the temperature and dewpoint:				*
 *									*
 *     TLCL (TMPC, DWPC) = PD_TLCL (TMPC, DWPC, NPT, TLCL, IRET)	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * S. Chiswell/Unidata	 9/03		Created				*
 * R. Tian/SAIC		11/05		Recoded from Fortran		*
 ************************************************************************/
{
    int num1, num2, num, kxd, kyd, kxyd, ksub1, ksub2, ier, zero;
    float *gnum1, *gnum2, *gnum;
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

    /*
     * Get a new grid number and compute tlcl.
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
    pd_tlcl ( gnum1, gnum2, &kxyd, gnum, &ier );

    /*
     * Make a name of the form 'TLCL'//S1//S2 and update header;
     * update stack.
     */
    dg_updh ( "TLCL", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
