#include "df.h"

void df_tmst ( int *iret )
/************************************************************************
 * df_tmst								*
 *									*
 * This subroutine computes the parcel temperature in Kelvin at level	* 
 * PRES on a specified moist adiabat (THTE).				*
 *									*
 *     TMST ( THTE, PRES ) = PD_TMST ( THTE, PRES )			*
 *									*
 * df_tmst ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * W. Skillman/GSFC	 5/88	Added new stack subroutines		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 7/89	Added PA subroutines			*
 * M. desJardins/GSFC	 8/89	PA to PD subroutines			*
 * M. desJardins/GSFC	 2/90	Correct calling sequence to PD_TMST	*
 * K. Tyle/GSC		10/95	Eliminated user-input guess field	*
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
     * Compute TMST.
     */
    kxyd = kxd * kyd;
    pd_tmst ( gnum1, gnum2, &kxyd, gnum, &ier );

    /*
     * Make a name of the form 'TMST'//S1//S2 and update header.
     */
    dg_updh ( "TMST", &num, &num1, &num2, iret );

    /*
     * Update stack.
     */
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
