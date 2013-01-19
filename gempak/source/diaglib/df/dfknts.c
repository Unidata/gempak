#include "df.h"

void df_knts ( int *iret )
/************************************************************************
 * df_knts								*
 *									*
 * This subroutine converts speed in meters/second to knots:		*
 *									*
 *     KNTS (S) = PD_MSKN (S)						*
 *              = S * 1.9438						*
 *									*
 * df_knts ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. Goodman/RDS	11/85						*
 * W. Skillman/GSFC	 5/88	Added new stack subroutines		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 7/89	Added PA subroutines			*
 * M. desJardins/GSFC	 8/89	PA to PD subroutines			*
 * M. desJardins/GSFC	 2/90	Correct call to PD_MSKN			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, kxd, kyd, kxyd, ksub1, ksub2, zero, ier;
    float *gnum1, *gnum;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the grid number.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get a new grid number and convert to knots.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );

    kxyd = kxd * kyd;
    pd_mskn ( gnum1, &kxyd, gnum, &ier );

    /*
     * Make a name of the form 'KNT'//S and update header;
     * update stack.
     */
    dg_updh ( "KNT", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
