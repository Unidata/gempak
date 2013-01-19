#include "df.h"

void df_bool ( int *iret )
/************************************************************************
 * df_bool								*
 *									*
 * This subroutine checks a scalar grid and returns 0 if the grid point *
 * is missing and 1 if the grid point has data.				*
 *									*
 * df_bool ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/98	Copied from DF_MUL			*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		10/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num, kxd, kyd, ksub1, ksub2, ier, fidx, cidx, zero;
    float *gnum1, *gnum, dg1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the grid from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &num,  &gnum,  &kxd, &kyd, &ksub1, &ksub2, &ier );

    /*
     * Process the grids.
     */
    for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
	cidx = fidx - 1;
        dg1 = gnum1[cidx];
        if ( ERMISS (dg1) ) {
	    gnum[cidx] = 0.0;
	} else {
	    gnum[cidx] = 1.0;
	}
    }

    /*
     * Make a name of the form 'BOOL'//S1 and update header;
     * update stack.
     */
    dg_updh ( "BOOL", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
