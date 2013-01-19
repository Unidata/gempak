#include "df.h"

void df_sgmx ( int *iret )
/************************************************************************
 * df_sgmx								*
 *									*
 * This subroutine finds the maximum value on the internal grid		*
 * associated with its argument and returns a grid with this maximum 	*
 * assigned at every grid point.					* 
 *									*
 *     SGMX (S) 							*
 *									*
 * df_sgmx ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * T. Lee/SAIC		10/05	Created					*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num, nval, kxd, kyd, ksub1, ksub2, kgxmin, kgymin, kgxmax, kgymax,
        i, ier, zero;
    float *gnum, dg2, rmin, rmax, ravg, rdev;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get scalar grid from the stack.
     */
    dg_gets ( &num, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Use GR_STAT to find the maximum.
     */
    nval = 1;
    dg_iget ( "KGXMIN", &nval, &kgxmin, iret );
    dg_iget ( "KGYMIN", &nval, &kgymin, iret );
    dg_iget ( "KGXMAX", &nval, &kgxmax, iret );
    dg_iget ( "KGYMAX", &nval, &kgymax, iret );

    grc_stat ( gnum, &kxd, &kyd, &kgxmin, &kgymin, &kgxmax, &kgymax, 
     	      &rmin, &rmax, &ravg, &rdev, &ier );

    /*
     * Assign the maximum value found to each point on the output grid.
     */
    for ( i = ksub1; i <= ksub2; i++ ) {
	dg2 = gnum[i-1];
	if ( ! ERMISS ( dg2 ) ) {
	    gnum[i-1] = rmax;
	}
    }

    /*
     * Get a name of the form 'SGMX'//S and update the header and  stack.
     */
    dg_updh ( "SGMX", &num, &zero, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
