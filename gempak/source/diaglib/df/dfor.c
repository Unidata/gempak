#include "df.h"

void df_or ( const int *nargs, int *iret )
/************************************************************************
 * df_or								*
 *									*
 * This subroutine evaluates the OR logical function for all of nargs	*
 * input arguments.  If at least one argument is greater than 0 at	*
 * grid point, DF_OR returns a 1; otherwise, it returns a 0 at that	*
 * grid point.  If any one of the input arguments is missing at		*
 * a point, DF_OR returns a missing value.				*
 *									*
 * df_or (nargs, iret)							*
 *									*
 * Input parameters:							*
 *	*nargs		const int	Number of input parameters	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 - normal return		*
 **									*
 * Log:									*
 * m.gamazaychikov/SAIC	09/05						*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num, num1, kxd, kyd, ksub1, ksub2, i, im1, iarg, zero, ier;
    float *gnum, *gnum1, dg1, dgo;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the output grid and initialize it to zero.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );
    for ( i = ksub1; i <= ksub2; i++ ) {
        im1 = i - 1;
	gnum[im1] = 0.;
    }

    /*
     * Loop over all NARGS input arguments.
     */
    for ( iarg = 1; iarg <= *nargs; iarg++ ) {
	dg_gets ( &num1, iret );
	if ( *iret != 0 ) return;
	dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1; i <= ksub2; i++ ) {
	    im1 = i - 1;
	    dg1 = gnum1[im1];
	    dgo = gnum[im1]; 
	    if ( ERMISS ( dg1 ) || ERMISS ( dgo ) ) {
		gnum[im1] = RMISSD;
	    } else if ( dg1 > 0.0 ) {
		gnum[im1] += 1;
	    }
	}
    }

    /*
     * Do one more loop from ksub1 to ksub2 to set the output grid
     * to 1 wherever NINT of its value is equal to NARGS.    
     */
    for ( i = ksub1; i <= ksub2; i++ ) {
	im1 = i - 1;
	dgo = gnum[im1];
	if ( ! ERMISS ( dgo ) ) {
	    if ( G_NINT ( dgo ) >= 1 ) {
		gnum[im1] = 1.0;
	    } else {
		gnum[im1] = 0.0;
	    }
	}
    }

    /*
     * Get a name of the form 'OR'//'last arg' and update header;
     * update stack.
     */
    dg_updh ( "OR", &num, &num1, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
