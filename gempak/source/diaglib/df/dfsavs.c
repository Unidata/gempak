#include "df.h"

void df_savs ( int *iret )
/************************************************************************
 * df_savs								*
 *									*
 * This subroutine computes the average over all valid points of an	*
 * internal scalar grid and places the average at each grid point.	*
 *									*
 * df_savs ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/NMC		10/90						*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Avg only on JGX/YMIN -> JGX/YMAX; check	*
 *				for zero points averaged		*
 * R. Tian/SAIC		12/02	Try to make loop more clear		*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int jgymin, jgymax, jgxmin, jgxmax, kxd, kyd, ksub1, ksub2;
    int ni, no, npts, ix, iy, ii, zero, ier;
    float *gni, *gno, dsum, averag;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the grid number.
     */
    dg_gets ( &ni, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &ni, &gni, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get a new grid number and do the averaging.
     */
    dg_nxts ( &no, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &no, &gno, &kxd, &kyd, &ksub1, &ksub2, iret );

    dsum = 0.0;
    npts = 0;
    dg_qbnd ( &jgxmin, &jgxmax, &jgymin, &jgymax, iret );
    for ( iy = jgymin; iy <= jgymax; iy++ ) {
	for ( ix = jgxmin; ix <= jgxmax; ix++ ) {
	    ii = ( iy - 1 ) * kxd + ix;
	    if ( ! ERMISS ( gni[ii-1] ) ) {
	        dsum += gni[ii-1];
	        npts += 1;
	    }
	}
    }
    if ( npts <= 0 ) {
	averag = RMISSD;
    } else {
	averag = dsum / npts;
    }

    for ( iy = jgymin; iy <= jgymax; iy++ ) {
	for ( ix = jgxmin; ix <= jgxmax; ix++ ) {
	    ii = ( iy - 1 ) * kxd + ix;
	    if ( ! ERMISS ( gni[ii-1] ) ) {
		gno[ii-1] = averag;
	    } else {
		gno[ii-1] = RMISSD;
	    }
	}
    }

    /*
     * Make a name of the form 'AVS'//S and update header;
     * update stack.
     */
    dg_updh ( "AVS", &no, &ni, &zero, iret );
    dg_puts ( &no, iret );
    dg_esub ( &no, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
