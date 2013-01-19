#include "df.h"

void df_sm5s ( int *iret )
/************************************************************************
 * df_sm5s								*
 *									*
 * This subroutine smoothes a scalar grid.				*
 *									*
 * df_sm5s ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/GSC		10/89						*
 * K. Brill/NMC		 5/91	Took out unneeded code			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop;rearranged  *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int ni, no, jgymin, jgymax, jgxmin, jgxmax, kxd, kyd, ksub1, ksub2;
    int i, j, ii, ip1, im1, jp1, jm1, ier, zero;
    float *gni, *gno;
    float wt, wt4, dip1, dim1, djp1, djm1, dsum, wsum;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Set filter weight.
     */
    wt = .125;
    wt4 = 4. * wt;

    /*
     * Get the grid number.
     */
    dg_gets ( &ni, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number and do the smoothing.
     */
    dg_nxts ( &no, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &ni, &gni, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &no, &gno, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Apply five-point binomial smoother over subset grid. 
     */
    dg_qbnd ( &jgxmin, &jgxmax, &jgymin, &jgymax, iret );
    for ( j = jgymin; j <= jgymax; j++ ) {
	for ( i = jgxmin; i <= jgxmax; i++ ) {
	    ii = ( j - 1 ) * kxd + i;
	    if ( ERMISS ( gni[ii-1] ) ) {
		/*
		 * Check for missing data.
		 */
		gno[ii-1] = RMISSD;
	    } else {
		ip1 = ii + 1;
		if ( i+1 > jgxmax ) {
		    dip1 = RMISSD;
		} else {
		    dip1 = gni[ip1-1];
		}
		im1 = ii - 1;
		if ( i-1 < jgxmin ) {
		    dim1 = RMISSD;
		} else {
		    dim1 = gni[im1-1];
		}
		jp1 = ii + kxd;
		if ( j+1 > jgymax ) {
		    djp1 = RMISSD;
		} else {
		    djp1 = gni[jp1-1];
		}
		jm1 = ii - kxd;
		if ( j-1 < jgymin ) {
		    djm1 = RMISSD;
		} else {
		    djm1 = gni[jm1-1];
		}
		dsum = gni[ii-1] * wt4;
		wsum = wt4;
		if ( ! ERMISS ( dip1 ) ) {
		    dsum += dip1 * wt;
		    wsum += wt;
		}
		if ( ! ERMISS ( dim1 ) ) {
		    dsum += dim1 * wt;
		    wsum += wt;
		}
		if ( ! ERMISS ( djp1 ) ) {
		    dsum += djp1 * wt;
		    wsum += wt;
		}
		if ( ! ERMISS ( djm1 ) ) {
		    dsum += djm1 * wt;
		    wsum += wt;
		}
		gno[ii-1] = dsum/wsum ;
	    }
	}
    }

    /*
     * Make a name of the form 'SM5'//S and update header;
     * update stack.
     */
    dg_updh ( "SM5", &no, &ni, &zero, iret );
    dg_puts ( &no, iret );
    dg_esub ( &no, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
