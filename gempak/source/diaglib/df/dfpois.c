#include "df.h"

void df_pois ( int *iret )
/************************************************************************
 * df_pois								*
 *									*
 * This subroutine solves a Poisson equation given a forcing function,	*
 * and boundary conditions.						*
 *									*
 *									*
 * df_pois ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/NMC		 1/93						*
 * S. Jacobs/EAI	 2/93	Eliminated ref to Neumann cond		*
 * K. Brill		 4/95	Fill missing values over subset		*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 4/02	Use internal grids as scratch arrays	*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids for scl fctrs	*
 * K. Brill/HPC		11/02	Eliminate SUBA; use bounds instead	*
 * R. Tian/SAIC		12/02	Try to make loop more clear		*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int nval, dgsubg, jgymin, jgymax, jgxmin, jgxmax, kxd, kyd, ksub1,
        ksub2, ixmscl, iymscl;
    int nfrc, nbcs, num, naa, nbb, itypbc, i, j, ii, ier, zero;
    float *gnfrc, *gnbcs, *gnum, *gnaa, *gnbb, *gixmscl, *giymscl, 
        gddx, gddy;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Compute map scale factors.
     */
    dg_mscl ( iret );
    if ( *iret != 0 ) return;

    /*
     * Get the forcing function grid from the stack.
     */
    dg_gets ( &nfrc, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &nfrc, &gnfrc, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get the boundary condition/guess grid.
     */
    dg_gets ( &nbcs, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &nbcs, &gnbcs, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Set the type of boundary conditions:
     *		0 = Dirichlet
     *		1 = Alternate Dirichlet
     */
    itypbc = 0;

    /*
     * Set grid points outside of subset area to missing.
     */
    nval = 1;
    dg_iget ( "DGSUBG", &nval, &dgsubg, iret );
    dg_qbnd ( &jgxmin, &jgxmax, &jgymin, &jgymax, iret );

    if ( dgsubg == G_FALSE ) {
	if ( jgymin > 1 ) {
	    for ( j = 1; j <= jgymin - 1; j++ ) {
		for ( i = 1; i <= kxd; i++ ) {
		    ii = ( j - 1 ) * kxd + i;
		    gnfrc[ii-1] = RMISSD;
		    gnbcs[ii-1] = RMISSD;
		}
	    }
	}
	if ( jgymax < kyd ) {
	    for ( j = jgymax + 1; j <= kyd; j++ ) {
	        for ( i = 1; i <= kxd; i++ ) {
		    ii = ( j - 1 ) * kxd + i;
		    gnfrc[ii-1] = RMISSD;
		    gnbcs[ii-1] = RMISSD;
	        }
	    }
        }
        if ( jgxmin > 1 ) {
	    for ( i = 1; i <= jgxmin - 1; i++ ) {
		for ( j = jgymin; j <= jgymax; j++ ) {
		    ii = ( j - 1 ) * kxd + i;
		    gnfrc[ii-1] = RMISSD;
		    gnbcs[ii-1] = RMISSD;
		}
	    }
	}
	if ( jgxmax < kxd ) {
	    for ( i = jgxmax + 1; i <= kxd; i++ ) {
		for ( j = jgymin; j <= jgymax; j++ ) {
		    ii = ( j - 1 ) * kxd + i;
		    gnfrc[ii-1] = RMISSD;
		    gnbcs[ii-1] = RMISSD;
		}
	    }
	}
    }

    /*
     * Get the output grid.
     */ 
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get scratch work arrays.
     */
    dg_nxts ( &naa, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &naa, &gnaa, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_nxts ( &nbb, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &nbb, &gnbb, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Call the POISSON solver.
     */
    dg_qmsl ( &ixmscl, &iymscl, &gddx, &gddy, iret );
    dg_getg ( &ixmscl, &gixmscl, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &iymscl, &giymscl, &kxd, &kyd, &ksub1, &ksub2, iret );

    pd_slvp ( gnfrc, gnbcs, &itypbc, gixmscl, giymscl, &kxd, &kyd, 
              &gddx, &gddy, gnaa, gnbb, gnum, &ier );

    /*
     * Make a name of the form 'POIS'//S and update header;
     * update stack.
     */ 
    dg_updh ( "POIS", &num, &nfrc, &zero, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
