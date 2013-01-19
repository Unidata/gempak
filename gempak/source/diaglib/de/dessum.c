#include "ensdiag.h"

void de_ssum ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_ssum								*
 *									*
 * This subroutine computes the sum over the ensemble of a scalar.	*
 *									*
 * de_ssum ( uarg, stprm, iret )					*
 *									*
 * Input and parameters:						*
 *	*uarg		const char	Function argument string	*
 *									*
 * Output parameters:							*
 *	*stprm		char		Substitution string		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -8 = cannot parse argument	*
 *					 -9 = ensemble cannot computed	*
 **									*
 * Log:									*
 * K. Brill/HPC         08/10	Created from de_ssprd			*
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    int ns, num, kxd, kyd, ksub1, ksub2, level1, level2, ivcord,
        nina, one, zero, i, j, ier;
    float *gns, *gnum;
    double *dgns, d1, d2;
/*----------------------------------------------------------------------*/
    *iret = 0;
    one = 1;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get a new grid number.
     */
    dg_nxts ( &ns, iret );
    if ( *iret != 0 ) return;

    /*
     * Initialize the output grid.
     * Allocate internal double arrays.
     */
    dg_getg ( &ns,  &gns,  &kxd, &kyd, &ksub1, &ksub2, iret );
    G_MALLOC(dgns, double, kxd*kyd, "DE_SSUM");
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
        gns[i] = 0.;
        dgns[i] = 0.;
    }

    /*
     * Set the number of input arguments.  There is only one argument
     * for DE_SSUM.
     */
    nina = 1;
    for ( i = 0; i < MXARGS; i++ ) {
	_ensdiag.allarg[i][0] = '\0';
    }
    strcpy ( _ensdiag.allarg[0], uarg );

    /*
     * Scan the allarg array.
     */
    de_scan ( &nina, iret );
    if ( *iret != 0 ) return;

    /*
     * Loop over number of members set by DE_SCAN.
     */
    for ( i = 0; i < _ensdiag.nummbr; i++ ) {
	de_mset ( &i, iret );
	dg_pfun ( _ensdiag.allarg[0], iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
	    *iret = -8;
	    return;
	}
	dg_driv( &one, iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "DG", iret, _ensdiag.allarg[0], &ier,
	        strlen("DG"), strlen(_ensdiag.allarg[0]) );
	    *iret = -9;
	    return;
	}

	/*
	 * Retrieve the output grid from the stack.  Check that the 
	 * output is a scalar.
	 */	
	dg_tops ( tname, &num, time1, time2, &level1, &level2,
	    &ivcord, pdum, iret );
	dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( j = ksub1 - 1; j < ksub2; j++ ) {
	    d1 = gnum[j];
	    d2 = dgns[j];
	    if ( ERMISS ( d1 ) || ERMISS ( d2 ) ) {
		dgns[j] = RMISSD;
	    } else {
		dgns[j] += gnum[j];
	    }
	}
	dg_frig ( &num, &ier );
    }

    /*
     * Assign the result to the output array and free the internal arrays.
     */
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
        gns[i] = (float)dgns[i];
    }
    G_FREE(dgns, double);

    /*
     * Reset DGCMN.CMN and set internal grid identifier.
     */
    de_rset ( iret );
    dg_udig ( "EXX_", &ns, &zero, &_ensdiag.idgens, stprm, iret );
    dg_esub ( &ns, &zero, &zero, &zero, &ier );
    if ( ier != 0 )  *iret = ier;

    return;
}
