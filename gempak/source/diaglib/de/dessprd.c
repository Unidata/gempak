#include "ensdiag.h"

void de_ssprd ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_ssprd								*
 *									*
 * This subroutine computes the ensemble spread of its scalar argument.	*
 *									*
 * de_ssprd ( uarg, stprm, iret )					*
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
 * T. Lee/SAIC		04/05						*
 * T. Lee/SAIC		05/05	Free unneeded internal grid		*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 * S. Jacobs/NCEP	 8/09	Use double arrays internally		*
 * K. Brill/HPC         11/10   Set any negative sqrt argument to zero	*
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    int ns, ns2, num, kxd, kyd, ksub1, ksub2, level1, level2, ivcord,
        nina, one, zero, i, j, ier;
    float *gns, *gnum, ovrnm1, ovrnn;
    double *dgns, *dgns2, d1, d2, d3;
/*----------------------------------------------------------------------*/
    *iret = 0;
    one = 1;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get new grid numbers.
     */
    dg_nxts ( &ns, iret );
    if ( *iret != 0 ) return;
    dg_nxts ( &ns2, iret );
    if ( *iret != 0 ) return;

    /*
     * Initialize the output grid.
     * Allocate internal double arrays.
     */
    dg_getg ( &ns,  &gns,  &kxd, &kyd, &ksub1, &ksub2, iret );
    G_MALLOC(dgns, double, kxd*kyd, "DE_SSPRD");
    G_MALLOC(dgns2, double, kxd*kyd, "DE_SSPRD");
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
        gns[i] = 0.;
        dgns[i] = 0.;
	dgns2[i] = 0.;
    }

    /*
     * Set the number of input arguments.  There is only one argument
     * for DE_SSPRD.
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
	    d3 = dgns2[j];
	    if ( ERMISS ( d1 ) || ERMISS ( d2 ) || ERMISS ( d3 ) ) {
		dgns[j] = RMISSD;
		dgns2[j] = RMISSD;;
	    } else {
		dgns[j] += gnum[j];
		dgns2[j] += gnum[j] * gnum[j];
	    }
	}
	dg_frig ( &num, &ier );
    }

    ovrnm1 = 1. / ( _ensdiag.nummbr - 1 );
    ovrnn  = ovrnm1 / _ensdiag.nummbr;

    /*
     * Compute Variance.
     */
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	d2 = dgns[i];
	d3 = dgns2[i];
	if ( ERMISS ( d2 ) || ERMISS ( d3 ) ) {
	    dgns[i] = RMISSD;
	} else {
	    dgns[i] = ovrnm1 * dgns2[i] - ovrnn * dgns[i] * dgns[i];
	}
    }

    /*
     * Compute spread (standard deviation).
     */
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	d2 = dgns[i];
	if ( ERMISS ( d2 ) ) {
	    dgns[i] = RMISSD;
	} else {
            if ( dgns[i] < 0.0 ) {
		dgns[i] = 0.0;
	    }
	    dgns[i] = sqrt ( dgns[i] );
	}
    }

    /*
     * Assign the result to the output array and free the internal arrays.
     */
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
        gns[i] = (float)dgns[i];
    }
    G_FREE(dgns, double);
    G_FREE(dgns2, double);

    /*
     * Reset DGCMN.CMN and set internal grid identifier.
     */
    de_rset ( iret );
    dg_udig ( "EXX_", &ns, &zero, &_ensdiag.idgens, stprm, iret );
    dg_esub ( &ns, &zero, &zero, &zero, &ier );
    if ( ier != 0 )  *iret = ier;

    return;
}
