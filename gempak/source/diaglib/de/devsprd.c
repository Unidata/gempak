#include "ensdiag.h"

void de_vsprd ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_vsprd								*
 *									*
 * This subroutine computes the ensemble spread of its vector argument.	*
 *									*
 * de_vsprd ( uarg, stprm, iret )					*
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
 * T. Lee/SAIC		05/05	Free unneeded internal grids		*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 * K. Brill/HPC         11/10   Set any negative sqrt argument to zero	*
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    int nu, nv, nu2, nv2, num, numu, numv, kxd, kyd, ksub1, ksub2, nina,
        zero, two, i, j, ier;
    int level1, level2, ivcord;
    float *gnu, *gnv, *gnu2, *gnv2, *gnumu, *gnumv, du1, dv1, du2, dv2,
        du3, dv3, ovrnm1, ovrnn, ss;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    two = 2;

    dg_ssub ( iret );

    /*
     * Get new grid numbers.
     */
    dg_nxts ( &nu, iret );
    if ( *iret != 0 ) return;
    dg_nxts ( &nv, iret );
    if ( *iret != 0 ) return;
    dg_nxts ( &nu2, iret );
    if ( *iret != 0 ) return;
    dg_nxts ( &nv2, iret );
    if ( *iret != 0 ) return;

    /*
     * Initialize the output grid.
     */
    dg_getg ( &nu, &gnu, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nv, &gnv, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nu2, &gnu2, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nv2, &gnv2, &kxd, &kyd, &ksub1, &ksub2, iret );
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	gnu[i] = 0.;
	gnv[i] = 0.;
	gnu2[i] = 0.;
	gnv2[i] = 0.;
    }

    /*
     * Set the number of input arguments.  There is only one argument
     * for DE_VSPRD.
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
	dg_driv ( &two, iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "DG", iret, _ensdiag.allarg[0], &ier,
	        strlen("DG"), strlen(_ensdiag.allarg[0]) );
	    *iret = -9;
	    return;
	}

	/*
	 * Retrieve the output grid from the stack.  Check that the 
	 * output is a vector.
	 */
	dg_tops ( tname, &num, time1, time2, &level1, &level2,
	    &ivcord, pdum, iret );
	numu = num / 100;
	numv = num - numu * 100;
	dg_getg ( &numu, &gnumu, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &numv, &gnumv, &kxd, &kyd, &ksub1, &ksub2, iret );
	for ( j = ksub1 - 1; j < ksub2; j++ ) {
	    du1 = gnumu[j];
	    dv1 = gnumv[j]; 
	    du2 = gnu[j];
	    dv2 = gnv[j];
	    du3 = gnu2[j];
	    dv3 = gnv2[j];
	    if ( ERMISS ( du1 ) || ERMISS ( dv1 ) || 
	         ERMISS ( du2 ) || ERMISS ( dv2 ) || 
	         ERMISS ( du3 ) || ERMISS ( dv3 ) ) {
		gnu[j] = RMISSD;
		gnv[j] = RMISSD;
		gnu2[j] = RMISSD;
		gnv2[j] = RMISSD;
	    } else {
		gnu[j] += gnumu[j];
		gnv[j] += gnumv[j];
		gnu2[j] += gnumu[j] * gnumu[j];
		gnv2[j] += gnumv[j] * gnumv[j];
	    }
	}
	dg_frig ( &numu, &ier );
	dg_frig ( &numv, &ier );
    }

    ovrnm1 = 1. / ( _ensdiag.nummbr - 1 );
    ovrnn  = ovrnm1 / _ensdiag.nummbr;

    /*
     * Compute Variance.
     */
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	du2 = gnu[i];
	dv2 = gnv[i];
	du3 = gnu2[i];
	dv3 = gnv2[i];
	if ( ERMISS ( du2 ) || ERMISS ( du3 ) || 
	     ERMISS ( dv2 ) || ERMISS ( dv3 ) ) {
	    gnu[i] = RMISSD;
	} else {
	    ss = ovrnm1 * ( gnu2[i] + gnv2[i] ) -
	 	 ovrnn * ( gnu[i] * gnu[i] + gnv[i] * gnv[i] );
            if ( ss < 0.0 ) {
		ss = 0.0;
	    }
	    gnu[i] = sqrt ( ss );
	}
    }

    /*
     * Reset DGCMN.CMN and set internal grid identifier.
     */
    de_rset ( iret );
    dg_udig ( "EXX_", &nu, &zero, &_ensdiag.idgens, stprm, iret );
    dg_esub ( &nu, &zero, &zero, &zero, &ier );
    if ( ier != 0 )  *iret = ier;

    return;
}
