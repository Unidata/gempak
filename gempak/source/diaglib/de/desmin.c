#include "ensdiag.h"

void de_smin ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_smin								*
 *									*
 * This subroutine computes the minimum of its scalar arguments among	*
 * ensemble members.							*
 *									*
 * de_smin ( uarg, stprm, iret )					*
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
 * R. Tian/SAIC		 6/05						*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    int ns, num, kxd, kyd, ksub1, ksub2, level1, level2, ivcord, nina,
        one, zero, i, j, ier;
    float *gns, *gnum, d1, d2;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    one = 1;

    dg_ssub ( iret );

    /*
     * Get a new grid number.
     */
    dg_nxts ( &ns, iret );
    if ( *iret != 0 ) return;

    /*
     * Initialize the output grid.
     */
    dg_getg ( &ns, &gns, &kxd, &kyd, &ksub1, &ksub2, iret );
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	gns[i] = FLT_MAX;
    }

    /*
     * Set the number of input arguments.  There is only one argument
     * for DE_SMIN.
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
	dg_driv ( &one, iret );
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
	    d2 = gns[j];
	    if ( ERMISS ( d1 ) || ERMISS ( d2 ) ) {
		gns[j] = RMISSD;
	    } else {
		gns[j] = G_MIN ( d1, d2 );
	    }
	}
	dg_frig ( &num, &ier );
    }

    /*
     * Reset DGCMN.CMN and set internal grid identifier.
     */
    de_rset ( iret );
    dg_udig ( "EXX_", &ns, &zero, &_ensdiag.idgens, stprm, iret );
    dg_esub ( &ns, &zero, &zero, &zero, &ier );
    if ( ier != 0 )  *iret = ier;

    return;
}
