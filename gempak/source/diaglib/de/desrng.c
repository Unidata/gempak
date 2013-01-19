#include "ensdiag.h"

void de_srng ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_srng								*
 *									*
 * This subroutine computes the range of its scalar arguments among	*
 * ensemble members. The range is the difference between the maximum	*
 * and the minimum.							*
 *									*
 * de_srng ( uarg, stprm, iret )					*
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
 * R. Tian/SAIC 	 6/05						*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    int nsmax, nsmin, num, kxd, kyd, ksub1, ksub2, level1, level2,
        ivcord, nina, one, zero, i, j, ier;
    float *gnsmax, *gnsmin, *gnum, d1, d2, d3;
/*----------------------------------------------------------------------*/
    *iret = 0;
    one = 1;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get new grid numbers for maximum and minimum fields.
     */
    dg_nxts ( &nsmax, iret );
    if ( *iret != 0 ) return;
    dg_nxts ( &nsmin, iret );
    if ( *iret != 0 ) return;

    /*
     * Initialize the output grid.
     */
    dg_getg ( &nsmax, &gnsmax, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nsmin, &gnsmin, &kxd, &kyd, &ksub1, &ksub2, iret );
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	gnsmax[i] = -FLT_MAX;
	gnsmin[i] =  FLT_MAX;
    }

    /*
     * Set the number of input arguments.  There is only one argument
     * for DE_SRNG.
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

	/*
	 * Compute the maximum and minimum.
	 */
	for ( j = ksub1 - 1; j < ksub2; j++ ) {
	    d1 = gnum[j];
	    d2 = gnsmax[j];
	    d3 = gnsmin[j];
	    if ( ERMISS ( d1 ) ) {
		gnsmax[j] = RMISSD;
		gnsmin[j] = RMISSD;
	    } else {
		if ( ! ERMISS ( d2 ) ) {
		    gnsmax[j] = G_MAX ( d1, d2 );
		}
		if ( ! ERMISS ( d2 ) ) {
		    gnsmin[j] = G_MIN ( d1, d3 );
		}
	    }
	}
	dg_frig ( &num, &ier );
    }

    /*
     * Compute the range.
     */
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	d1 = gnsmax[i];
	d2 = gnsmin[i];
	if ( ERMISS ( d1 ) || ERMISS ( d2 ) ) {
	    gnsmax[i] = RMISSD;
	} else {
	    gnsmax[i] = d1 - d2;
	}
    }
    dg_frig ( &nsmin, &ier );

    /*
     * Reset DGCMN.CMN and set internal grid identifier.
     */
    de_rset ( iret );
    dg_udig ( "EXX_", &nsmax, &zero, &_ensdiag.idgens, stprm, iret );
    dg_esub ( &nsmax, &zero, &zero, &zero, &ier );
    if ( ier != 0 )  *iret = ier;

    return;
}
