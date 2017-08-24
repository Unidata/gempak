#include "ensdiag.h"

void de_cval ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_cval								*
 *									*
 * This function determines the value of the function defined by PARM 	*
 * for which the probability is PRB that the observed value is less 	*
 * than or equal to that computed value based on an idealized ensemble 	*
 * forecast.								*
 *									*
 * GFUNC syntax: ENS_CVAL ( PARM & PRB & LWRBND & UPRBND )		*
 *									*
 * de_cval ( uarg, stprm, iret )					*
 *									*
 * Input and parameters:						*
 *	*uarg		const char	Function argument string	*
 *									*
 * Output parameters:							*
 *	*stprm		char		Substitution string		*
 *	*iret		int		Return code			*
 *					 +3 = Percentile < 0		*
 *					 +1 = Percentile > 100		*
 *					  0 = normal return		*
 *					 -8 = cannot parse argument	*
 *					 -9 = ensemble cannot computed	*
 * 					-15 = Incorrect # of arguments	*
 **									*
 * Log:									*
 * M. Li/SAIC 		10/06						*
 * M. Li/SAIC 		10/06 	Added a check for missing value		*
 * K. Brill/HPC      20080131   Add intrinsic weight computations; fix	*
 *   				eliminate duplicates coding error	*
 * K. Brill/HPC      20101118   Check for single value order stats case *
 * K. Brill/IMSG     20161222   Check against bounds when psum exactly  *
 *				equals the specified cumulative prob    *
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    char **argu;
    int igo, igp, num, kxd, kyd, ksub1, ksub2, nina, narg, level1, level2,
        ivcord, zero, one, ii, jj, kk, ll, mm, nn, ier;
    int iswflg, istop;
    float *gigo, *gigp, *gnum, data, swpbuf, psum;
    float *gilwr, *giupr, *tmpwt, wtbuf, tol;
    float *zwts, *zfreq;
    float zsum;
    float vn, qlt, qrt, fta, cta, aa, bb, cc;
    Boolean ibreak;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    one = 1;

    dg_ssub ( iret );

    /*
     * Get a new grid number.
     */
    dg_nxts ( &igo, iret );
    if ( *iret != 0 ) return;

    /*
     * Initialize the output grid.
     */
    dg_getg ( &igo, &gigo, &kxd, &kyd, &ksub1, &ksub2, iret );
    for ( ii = ksub1 - 1; ii < ksub2; ii++ ) {
	gigo[ii] = RMISSD;
    }

    /*
     * Set the number of input arguments.  There are up to four arguments
     * for DE_CVAL.
     */
    for ( ii = 0; ii < MXARGS; ii++ ) {
	_ensdiag.allarg[ii][0] = '\0';
    }
    nina = 4;
    argu = (char **)cmm_malloc2d ( 4, MXFLSZ+1, sizeof(char), &ier );
    cst_clst ( (char *)uarg, '&', " ", nina, MXFLSZ, argu, &narg, &ier );
    for ( ii = 0; ii < narg; ii++ ) {
        strcpy ( _ensdiag.allarg[ii], argu[ii] );
	if ( ii > 0 && strcmp(argu[ii], " ") == 0 ) {
	    cst_rlch ( RMISSD, 1, _ensdiag.allarg[ii], &ier );
	}
    }

    if ( narg == 2 ) {
	cst_rlch ( RMISSD, 1, _ensdiag.allarg[2], &ier );
	cst_rlch ( RMISSD, 1, _ensdiag.allarg[3], &ier );
    }
    if ( narg == 3 ) {
        cst_rlch ( RMISSD, 1, _ensdiag.allarg[3], &ier );
    }

    cmm_free2d ( (void **) argu, &ier );
    if ( narg < 2 ) {
	*iret = -15;
	return;
    }

    /*
     * Scan the allarg array.
     */
    de_scan ( &nina, iret );
    if ( *iret != 0 ) return;

    /*
     * Evaluate the static arguments.
     */
    for ( ii = 2; ii < nina; ii++ ) {
	dg_pfun ( _ensdiag.allarg[ii], iret );
        dg_driv ( &one, iret );
	dg_tops ( tname, &igp, time1, time2, &level1, &level2,
                  &ivcord, pdum, iret );
	if ( ii == 2 ) {
            dg_getg ( &igp, &gilwr, &kxd, &kyd, &ksub1, &ksub2, iret );
	}
 	else {
            dg_getg ( &igp, &giupr, &kxd, &kyd, &ksub1, &ksub2, iret );
	}
    }
    
    dg_pfun ( _ensdiag.allarg[1], iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
	*iret = -8;
	return;
    }
    dg_driv ( &one, iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, _ensdiag.allarg[1], &ier,
	    strlen("DG"), strlen(_ensdiag.allarg[1]) );
	*iret = -9;
	return;
    }

    /*
     * Retrieve the output grid from the stack.  Check that the 
     * output is a scalar.
     */	
    dg_tops ( tname, &igp, time1, time2, &level1, &level2,
        &ivcord, pdum, iret );
    dg_getg ( &igp, &gigp, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Loop over number of members set by DE_SCAN.
     */
    for ( ii = 0; ii < _ensdiag.nummbr; ii++ ) {
	de_mset ( &ii, iret );
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
	 * Retrieve the output grid from the stack and store the
	 * grid number.
	 */
	dg_tops ( tname, &num, time1, time2, &level1, &level2,
	    &ivcord, pdum, iret );
	_ensdiag.iglist[ii] = num;
    }

    /*
     * Get memory for intrinsic weights (zwts), intrinsic weight
     * frequency (zfreq), and temporary weights.
     */
    G_MALLOC ( zwts, float, _ensdiag.nummbr+1, "x" );
    G_MALLOC ( zfreq, float, _ensdiag.nummbr+1, "x" );
    G_MALLOC ( tmpwt, float, _ensdiag.nummbr+2, "x" );

    for ( ll = ksub1 - 1; ll < ksub2; ll++ ) {

	if ( ERMISS(gigp[ll]) ) continue;	

        if ( gigp[ll] < 0.0F || gigp[ll] > 1.0F ) continue;

	for ( ii = 0; ii < _ensdiag.nummbr; ii++ ) {
	    ibreak = False;
	    num = _ensdiag.iglist[ii];
	    dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );
	    data = gnum[ll];
	    if ( ! ERMISS ( data ) ) {
	        _ensdiag.emvalu[ii+1] = data;
		tmpwt[ii+1] = _ensdiag.enswts[ii];

	        if ( ii == _ensdiag.nummbr - 1 ) {
	            /*
		     * Bubble sorting the grid values in emvalu with 
		     * emvalue (1) lowest and emvalu (nummbr) highest.
		     */
	            iswflg = 1;
	            istop = _ensdiag.nummbr;
		    while ( iswflg != 0 && istop > 0 ) {
		        iswflg = 0;
		        for ( kk = 1; kk < istop; kk++ ) {
		            if ( _ensdiag.emvalu[kk] > _ensdiag.emvalu[kk+1] ) {
		                iswflg = 1;
			        swpbuf = _ensdiag.emvalu[kk];
				wtbuf = tmpwt[kk];
			        _ensdiag.emvalu[kk] = _ensdiag.emvalu[kk+1];
				tmpwt[kk] = tmpwt[kk+1];
			        _ensdiag.emvalu[kk+1] = swpbuf;
			        tmpwt[kk+1] = wtbuf;
		            }
		        }
		        istop--;
		    }

		    /*
		     * Check for identical values and compute intrinsic weight
      		     * frequency (zfreq).
		     */
		    mm = _ensdiag.nummbr;
		    nn = mm;
		    /*
                     * Initialize intrinsic weight frequency array.
                     */
                    for (kk = 1; kk <= nn; kk++){
                        zfreq[kk] = 1.0F;
                    }
		    tol = 0.001F * (_ensdiag.emvalu[mm]-_ensdiag.emvalu[1]) / mm;
		    for (kk = 1; kk < mm; kk++) {
			if ( G_ABS(_ensdiag.emvalu[kk] - _ensdiag.emvalu[kk+1]) <= tol ) {
			    tmpwt[kk] += tmpwt[kk+1];
			    zfreq[kk] = zfreq[kk] + 1.0F;
			    mm--;
			    for (jj = kk+1; jj <= mm; jj++) {
				_ensdiag.emvalu[jj] = _ensdiag.emvalu[jj+1];
				tmpwt[jj] = tmpwt[jj+1];
			    } 
			    /* This algorithm was originally coded incorrectly.  The value
			     * of kk must also be held back to correctly eliminate three
			     * or more identical values.
			     */
			    kk--;
			}
		    } 

		    /*
		     * Fabricate order statistics if it has collapsed to a single value.            
		     */
		    if ( mm == 1 ) {
		    	if ( G_DIFF(_ensdiag.emvalu[1], 0.0F) ) {
			    _ensdiag.emvalu[1] = -0.00001F;
			    _ensdiag.emvalu[2] = 0.00001F;
			}
			else {
                            _ensdiag.emvalu[2] = _ensdiag.emvalu[1] + 0.00001F * G_ABS(_ensdiag.emvalu[1]);
			    _ensdiag.emvalu[1] -= 0.00001F * G_ABS(_ensdiag.emvalu[1]);
			}

			tmpwt[1] = 0.5F;
		        tmpwt[2] = 0.5F;
			mm = 2;
			zfreq[1] = 1.0F;
			zfreq[2] = 1.0F;
		    }
		    /*
		     *Compute and sum intrinsic weights.
		    */
		    zwts[1] = zfreq[1] / ( _ensdiag.emvalu[2] - _ensdiag.emvalu[1] );
		    zsum = zwts[1];
		    for (kk=2; kk < mm; kk++){
			zwts[kk] = ( zfreq[kk] * 2.0F ) / ( _ensdiag.emvalu[kk+1] - _ensdiag.emvalu[kk-1] );
			zsum = zsum + zwts[kk];
		    }
		    zwts[mm] = zfreq[mm] / ( _ensdiag.emvalu[mm] - _ensdiag.emvalu[mm-1] );
		    zsum = zsum + zwts[mm];
		    /*
		     * Scale external weights by normalized intrinsic weights and
		     * normalize.
		     */
 		    psum = 0.0F;
		    for (kk=1; kk <= mm; kk++ ){
			tmpwt[kk] = ( zwts[kk] / zsum ) * tmpwt[kk];
			psum = psum + tmpwt[kk];
		    }
		    for (kk=1; kk <= mm; kk++ ){
			tmpwt[kk] = tmpwt[kk] / psum;
		    }
	        } /*End "if" for all members ready check.*/
	    }
	    else {
		ibreak = True;
		break;
	    } /*End "if" for check for non-missing value.*/
	} /*End "for" loop over members.*/

	if ( ibreak ) continue;

	/*
         * Compute Qun, the area; Vn, the normalized value; 
	 * w(), normalized weight; and qlt, qrt.
         */
        vn = 0.0F;
        for ( kk = 2; kk <= mm; kk++ ) {
            vn += 0.5 * (tmpwt[kk] + tmpwt[kk-1]) * (_ensdiag.emvalu[kk] - _ensdiag.emvalu[kk-1]);
        }
	/*
	 * If the distribution is a Dirac spike over a single value, then set the result to
         * that single value.
	 */
	if ( G_DIFF ( vn, 0.0 ) ) {
	    gigo[ll] = _ensdiag.emvalu[1];
	    continue;
	}
        vn = vn / (1.0F - 2.0F / (nn+1));

        for ( kk = 1; kk <= mm; kk++ ) {
            tmpwt[kk] = tmpwt[kk] / vn;
        }

        qlt = _ensdiag.emvalu[1] - 2.0F / (tmpwt[1] * (nn + 1));
        qrt = _ensdiag.emvalu[mm] + 2.0F / (tmpwt[mm] * (nn + 1));

        tmpwt[0] = 0.0F;
        tmpwt[mm+1] = 0.0F;
        _ensdiag.emvalu[0] = qlt;
        _ensdiag.emvalu[mm+1] = qrt;

 	psum = 0.0F;
	for ( kk = 1; kk <= mm + 1; kk++ ) {

	    /*
	     * Compute CTA.
	     */
	    cta = 0.5F * (tmpwt[kk] + tmpwt[kk-1]) * (_ensdiag.emvalu[kk] - _ensdiag.emvalu[kk-1]);
	    psum += cta;
	    if ( kk == mm + 1 ) psum = 1.0F;

	    /*
	     * If psum = PRB, assign q[i] to output grid and move on to next grid point.
	     * (This was coded incorrectly and fixed on 20080205 -KB.)
	     */
	    if ( G_DIFF ( gigp[ll], psum ) ) {
		gigo[ll] = _ensdiag.emvalu[kk];
		if ( ! ERMISS(gilwr[ll]) && gigo[ll] < gilwr[ll] ) gigo[ll] = gilwr[ll];
		if ( ! ERMISS(giupr[ll]) && gigo[ll] > giupr[ll] ) gigo[ll] = giupr[ll];
		break;
	    }

	    /*
	     * If psum > PRB. Solve the quadratic equation.
	     */
	    if ( psum > gigp[ll] ) {
	    	psum -= cta;
	    	fta = gigp[ll] - psum;
	    	aa = (tmpwt[kk] - tmpwt[kk-1]) / (_ensdiag.emvalu[kk] - _ensdiag.emvalu[kk-1]); 
	    	bb = 2.0F * tmpwt[kk-1];
	    	cc = 2.0F * fta;

	    	if ( G_DIFF ( aa, 0.0F ) ) {
	   	    gigo[ll] = _ensdiag.emvalu[kk-1] + cc / bb;
	    	}
 	    	else {
		    gigo[ll] = _ensdiag.emvalu[kk-1] + (sqrt(bb * bb + 4 * aa * cc) - bb) / (2 * aa);
	        }
	        
		if ( ! ERMISS(gilwr[ll]) && gigo[ll] < gilwr[ll] ) gigo[ll] = gilwr[ll];
		if ( ! ERMISS(giupr[ll]) && gigo[ll] > giupr[ll] ) gigo[ll] = giupr[ll];
	        
	        break;
	    }

	}

    }

    G_FREE (tmpwt, float);
    G_FREE (zfreq, float);
    G_FREE (zwts, float);
    /*
     * Reset DGCMN.CMN and set internal grid identifier.
     */
    de_rset ( iret );
    dg_udig ( "EXX_", &igo, &zero, &_ensdiag.idgens, stprm, iret );
    dg_esub ( &igo, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
