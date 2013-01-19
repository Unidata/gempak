#include "ensdiag.h"

void de_prcntl ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_prcntl								*
 *									*
 * This subroutine returns a value at each grid point such that the 	*
 * value returned is greater than or equal to the value found at the	*
 * same grid point in P% of the weighted members of an ensemble.  The	*
 * value of P ranges between 0 and 100 and may vary from grid point to	*
 * point.								*
 *									*
 * The relationship between the percentile value, p, and the index, k,	*
 * in the order statistics of count N is				*
 *									*
 *                   ( k - 1 ) / ( N - 1 ) = p		(1)		*
 *									*
 * Rewriting this in terms of equally weighted order statistics 	*
 * (multiplying both sides by (N-1)/N) yields				*
 *									*
 *                   (k-1)*(1/N) = p - p*(1/N)	 	(2)		*
 *									*
 * Since k can have a fractional value, the weights may vary, and the	*
 * (1/N) subtracted on both sides of (2) must be the first weight value *
 * (w(1)), the problem is one of finding integer K and residual weight	*
 * wr such that								*
 *									*
 *                    K							*
 *              wr + SUM w(i) = p ( 1 - w(1) )          (3)		*
 *                   i=2						*
 *									*
 * The value of wr is easily obtained by solving (3) after summing the	*
 * weights up to the point in the order statistics where adding on one	*
 * more weight exceeds the value of the R.H.S of (3).  The value of wr  *
 * establishes the position in the weight summation to which to		*
 * interpolate the values of the order statistics, x, according to the	*
 * following linear relationship:					*
 *									*
 *    wr / [ W(K+1) - W(K) ] = [ x - x(K) ] / [ x(K+1) - x(K) ]  (4)    *
 *									*
 * In (4), W(K) is the summation of the weights from i=2 to K.  The	*
 * percentile value is found by solving (4) for x.  Since the denom-	*
 * inator on the L.H.S of (4) is just w(K+1), the value of x is		*
 *									*
 *      x = x(K) + [ wr / w(K+1) ] * [ x(K+1) - x(K) ]         (5)	*
 *									*
 *									*
 * de_prcntl ( uarg, stprm, iret )					*
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
 * T. Lee/SAIC		01/05						*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 * T. Piper/SAIC	08/06	Added G_DIFF				*
 * K. Brill/HPC		08/06   Fix to remove low bias; document eqtns	*
 * m.gamazaychikov/SAIC 01/08   Add ability to use weights              *
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    char **argu;
    int igo, igp, num, kxd, kyd, ksub1, ksub2, nina, narg, level1, level2,
        ivcord, zero, one, three, ii, jj, kk, ll, ier;
    int wmesg, nmesg, iswflg, istop, iwpntr;
    int nsw, numw;
    float *gigo, *gigp, *gnum, data, swpbuf, pntt, psum, smw, wr,
          *gnumw, *gwgt, d1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    one = 1;
    three = 3;

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
     * Set the number of input arguments.  There are two arguments
     * for DE_PRCNTL.
     */
    for ( ii = 0; ii < MXARGS; ii++ ) {
	_ensdiag.allarg[ii][0] = '\0';
    }
    nina = 3;
    argu = (char **)cmm_malloc2d ( 3, MXFLSZ+1, sizeof(char), &ier );
    cst_clst ( (char *)uarg, '&', " ", nina, MXFLSZ, argu, &narg, &ier );
    for ( ii = 0; ii < narg; ii++ ) {
        strcpy ( _ensdiag.allarg[ii], argu[ii] );
    }

    /*
     * If weight grid is provided get new grid number
     * for sum-weight grid and initialize it
     */
    if ( narg == 3 ) {
       dg_nxts ( &nsw, iret );
       if ( *iret != 0 ) return;
       dg_getg ( &nsw, &gwgt, &kxd, &kyd, &ksub1, &ksub2, iret );
       for ( ii = ksub1 - 1; ii < ksub2; ii++ ) {
         gwgt[ii] = 0.;
       }
    }
    cmm_free2d ( (void **) argu, &ier );
    if ( narg < 2 ) {
	*iret = -15;
	return;
    }

    /*
     * Scan the allarg array.
     */
    de_scan ( &narg, iret );
    if ( *iret != 0 ) return;

    /*
     * Evaluate the static argument defined by the second entry in
     * uarg or allarg (2).
     */
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

        /*
         * If the weight grid present store the starting index
         * of the weight grid.
         */
        if ( narg == 3 ) {
           dg_pfun ( _ensdiag.allarg[2], iret );
           if ( *iret != 0 ) {
              er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
              *iret = -8;
              return;
           }
           dg_driv ( &one, iret );
           if ( *iret != 0 ) {
              er_wmsg ( "DG", iret, _ensdiag.allarg[2], &ier,
                   strlen("DG"), strlen(_ensdiag.allarg[2]) );
              *iret = -9;
              return;
           }

           dg_tops ( tname, &numw, time1, time2, &level1, &level2,
                &ivcord, pdum, iret );
           dg_getg ( &numw, &gnumw, &kxd, &kyd, &ksub1, &ksub2, iret );
           _ensdiag.iwlist[ii] = numw;
           /*
            * the weight summing grid
            */
           for ( jj = ksub1 - 1; jj < ksub2; jj++ ) {
                d1 = gnumw[jj];
                if ( ERMISS ( d1 ) || ERMISS ( gwgt[jj] ) ) {
                    gwgt[jj]  = RMISSD;
                } else {
                    gwgt[jj] += gnumw[jj];
                }
           }
        }
    }

    wmesg = G_FALSE;
    nmesg = G_FALSE;
    for ( ll = ksub1 - 1; ll < ksub2; ll++ ) {
	for ( ii = 0; ii < _ensdiag.nummbr; ii++ ) {
	    num = _ensdiag.iglist[ii];
	    dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );
	    data = gnum[ll];
            /*
            * Fill out the weight array and normalize by the sum of weights
            */
            if ( narg == 3 ) {
               numw = _ensdiag.iwlist[ii];
               dg_getg ( &numw, &gnumw, &kxd, &kyd, &ksub1, &ksub2, iret );
               _ensdiag.ewtval[ii] = gnumw[ll] / gwgt[ll];
            } 
	    if ( ! ERMISS ( data ) ) {
	        _ensdiag.emvalu[ii] = data;
	        _ensdiag.igpntr[ii] = ii;
	        if ( ii == _ensdiag.nummbr - 1 ) {
	            /*
		     * Bubble sorting the grid values in emvalu with 
		     * emvalue (1) lowest and emvalu (nummbr) highest.
		     */
	            iswflg = 1;
	            istop = _ensdiag.nummbr - 1;
		    while ( iswflg != 0 && istop >= 0 ) {
		        iswflg = 0;
		        for ( kk = 0; kk < istop; kk++ ) {
		            if ( _ensdiag.emvalu[kk] > _ensdiag.emvalu[kk+1] ) {
		                iswflg = 1;
			        swpbuf = _ensdiag.emvalu[kk];
			        iwpntr = _ensdiag.igpntr[kk];
			        _ensdiag.emvalu[kk] = _ensdiag.emvalu[kk+1];
			        _ensdiag.igpntr[kk] = _ensdiag.igpntr[kk+1];
			        _ensdiag.emvalu[kk+1] = swpbuf;
			        _ensdiag.igpntr[kk+1] = iwpntr;
		            }
		        }
		        istop--;
		    }

		    /*
		     * Set normalized target percentile.
		     */
		    pntt = gigp[ll] / 100.0F;
		    if ( pntt >= 1. ) {
		        gigo[ll] = _ensdiag.emvalu[_ensdiag.nummbr-1];
		        if ( pntt > 1.0F && wmesg == G_FALSE ) {
		            er_wmsg ( "DE", &one, " ", &ier,
		                strlen("DE"), strlen(" ") );
			    wmesg = G_TRUE;
		        }
		    } else if ( pntt <= 0. ) {
		        gigo[ll] = _ensdiag.emvalu[0];
		        if ( pntt < 0.0F &&  nmesg == G_FALSE ) {
		            er_wmsg ( "DE", &three, " ", &ier,
			        strlen("DE"), strlen(" ") );
			    nmesg = G_TRUE;
		        }
		    } else {
		        jj = 0;
		        psum = 0.0;
                        if ( narg == 3 ) {
			   pntt = pntt * ( 1.0F - _ensdiag.ewtval[_ensdiag.igpntr[0]] );
                        }
                         else {
			   pntt = pntt * ( 1.0F - _ensdiag.enswts[_ensdiag.igpntr[0]] );
                        } 
		        while (jj < _ensdiag.nummbr - 1 && psum < pntt ) {
		            jj++;
                            /*
                             *  The 1st weight ([0]) must be omitted from the
                             *  summation.
			     */
                             if ( narg == 3 ) {
		                psum += _ensdiag.ewtval[_ensdiag.igpntr[jj]];
                             }
                              else {
		                psum += _ensdiag.enswts[_ensdiag.igpntr[jj]];
                             } 
		        }

		        /*
		         * Compute the percentile value for the output grid.
		         */
		        if ( G_DIFF(psum, pntt) ) {
		            gigo[ll] = _ensdiag.emvalu[jj];
		        } else {
                            if ( narg == 3 ) {
		                smw = psum - _ensdiag.ewtval[_ensdiag.igpntr[jj]];
			        wr = pntt - smw;
			        if ( G_DIFF (_ensdiag.ewtval[_ensdiag.igpntr[jj]], 0.0F) ) {
		                   gigo[ll] =  RMISSD;
			        } else {
		                   gigo[ll] =  _ensdiag.emvalu[jj-1] + 
                                            ( wr / _ensdiag.ewtval[_ensdiag.igpntr[jj]] ) *
			                    (_ensdiag.emvalu[jj]-_ensdiag.emvalu[jj-1]);
                                }
                            }
                              else {
		                smw = psum - _ensdiag.enswts[_ensdiag.igpntr[jj]];
			        wr = pntt - smw;
			        if ( G_DIFF (_ensdiag.enswts[_ensdiag.igpntr[jj]], 0.0F) ) {
		                   gigo[ll] =  RMISSD;
			        } else {
		                   gigo[ll] =  _ensdiag.emvalu[jj-1] + 
                                            ( wr / _ensdiag.enswts[_ensdiag.igpntr[jj]] ) *
			                    (_ensdiag.emvalu[jj]-_ensdiag.emvalu[jj-1]);
			        }
                            }
		        }
		    }
	        }
	    }
	}
    }

    /*
     * Reset DGCMN.CMN and set internal grid identifier.
     */
    de_rset ( iret );
    dg_udig ( "EXX_", &igo, &zero, &_ensdiag.idgens, stprm, iret );
    dg_esub ( &igo, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
