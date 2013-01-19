#include "ensdiag.h"

void de_mode ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_mode								*
 *									*
 * This subroutine computes the mode of weighted ensemble members.	*
 *									*
 * de_mode( uarg, stprm, iret )						*
 *									*
 * Input:								*
 *	*uarg		const char	Function argument string	*
 *									*
 * Output parameters:							*
 *	*stprm		char		Substitution string		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *                                       -8 = cannot parse argument     *
 *                                       -9 = ensemble cannot computed  *
 * Log:									*
 * R. Tian/SAIC		7/05	Modified from de_prcntl			*
 * R. Tian/SAIC		1/06	Translated from Fortran			*
 * T. Piper/SAIC	08/06	Added G_DIFFs				*
 * m.gamazaychikov/SAIC 01/08   Add ability to use weights              *
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    char **argu;
    int igo, num, kxd, kyd, ksub1, ksub2, level1, level2, ivcord, nina,
        one, zero, iswflg, istop, ii, iwpntr, ier, jj, nn, kk, kmax, kthrd;
    int missing, done, l1e2, l1e3, l2e3, l123;
    int nsw, numw, narg;
    float *gigo, *gnum, sum, emean, swpbuf, rng, x1, x2, thrd, xmrk1, xmrk2,
        wtsum[3], sum2, xrng[3][2], dxm1, dxm2, dxm3, dxmin, wtmax, 
        *gnumw, *gwgt, d1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    one = 1;

    dg_ssub ( iret );

    /*
     * Get new grid number for output grid
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
     * Set the number of input arguments. 
     * There may be two arguments.
     */
    for ( ii = 0; ii < MXARGS; ii++ ) {
	_ensdiag.allarg[ii][0] = '\0';
    }
    nina = 2;
    argu = (char **)cmm_malloc2d ( 2, LLMXLN, sizeof(char), &ier );
    cst_clst ( (char *)uarg, '&', " ", nina, LLMXLN, argu, &narg, &ier );
    for ( ii = 0; ii < narg; ii++ ) {
        strcpy ( _ensdiag.allarg[ii], argu[ii] );
        if ( ii > 0 && strcmp(argu[ii], " ") == 0 ) {
            cst_rlch ( RMISSD, 1, _ensdiag.allarg[ii], &ier );
        }
    }
                                                                               
    /*
     * If weight grid is provided get new grid number 
     * for sum-weight grid and initialize it
     */
    if ( narg == 2 ) {
       dg_nxts ( &nsw, iret );
       if ( *iret != 0 ) return;
       dg_getg ( &nsw, &gwgt, &kxd, &kyd, &ksub1, &ksub2, iret );
       for ( ii = ksub1 - 1; ii < ksub2; ii++ ) {
         gwgt[ii] = 0.;
       }
    }
     else if ( narg == 1 ) {
        cst_rlch ( RMISSD, 1, _ensdiag.allarg[1], &ier );
    }
                                                                               
    cmm_free2d ( (void **) argu, &ier );

    if ( narg < 1 ) {
        *iret = -15;
        return;
    }

    /*
     * Scan the allarg array.
     */
    de_scan ( &narg, iret );
    if ( *iret != 0 ) return;

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
	 * starting index.
	 */
	dg_tops ( tname, &num, time1, time2, &level1, &level2,
	    &ivcord, pdum, iret );
	_ensdiag.iglist[ii] = num;

        /* 
         * If the weight grid present store the starting index
         * of the weight grid.
         */
        if ( narg == 2 ) {
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

    /*
     * Loop over grid points of each member.
     */
    for ( jj = ksub1 - 1; jj < ksub2; jj++ ) {
	sum = 0.0;
	missing = G_FALSE;
	for ( ii = 0; ii < _ensdiag.nummbr; ii++ ) {
	    num = _ensdiag.iglist[ii];
	    dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );
	    _ensdiag.emvalu[ii] = gnum[jj];
	    _ensdiag.igpntr[ii] = ii;
	    sum += _ensdiag.emvalu[ii];
           /*
            * Fill out the weight array and normalize by the sum of weights
            */
            if ( narg == 2 ) {
               numw = _ensdiag.iwlist[ii];
               dg_getg ( &numw, &gnumw, &kxd, &kyd, &ksub1, &ksub2, iret );
               _ensdiag.ewtval[ii] = gnumw[jj] / gwgt[jj];
            }
	    if ( ERMISS ( _ensdiag.emvalu[ii] ) ) missing = G_TRUE;
	}
	emean = sum / _ensdiag.nummbr;
	if ( missing == G_FALSE ) {
	    /*
	     * Bubble sorting the grid values in emvalu with 
	     * emvalue (1) lowest and emvalu (nummbr) highest.
	     */
	    iswflg = 1;
	    istop = _ensdiag.nummbr - 1;
	    while ( iswflg != 0 && istop >= 0 ) {
		iswflg = 0;
		for ( ii = 0; ii < istop; ii++ ) {
		    if ( _ensdiag.emvalu[ii] > _ensdiag.emvalu[ii+1] ) {
			iswflg = 1;
			swpbuf = _ensdiag.emvalu[ii];
			iwpntr = _ensdiag.igpntr[ii];
			_ensdiag.emvalu[ii] = _ensdiag.emvalu[ii+1];
			_ensdiag.igpntr[ii] = _ensdiag.igpntr[ii+1];
			_ensdiag.emvalu[ii+1] = swpbuf;
			_ensdiag.igpntr[ii+1] = iwpntr;
		    }
		}
		istop--;
	    }

	    /*
	     * Use iterative trisection method to compute the mode.
	     */
	    nn = _ensdiag.nummbr;
	    rng = _ensdiag.emvalu[nn-1] - _ensdiag.emvalu[0];
	    x1 = _ensdiag.emvalu[0] - 0.01F * rng;
	    x2 = _ensdiag.emvalu[nn-1] + 0.01F * rng;
	    done = G_FALSE;
	    kmax = 3 * nn;
	    kk = 0;
	    while ( done == G_FALSE && kk < kmax ) {
		kk++;
		thrd = rng / 3.;
		xmrk1 = x1 + thrd;
		xmrk2 = x2 - thrd;
		for ( ii = 0; ii < 3; ii++ ) {
		    wtsum[ii] = 0.0F;
		}
		sum2 = 0.0F;
		for ( ii = 0; ii < nn; ii++ ) {
		    if ( _ensdiag.emvalu[ii] > x1 &&
			 _ensdiag.emvalu[ii] < xmrk1 ) {
                       if ( narg == 2 ) {
			   wtsum[0] += _ensdiag.ewtval[_ensdiag.igpntr[ii]];
                       }
		         else { 
                           wtsum[0] += _ensdiag.enswts[_ensdiag.igpntr[ii]];
                       }
		    } else if ( _ensdiag.emvalu[ii] >= xmrk1 &&
			        _ensdiag.emvalu[ii] <= xmrk2 ) {
                       if ( narg == 2 ) {
			   wtsum[1] += _ensdiag.ewtval[_ensdiag.igpntr[ii]];
			   sum2 += _ensdiag.emvalu[ii]*
			           _ensdiag.ewtval[_ensdiag.igpntr[ii]];
                       }
		         else { 
			   wtsum[1] += _ensdiag.enswts[_ensdiag.igpntr[ii]];
			   sum2 += _ensdiag.emvalu[ii]*
			           _ensdiag.enswts[_ensdiag.igpntr[ii]];
                       }
		    } else if ( _ensdiag.emvalu[ii] > xmrk2 &&
			        _ensdiag.emvalu[ii] < x2 ) {
                       if ( narg == 2 ) {
			   wtsum[2] += _ensdiag.ewtval[_ensdiag.igpntr[ii]];
                       }
		         else { 
			   wtsum[2] += _ensdiag.enswts[_ensdiag.igpntr[ii]];
                       }
		    }
		}
		if ( G_DIFF(wtsum[0], 0.0F) && !G_DIFF(wtsum[1], 0.0F) && G_DIFF(wtsum[2], 0.0F) )
		    done = G_TRUE;
		if ( done == G_FALSE ) {
		    xrng[0][0] = x1;
		    xrng[0][1] = xmrk1;
		    xrng[1][0] = xmrk1;
		    xrng[1][1] = xmrk2;
		    xrng[2][0] = xmrk2;
		    xrng[2][1] = x2;
		    dxm1 = G_ABS ( 0.5F * ( x1 + xmrk1 ) - emean );
		    dxm2 = G_ABS ( 0.5F * ( xmrk1 + xmrk2 ) - emean );
		    dxm3 = G_ABS ( 0.5F * ( xmrk2 + x2 ) - emean );
		    wtmax = G_MAX ( G_MAX ( wtsum[0], wtsum[1] ), wtsum[2] );
		    if ( G_DIFF(wtsum[0], wtsum[1]) && G_DIFF(wtsum[0], wtmax) ) {
		        l1e2 = G_TRUE;
		    } else {
		        l1e2 = G_FALSE;
		    }
		    if ( G_DIFF(wtsum[0], wtsum[2]) && G_DIFF(wtsum[0], wtmax) ) {
		        l1e3 = G_TRUE;
		    } else {
		        l1e3 = G_FALSE;
		    }
		    if ( G_DIFF(wtsum[1], wtsum[2]) && G_DIFF(wtsum[1], wtmax) ) {
		        l2e3 = G_TRUE;
		    } else {
		        l2e3 = G_FALSE;
		    }
		    if ( G_DIFF(wtsum[0], wtsum[1]) && G_DIFF(wtsum[0], wtsum[2]) ) {
		        l123 = G_TRUE;
		    } else {
		        l123 = G_FALSE;
		    }

		    /*
		     * For ties, choose third closest to mean.
		     */
		    kthrd = 0;
		    if ( l123 == G_TRUE ) {
			dxmin = G_MIN ( G_MIN ( dxm1, dxm2 ), dxm3 );
			if ( G_DIFF(dxm1, dxmin) ) {
			    kthrd = 1;
			} else if ( G_DIFF(dxm2, dxmin) ) {
			    kthrd = 2;
			} else if ( G_DIFF(dxm3, dxmin) ) {
			    kthrd = 3;
			}
		    } else if ( l1e2 == G_TRUE ) {
			dxmin = G_MIN ( dxm1, dxm2 );
			if ( G_DIFF(dxm1, dxmin) ) {
			    kthrd = 1;
			} else if ( G_DIFF(dxm2, dxmin) ) {
			    kthrd = 2;
			}
		    } else if ( l1e3 == G_TRUE ) {
			dxmin = G_MIN ( dxm1, dxm3 );
			if ( G_DIFF(dxm1, dxmin) ) {
			    kthrd = 1;
			} else if ( G_DIFF(dxm3, dxmin) ) {
			    kthrd = 3;
			}
		    } else if ( l2e3 == G_TRUE ) {
			dxmin = G_MIN ( dxm2, dxm3 );
			if ( G_DIFF(dxm2, dxmin) ) {
			    kthrd = 2;
			} else if ( G_DIFF(dxm3, dxmin) ) {
			    kthrd = 3;
			}
		    } else if ( G_DIFF(wtsum[0], wtmax) ) {
			kthrd = 1;
		    } else if ( G_DIFF(wtsum[1], wtmax) ) {
			kthrd = 2;
		    } else if ( G_DIFF(wtsum[2], wtmax) ) {
			kthrd = 3;
		    }
		    if ( kthrd == 0 ) {
			sum2 = RMISSD;
			done = G_TRUE;
		    } else {
			if ( kthrd == 1 ) {
			    x1 = xrng[0][0] - rng / 6.0F;
			    x2 = xrng[0][1] + rng / 6.0F;
			} else if ( kthrd == 2 ) {
			    x1 = xrng[1][0] - rng / 6.0F;
			    x2 = xrng[1][1] + rng / 6.0F;
			} else if ( kthrd == 3 ) {
			    x1 = xrng[2][0] - rng / 6.0F;
			    x2 = xrng[2][1] + rng / 6.0F;
			}
			rng = x2 - x1;
		    }
		}
	    }
	    if ( ! ERMISS (sum2) && kk < kmax ) {
	       gigo[jj] = sum2 / wtsum[1];
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
