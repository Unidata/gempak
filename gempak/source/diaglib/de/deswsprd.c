#include "ensdiag.h"

void de_swsprd ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_swsprd								*
 *									*
 * This subroutine computes the weighted ensemble spread of its scalar  *
 * argument.								*
 *									*
 * de_swsprd ( uarg, stprm, iret )					*
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
 * m.gamazaychikov/SAIC	01/08	From de_ssprd				*
 * m.gamazaychikov/SAIC	01/08	Fixed the calculation problem		*
 * S. Jacobs/NCEP	 8/09	Use double arrays internally		*
 * K. Brill/HPC         11/10   Set any negative sqrt argument to zero	*
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    char **argu;
    int ns, ns2, num, kxd, kyd, ksub1, ksub2, level1, level2, ivcord,
        nina, one, zero, i, j, ier, narg, numw, nsw;
    float *gns, *gnum, *gwgt, *gnumw;
    double *dgns, *dgns2, d1, d2, d3, d4;
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
     */
    dg_getg ( &ns,  &gns,  &kxd, &kyd, &ksub1, &ksub2, iret );
    G_MALLOC(dgns, double, kxd*kyd, "DE_SWSPRD");
    G_MALLOC(dgns2, double, kxd*kyd, "DE_SWSPRD");
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
        gns[i] = 0.;
        dgns[i] = 0.;
	dgns2[i] = 0.;
    }

    /*
     * Set the number of input arguments.  There could be two arguments.
     */
    for ( i = 0; i < MXARGS; i++ ) {
	_ensdiag.allarg[i][0] = '\0';
    }
    nina = 2;
    argu = (char **)cmm_malloc2d ( 2, LLMXLN, sizeof(char), &ier );
    cst_clst ( (char *)uarg, '&', " ", nina, LLMXLN, argu, &narg, &ier );
    for ( i = 0; i < narg; i++ ) {
        strcpy ( _ensdiag.allarg[i], argu[i] );
        if ( i > 0 && strcmp(argu[i], " ") == 0 ) {
            cst_rlch ( RMISSD, 1, _ensdiag.allarg[i], &ier );
        }
    }

    cmm_free2d ( (void **) argu, &ier );
                                                                                       
    if ( narg < 1 ) {
        *iret = -15;
        return;
    }
      else if ( narg == 1 ) {
        cst_rlch ( RMISSD, 1, _ensdiag.allarg[1], &ier );
    }
      else if ( narg == 2 ) {
        dg_nxts ( &nsw, iret );
        if ( *iret != 0 ) return;
        dg_getg ( &nsw, &gwgt, &kxd, &kyd, &ksub1, &ksub2, iret );
        for ( i = ksub1 - 1; i < ksub2; i++ ) {
            gwgt[i] = 0.;
        } 
    }


    /*
     * Scan the allarg array.
     */
    de_scan ( &narg, iret );
    if ( *iret != 0 ) return;

    /*
     * Loop over number of members set by DE_SCAN.
     */
    for ( i = 0; i < _ensdiag.nummbr; i++ ) {
        if ( narg == 2 ) {
           de_mset ( &i, iret );
          /*
           * Compute weight grid and retrieve it from the stack.
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
                                                                                       
           dg_tops ( tname, &numw, time1, time2, &level1, &level2,
                     &ivcord, pdum, iret );
           dg_getg ( &numw, &gnumw, &kxd, &kyd, &ksub1, &ksub2, iret );

          /*
           * Compute field grid and retrieve it from the stack.
           */
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
              d4 = gnumw[j];
              if ( ERMISS ( d1 ) || ERMISS ( d2 ) || 
                   ERMISS ( d3 ) || ERMISS ( d4 ) ) {
                 dgns[j]  = RMISSD;
                 dgns2[j] = RMISSD;
                 gwgt[j] = RMISSD;
              } else {
                 dgns[j]  += d1 * d4;
                 dgns2[j] += d1 * d1 * d4;
                 gwgt[j] += d4;
              }
           }
           dg_frig ( &numw, &ier );
           dg_frig ( &num, &ier );
        } else if ( narg == 1 ) {
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
	 	 dgns[j] += d1 * _ensdiag.enswts[i];
		 dgns2[j] += d1 * d1 * _ensdiag.enswts[i];
	      }
	   }
	   dg_frig ( &num, &ier );
        }
    }

    /*
     * Compute Variance.
     */
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	d2 = dgns[i];
	d3 = dgns2[i];
	if ( ERMISS ( d2 ) || ERMISS ( d3 ) ) {
	    dgns[i] = RMISSD;
	} else {
            if ( narg == 2) {
                 d1 = gwgt[i];
	       if ( ERMISS ( d1 ) ) {
	          dgns[i] = RMISSD;
               } else {
	         dgns[i] = dgns[i]/gwgt[i];
	         dgns[i] = dgns2[i]/gwgt[i] - dgns[i] * dgns[i];
               }
            } else if ( narg == 1 ) {
	       dgns[i] = dgns2[i] - dgns[i] * dgns[i];
            }
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
