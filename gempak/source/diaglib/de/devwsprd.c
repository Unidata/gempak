#include "ensdiag.h"

void de_vwsprd ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_vwsprd								*
 *									*
 * This subroutine computes the weighted ensemble spread of its 	*
 * vector argument.							*
 *									*
 * de_vwsprd ( uarg, stprm, iret )					*
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
 * m.gamazaychikov/SAIC 01/08   From de_ssprd                           *
 * m.gamazaychikov/SAIC 01/08   Fixed the calculation problem           *
 * K. Brill/HPC         11/10   Set any negative sqrt argument to zero	*
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    char **argu;
    int nu, nv, nu2, nv2, num, numu, numv, kxd, kyd, ksub1, ksub2, nina,
        zero, two, i, j, ier;
    int level1, level2, ivcord, one, narg, numw, nsw;
    float *gnu, *gnv, *gnu2, *gnv2, *gnumu, *gnumv, du1, dv1, du2, dv2,
        du3, dv3, *gwgt, *gnumw, d4, ss;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    one = 1;
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
     * Set the number of input arguments. 
     * There may be three arguments.
     */
    for ( i = 0; i < MXARGS; i++ ) {
	_ensdiag.allarg[i][0] = '\0';
    }
    nina = 3;
    argu = (char **)cmm_malloc2d ( 3, LLMXLN, sizeof(char), &ier );
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
               d4 = gnumw[j];
	       if ( ERMISS ( du1 ) || ERMISS ( dv1 ) || 
	            ERMISS ( du2 ) || ERMISS ( dv2 ) || 
	            ERMISS ( du3 ) || ERMISS ( dv3 ) || 
                    ERMISS ( d4 ) ) {
		     gnu[j] = RMISSD;
		     gnv[j] = RMISSD;
		     gnu2[j] = RMISSD;
		     gnv2[j] = RMISSD;
                     gwgt[j] = RMISSD;
	       } else {
		     gnu[j] += gnumu[j] * d4;
		     gnv[j] += gnumv[j] * d4;
		     gnu2[j] += gnumu[j] * gnumu[j] * d4;
		     gnv2[j] += gnumv[j] * gnumv[j] * d4;
                     gwgt[j] += gnumw[j];
	       }
	   }
	   dg_frig ( &numu, &ier );
	   dg_frig ( &numv, &ier );
           dg_frig ( &numw, &ier );

        } else if ( narg == 1) {
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
	  	  gnu[j] += gnumu[j] * _ensdiag.enswts[i];
		  gnv[j] += gnumv[j] * _ensdiag.enswts[i];
		  gnu2[j] += gnumu[j] * gnumu[j] * _ensdiag.enswts[i];
		  gnv2[j] += gnumv[j] * gnumv[j] * _ensdiag.enswts[i];
	      }
	   }
	   dg_frig ( &numu, &ier );
	   dg_frig ( &numv, &ier );
        }
    }

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
             if ( narg == 2) {
                  d4 = gwgt[i];
                if ( ERMISS ( d4 ) ) {
	           gnu[i] = RMISSD;
                } else {
                    gnu [i] = gnu[i]/gwgt[i]; 
                    gnv [i] = gnv[i]/gwgt[i]; 

                    gnu2[i] = gnu2[i]/gwgt[i];
                    gnv2[i] = gnv2[i]/gwgt[i];
                  
                    ss = gnu2[i] + gnv2[i] - 
                         gnu[i]*gnu[i] - gnv[i]*gnv[i];
                    if ( ss < 0.0 ) {
			ss = 0.0;
	    	    }
                    gnu[i] = sqrt ( ss );
                }
             } else if ( narg == 1 ) {
                 ss = gnu2[i] + gnv2[i] - 
                      gnu[i]*gnu[i] - gnv[i]*gnv[i]; 
                 if ( ss < 0.0 ) {
		     ss = 0.0;
	    	 }
                 gnu[i] = sqrt ( ss );
	     }
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
