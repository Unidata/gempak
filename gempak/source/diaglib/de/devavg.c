#include "ensdiag.h"

void de_vavg ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_vavg								*
 *									*
 * This subroutine computes the ensemble mean of its vector argument.	*
 *									*
 * de_vavg ( uarg, stprm, iret )					*
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
 * T. Lee/SAIC		04/05	Free unneeded internal grids		*
 * T. Lee/SAIC		06/05	Compute weighted mean			*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 * m.gamazaychikov/SAIC 01/08   Add ability to use weights              *
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    char **argu;
    int nu, nv, num, numu, numv, kxd, kyd, ksub1, ksub2, nina, one,
        level1, level2, ivcord, two, zero, i, j, ier, narg, numw, nsw;
    float *gnu, *gnv, *gnumu, *gnumv, du1, dv1, du2, dv2, *gwgt, *gnumw, 
           d1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    one = 1;
    two = 2;

    dg_ssub ( iret );

    /*
     * Get a new grid number.
     */
    dg_nxts ( &nu, iret );
    if ( *iret != 0 ) return;
    dg_nxts ( &nv, iret );
    if ( *iret != 0 ) return;
    dg_nxts ( &nsw, iret );
    if ( *iret != 0 ) return;

    /*
     * Initialize the output grid.
     */
    dg_getg ( &nu, &gnu, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nv, &gnv, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nsw, &gwgt, &kxd, &kyd, &ksub1, &ksub2, iret );
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	gnu[i] = 0.;
	gnv[i] = 0.;
        gwgt[i] = 0.;
    }

    /*
     * Set the number of input arguments. 
     * There may be two arguments for DE_VAVG.
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

    if ( narg == 1 ) {
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
               d1 = gnumw[j];
	       if ( ERMISS ( du1 ) || ERMISS ( dv1 ) || 
	   	    ERMISS ( du2 ) || ERMISS ( dv2 ) ) {
		    gnu[j] = RMISSD;
		    gnv[j] = RMISSD;
                }
                else if ( ERMISS ( d1 ) || ERMISS ( gwgt[j] ) ) {
                    gwgt[j]  = RMISSD;
	        } else {
                    gwgt[j] += gnumw[j];
	 	    gnu[j] += d1*du1; 
		    gnv[j] += d1*dv1;
	       }

           }

	   dg_frig ( &numu, &ier );
	   dg_frig ( &numv, &ier );
           dg_frig ( &numw, &ier );


        } else if (narg == 1)  {
	   de_mset ( &i, iret );
	   dg_pfun ( _ensdiag.allarg[0], iret );
	   if ( *iret != 0 )  {
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
	       if ( ERMISS ( du1 ) || ERMISS ( dv1 ) || 
	   	    ERMISS ( du2 ) || ERMISS ( dv2 ) ) {
		    gnu[j] = RMISSD;
		    gnv[j] = RMISSD;
	        } else {
	 	    gnu[j] += du1 * _ensdiag.enswts[i];
		    gnv[j] += dv1 * _ensdiag.enswts[i];
	       }
	   }
	   dg_frig ( &numu, &ier );
	   dg_frig ( &numv, &ier );
        }
    }

    /*
     * Normalize the truncated set of ensemble members
     */
    if ( narg == 2 ) {
      for ( j = ksub1 - 1; j < ksub2; j++ ) {
          if ( ERMISS ( gwgt[j] ) || ERMISS ( gnu[j] ) ) { 
             gnu[j] = RMISSD;
          }
          else if ( ERMISS ( gwgt[j] ) || ERMISS ( gnv[j] ) ) {
             gnv[j] = RMISSD;
          } else {
             gnu[j] = gnu[j]/gwgt[j];
             gnv[j] = gnv[j]/gwgt[j];
          }
      }
    }
    /*
     * Reset DGCMN.CMN and set internal grid identifier.
     */
    de_rset ( iret );
    dg_udig ( "EXX_", &nu, &nv, &_ensdiag.idgens, stprm, iret );
    dg_esub ( &nu, &nv, &zero, &zero, &ier );
    if ( ier != 0 )  *iret = ier;

    return;
}
