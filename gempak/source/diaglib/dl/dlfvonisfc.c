#include "lyrdiag.h"

#define SGN(x) ( (x) > 0.01 ? 1. : ( G_ABS(x) < 0.00001 ? 0. : -1. ) )

void dl_fvonisfc ( char *uargs, char *glvl, char *stprm, int *iret )
/************************************************************************
 * dl_fvonisfc                                                          *
 *                                                                      *
 * This subroutine manages the layer thresholding function:             *
 *   LYR_FVONISFC.  This computes a level of, or another function whose *
 *   value is coincident with, the desired threshold value.             *
 *                                                                      *
 * Usage: LYR_FVONISFC(fvalue & fisfc & visfc & n & gradflag[|levels])  *
 *   where fvalue is the GFUNC whose value on the isosurface is rqrd    *
 *     e.g. HGHT, PRES.                                                 *
 *   where fisfc is the GFUNC that determines the isosurface, e.g.      *
 *     RELH, TEMP, WND.                                                 *
 *   where visfc is the GFUNC that determines the local value of the    *
 *     of the isosurface e.g. 70 for 70%                                *
 *   where n is an integer used as a search direction & counter.        *
 *     positive values for scanning bottom to top.  Negative for top to *
 *     bottom.  n= 1->1st occurrence; n=2->2nd occurrence               *
 *   where gradflag is function to flag required gradient across        *
 *     isosurface.  If < 0, values of fisfc must decrease in the dir.   *
 *     of the search.  If = 0, it does not matter whether values        *
 *     increase or not. If > 0, values of fisfc must increase in the    *
 *     direction of the search.                                         *
 *                                                                      *
 * dl_fvonisfc ( uargs, glvl, stprm, iret )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *      *uargs		char  		Layer diagnostic arguments      *
 *      *glvl		char  		Level specification             *
 *                                                                      *
 * Output parameters:                                                   *
 *      *stprm		char  		Substitution string             *
 *      *iret		int 		Return code                     *
 *                                       +1 = extra argument specified  *
 *                                        0 = normal return             *
 *                                       -2 = no argument               *
 *                                       -8 = cannot parse arguments    *
 *                                       -9 = cannot be computed        *
 **                                                                     *
 * Additional Comments:  UARGS split by separator '|' into              *
 *                       uag(1) & uag(2). Uag(1) houses the arguments   *
 *                       fvalue, fisfc, visfc, n, and gradflag.         *
 *                       Uag(2) houses the arguments for levels.        *
 *                       Uag(1) is split by separator '&' into          *
 *                       5-element array uin(1) thru uin(5) as follows: *
 *                         uin(1) - fvalue                              *
 *                         uin(2) - fisfc                               *
 *                         uin(3) - visfc                               *
 *                         uin(4) - n                                   *
 *                         uin(5) - gradflag                            *
 * Log:                                                                 *
 * L. Hinson/AWC                3/06                                    *
 * R. Tian/SAIC			5/06	Translated from Fortran		*
 ************************************************************************/
{
    int nuag, nin, nlev, lflag, nval, one, zero, level1, level2, ivcord,
        searchdir, startlev, endlev, ilev, k, ier;
    int og, tcg, tg, gg, ng2, ng, p_fvalueg, wg, cg, fvalueg, kxd, kyd,
        ksub1, ksub2;
    float *gog, *gtcg, *gtg, *ggg, *gng2, *gng, *gp_fvalueg, *gwg, *gcg,
          *gfvalueg;
    char uag[2][LLMXLN+1], uin[MAXARG][LLMXLN+1], *cdp[MAXARG], tname[13],
         time1[21], time2[21], pdum[13];
/*----------------------------------------------------------------------*/
    *iret = 0;
    one = 1;
    zero = 0;
    nval = 1;

    /*
     * Establish ownership of the output grid.
     */
    dg_ssub ( iret );

    /*
     * Split uargs on |.
     */
    for ( k = 0; k < 2; k++ ) cdp[k] = uag[k];
    cst_clst ( uargs, '|', "", 2, LLMXLN, cdp, &nuag, &ier );
    if ( uag[0][0] == '\0' ) {
	*iret = -2;
	return;
    } else if ( uag[1][0] == '\0' ) {
	strcpy ( uag[1], glvl );
    }

    /*
     * Split uag(1) on &.
     */
    for ( k = 0; k < MAXARG; k++ ) cdp[k] = uin[k];
    cst_clst ( uag[0], '&', "", MAXARG, LLMXLN, cdp, &nin, &ier );

    /*
     * Set the list of levels.
     */
    dl_lvls ( cdp, nin, uag[1], &nlev, iret );
    if ( *iret != 0 ) return;
    
    /*
     * Determine if layer average quantity and set lflag if true.
     */
    dg_qlyr ( uin[0], &lflag, &ier );

    /*
     * Get a new grid number.
     */
    dg_nxts ( &og, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &og, &gog, &kxd, &kyd, &ksub1, &ksub2, &ier );

    /*
     * Initialize the output grid.
     */
    for ( k = ksub1 - 1; k < ksub2; k++ ) {
	gog[k] = RMISSD;
    }

    dg_nxts ( &tcg, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &tcg, &gtcg, &kxd, &kyd, &ksub1, &ksub2, &ier );

    for ( k = ksub1 - 1; k < ksub2; k++ ) {
	gtcg[k] = 0.;
    }

    dg_iset ( "LDLEVL1", &nval, &_lyrdiag.lyrlvs1[0], &ier );
    dg_iset ( "LDLEVL2", &nval, &_lyrdiag.lyrlvs2[0], &ier );

    /*
     * Get VISFC ( Argument 3 ) and store to dgg(tg)
     */
    dg_pfun ( uin[2], iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
        *iret = -8;
	return;
    }
    dg_driv ( &one, iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, uin[2], &ier, strlen("DG"), strlen(uin[2]) );
        *iret = -9;
        return;
    }
    dg_tops ( tname, &tg, time1, time2, &level1, &level2, &ivcord, pdum, &ier );
    dg_getg ( &tg, &gtg, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get Gradient Flag (Argument 5) and store to dgg(gg)
     */
    dg_pfun ( uin[4], iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
        *iret = -8;
        return;
    }
    dg_driv ( &one, iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, uin[4], &ier, strlen("DG"), strlen(uin[4]) );
        *iret = -9;
        return;
    }
    dg_tops ( tname, &gg, time1, time2, &level1, &level2, &ivcord, pdum, &ier );
    dg_getg ( &gg, &ggg, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get n  (Argument 4) and store to dgg(ng)
     * Note, two grids are required to handle n.  It was found
     * that if Argument 4 (n) was the same as Argument 5 (gg)
     * E.g. both set to -1, the grids management system
     * system would set ng to point to the the same grids
     * as gg.  This created problems in altering the
     * values of the gradient grids to be n.
     */
    dg_pfun ( uin[3], iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
        *iret = -8;
        return;
    }
    dg_driv ( &one, iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, uin[3], &ier, strlen("DG"), strlen(uin[3]) );
        *iret = -9;
        return;
    }
    dg_tops ( tname, &ng2, time1, time2, &level1, &level2, &ivcord, pdum, &ier);
    dg_getg ( &ng2, &gng2, &kxd, &kyd, &ksub1, &ksub2, &ier );
    searchdir = 1;
    startlev = 1;
    endlev = nlev;
    if ( gng2[0] < 0. ) {
	searchdir = -1;
        startlev = nlev - 2;
        endlev = -1;
    	dg_iset ( "LDLEVL1", &nval, &_lyrdiag.lyrlvs1[nlev-1], &ier );
    	dg_iset ( "LDLEVL2", &nval, &_lyrdiag.lyrlvs2[nlev-1], &ier );
    }

    /*
     * Create a physical ng grid derived from ng2.  If argument 4 and 5 are the
     * same value, they will be mapped to the same grids, creating problems in
     * evaluating the sign of the gradient in subsequent computations.
     */
    dg_nxts ( &ng, iret);
    if ( *iret != 0 ) return;
    dg_getg ( &ng, &gng, &kxd, &kyd, &ksub1, &ksub2, &ier );
    for ( k = ksub1 - 1; k < ksub2; k++ ) {
	gng[k] = gng2[k] * searchdir;
    }

    /*
     * Get fvalue (Argument 1) at 1st Level and store to dgg(p_valueg)
     */
    dg_pfun ( uin[0], iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
        *iret = -8;
        return;
    }
    dg_driv ( &one, iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, uin[0], &ier, strlen("DG"), strlen(uin[0]) );
        *iret = -9;
        return;
    }
    dg_tops ( tname, &p_fvalueg, time1, time2, &level1, &level2, &ivcord,
              pdum, &ier );
    dg_getg ( &p_fvalueg, &gp_fvalueg, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get fisfc (Argument 2) at 1st Level and store to dgg(wg)
     */
    dg_pfun ( uin[1], iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
        *iret = -8;
        return;;
    }
    dg_driv ( &one, iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, uin[1], &ier, strlen("DG"), strlen(uin[1]) );
        *iret = -9;
        return;
    }
    dg_tops ( tname, &wg, time1, time2, &level1, &level2, &ivcord, pdum, &ier );
    dg_getg ( &wg, &gwg, &kxd, &kyd, &ksub1, &ksub2, iret );

    for ( ilev = startlev; ilev != endlev; ilev += searchdir ) {
    	dg_iset ( "LDLEVL1", &nval, &_lyrdiag.lyrlvs1[ilev], &ier );
    	dg_iset ( "LDLEVL2", &nval, &_lyrdiag.lyrlvs2[ilev], &ier );

	/*
	 * Get fisfc (Argument 2) at Level and store to dgg(cg)
	 */
	dg_pfun ( uin[1], iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
	    *iret = -8;
	    return;
	}
	dg_driv ( &one, iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "DG", iret, uin[1], &ier, strlen("DG"), strlen(uin[1]) );
	    *iret = -9;
	    return;
	}
        dg_tops ( tname, &cg, time1, time2, &level1, &level2, &ivcord, pdum,
                  &ier );
	dg_getg ( &cg, &gcg, &kxd, &kyd, &ksub1, &ksub2, iret );

	/*
	 * Get fvalue (Argument 1) at Level and store to dgg(fvalueg)
	 */
	dg_pfun ( uin[0], iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
	    *iret = -8;
	    return;
	}
	dg_driv ( &one, iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "DG", iret, uin[0], &ier, strlen("DG"), strlen(uin[0]) );
	    *iret = -9;
	    return;
	}
        dg_tops ( tname, &fvalueg, time1, time2, &level1, &level2, &ivcord,
                  pdum, &ier );
	dg_getg ( &fvalueg, &gfvalueg, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( k = ksub1 - 1; k < ksub2; k++ ) {
	    if ( ( gwg[k] <= gtg[k] && gtg[k] < gcg[k] ) ||
	         ( gwg[k] >= gtg[k] && gtg[k] > gcg[k] ) ) {
		if ( (G_DIFF(SGN(gcg[k]-gwg[k]), SGN(ggg[k])) ) ||
		     G_ABS ( ggg[k] ) < .00001 ) {
		    if ( gtcg[k] < gng[k] ) {
			gtcg[k] += 1;
			if ( G_DIFF(gtcg[k], gng[k]) ) {
			    if ( !G_DIFFT(gcg[k] - gwg[k], 0.0F, GDIFFD)) {
				gog[k] = gp_fvalueg[k] +
				         ( gfvalueg[k] - gp_fvalueg[k] ) *
                                ( ( gtg[k] - gwg[k] ) / ( gcg[k] - gwg[k] ) );
			    }
			}
		    }
		}
	    }
	    gp_fvalueg[k] = gfvalueg[k];
	    gwg[k] = gcg[k];
	}
	dg_frig ( &cg, &ier);
	dg_frig ( &fvalueg, &ier );
    }

    /*
     * Set internal grid identifier.
     */
    dg_udig ( "LXX_", &og, &zero, &_lyrdiag.lyrnid, stprm, iret );
    dg_esub ( &og, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
