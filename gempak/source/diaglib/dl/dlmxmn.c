#include "lyrdiag.h"

void dl_mxmn ( char *uargs, char *glvl, char *stprm, int *iret )
/************************************************************************
 * dl_mxmn                                                              *
 * This subroutine manages the Layer Max/Min function:                  *
 *   LYR_MXMN.  This function computes the following:                   *
 *   1. The maximum or minimum value of a scalar quantity.              *
 *   2. The value of a second output function coincident with the       *
 *     extrema of the input function. E.g. a level such as hght or pres *
 *     or some other function such as absolute vorticity.               *
 *                                                                      *
 * Usage: LYR_MXMN(argin & fldout [& argout] [|levels])                 *
 *   where argin is the name of the input field.  (SPED, RELH, TEMP)    *
 *   where fldout specifies the desired output:                         *
 *     MXVAL -> returns max of argin                                    *
 *     MNVAL -> returns min of argin                                    *
 *     MXOUT -> returns value of argout where argin is maximum          *
 *     MNOUT -> returns value of argout where argin is minimum          *
 *   where argout can be a simple or complex function. (HGHT, THTA,     *
 *     PRES, AVOR(WND).   This argument is required when fldout is set  *
 *     to either MXOUT or MNOUT.                                        *
 *                                                                      *
 * dl_mxmn ( uargs, glvl, stprm, iret )                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *      *uargs		char		Layer diagnostic arguments      *
 *      *glvl		char		Level specification             *
 *                                                                      *
 * Output parameters:                                                   *
 *      *stprm		char		Substitution string             *
 *      *iret		int		Return code                     *
 *                                       +1 = extra argument specified  *
 *                                        0 = normal return             *
 *                                       -2 = no argument               *
 *                                       -8 = cannot parse arguments    *
 *                                       -9 = cannot be computed        *
 **                                                                     *
 * Additional Comments:  UARGS split by separator '|' into              *
 *                       uag(1) & uag(2). Uag(1) houses the arguments   *
 *                       argin, fldout, and argout.                     *
 *                       Uag(2) houses the argument for levels.         *
 *                       Uag(1) is split by separator '&' into          *
 *                       3-element array uin(1) thru uin(3) as follows: *
 *                         uin(1) - argin                               *
 *                         uin(2) - fldout                              *
 *                         uin(3) - argout                              *
 * Log:                                                                 *
 * L. Hinson/AWC                03/06                                   *
 * K. Brill/HPC                 05/06  Cleanup; og=mg for value output; *
 *                                     Call DL_LVLS with uin2; check for*
 *                                     correctly spelled keywords	*
 * R. Tian/SAIC			05/06  Translated from Fortran		*
 ************************************************************************/
{
    int nuag, nin, kin, nlev, nval, ilev, one, zero, k, ier;
    int outflag;
    int mg, og, cg, lg, kxd, kyd, ksub1, ksub2, level1, level2, ivcord;
    float *gmg, *gog, *gcg, *glg;
    char uag[2][LLMXLN+1], uin[MAXARG][LLMXLN+1], *cdp[MAXARG],
         uin2[2][LLMXLN+1];
    char tname[13], time1[21], time2[21], pdum[13];
/*----------------------------------------------------------------------*/
    *iret = 0;
    nval = 1;
    one = 1;
    zero = 0;
    outflag = G_FALSE;

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
     * The type of output must be specified as keyword.
     */
    if ( strcmp ( uin[1], "MXVAL" ) != 0 && 
         strcmp ( uin[1], "MNVAL" ) != 0 &&
         strcmp ( uin[1], "MXOUT" ) != 0 &&
         strcmp ( uin[1], "MNOUT" ) != 0 ) {
	*iret = -12;
	return;
    }

    if ( strstr ( uin[1], "OUT" ) ) {
	outflag = G_TRUE;
    }

    /*
     * Set the list of levels using first argument.  The
     * second argument is not a diagnostic function.
     */
    strcpy ( uin2[0], uin[0] );
    if ( nin == 3 ) {
	strcpy ( uin2[1], uin[2] );
	kin = 2;
    } else {
	kin = 1;
    }
    for ( k = 0; k < kin; k++ ) cdp[k] = uin2[k];
    dl_lvls ( cdp, kin, uag[1], &nlev, iret );
    if ( *iret != 0 ) return;

    /*
     * Evaluate the first argument.
     */
    dg_iset ( "LDLEVL1", &nval, &_lyrdiag.lyrlvs1[0], &ier );
    dg_iset ( "LDLEVL2", &nval, &_lyrdiag.lyrlvs2[0], &ier );
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
    dg_tops ( tname, &mg, time1, time2, &level1, &level2, &ivcord, pdum, &ier );
    dg_getg ( &mg, &gmg, &kxd, &kyd, &ksub1, &ksub2, &ier );

    if ( strcmp ( uin[1], "MXOUT" ) == 0 || strcmp ( uin[1], "MNOUT" ) == 0 ) {
	/*
	 * Setup Output Grid
	 */
	dg_pfun ( uin[2], iret );
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
	dg_tops ( tname, &og, time1, time2, &level1, &level2, &ivcord, pdum,
                  &ier );
	dg_getg ( &og, &gog, &kxd, &kyd, &ksub1, &ksub2, &ier );
    }

    for ( ilev = 1; ilev < nlev; ilev++ ) {
	dg_iset ( "LDLEVL1", &nval, &_lyrdiag.lyrlvs1[ilev], &ier );
	dg_iset ( "LDLEVL2", &nval, &_lyrdiag.lyrlvs2[ilev], &ier );
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
	dg_tops ( tname, &cg, time1, time2, &level1, &level2, &ivcord, pdum,
                  &ier );
	dg_getg ( &cg, &gcg, &kxd, &kyd, &ksub1, &ksub2, &ier );
	if ( cg <= 0 || cg >= 100 ) return;

	if ( strcmp ( uin[1], "MXOUT" ) == 0 ||
             strcmp ( uin[1], "MNOUT" ) == 0 ) {
	    dg_pfun ( uin[2], iret );
	    if ( *iret != 0 ) {
		er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
                *iret = -8;
                return;
	    }
	    dg_driv ( &one, iret );
	    if ( *iret != 0 ) {
		er_wmsg ( "DG", iret, uin[0], &ier, strlen("DG"),
                          strlen(uin[0]) );
                *iret = -9;
                return;
	    }
	    dg_tops ( tname, &lg, time1, time2, &level1, &level2, &ivcord,
                      pdum, &ier );
	    dg_getg ( &lg, &glg, &kxd, &kyd, &ksub1, &ksub2, &ier );
	    if ( lg <= 0 || lg >= 100 ) return;
	}

	if ( strcmp ( uin[1], "MXVAL" ) == 0 ||
             strcmp ( uin[1], "MXOUT" ) == 0 ) {
	    for ( k = ksub1 - 1; k < ksub2; k++ ) {
		if ( gcg[k] > gmg[k] ) {
		    gmg[k] = gcg[k];
		    if ( strcmp ( uin[1], "MXOUT" ) == 0 ) {
			gog[k] = glg[k];
		    }
		}
	    }
	}

	if ( strcmp ( uin[1], "MNVAL" ) == 0 ||
	     strcmp ( uin[1], "MNOUT" ) == 0 ) {
	    for ( k = ksub1 - 1; k < ksub2; k++ ) {
		if ( gcg[k] < gmg[k] ) {
		    gmg[k] = gcg[k];
		    if ( strcmp ( uin[1], "MNOUT" ) == 0 ) {
			gog[k] = glg[k];
		    }
		}
	    }
	}

	dg_frig ( &cg, &ier);
	if ( outflag == G_TRUE ) {
	    dg_frig ( &lg, &ier);
	}
    }

    if ( strcmp ( uin[1], "MXVAL" ) == 0 || strcmp ( uin[1], "MNVAL" ) == 0 ) {
	og = mg;
    }

    /*
     * Set internal grid identifier.
     */
    dg_udig ( "LXX_", &og, &zero, &_lyrdiag.lyrnid, stprm, iret );
    dg_esub ( &og, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
