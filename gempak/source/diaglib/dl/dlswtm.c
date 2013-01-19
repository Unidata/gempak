#include "lyrdiag.h"

void dl_swtm ( char *uargs, char *glvl, char *stprm, int *iret )
/************************************************************************
 * dl_swtm								*
 *									*
 * This subroutine computes the weighted mean of a scalar.		*
 *									*
 * dl_swtm  ( uargs, glvl, stprm, iret )				*
 *									*
 * Input parameters:							*
 *	*uargs		char		Layer diagnostic arguments	*
 *	*glvl		char		Level specification		*
 *									*
 * Output parameters:							*
 *	*stprm		char		Substitution string		*
 *	*iret		int		Return code			*
 *					 +1 = extra argument specified	*
 *					  0 = normal return		*
 *					 -2 = no argument		*
 *					 -8 = cannot parse arguments	*
 *					 -9 = cannot be computed	*
 **									*
 * Log:									*
 * T. Lee/SAIC		03/05						*
 * S. Gilbert/NCEP	12/05	Translation from Fortran               	*
 ************************************************************************/
{
        const int       zero=0, one=1;
        int             i, j, ier, nval, kxd, kyd, ksub1, ksub2, lvcord;
	int		ns, num, ldlevl1, ldlevl2, level1, level2, ivcord;
        float           *gs, *grnum, dp, dp2;
        float           top, bot, d1, d2, top1, top2;
	char 		uag[2][LLMXLN], uin[MAXARG][LLMXLN], *cptr[MAXARG],
		 	tname[13], time1[21], time2[21], pdum[13];
	float		w[LLMXLV], lp[2][LLMXLV];
	int		lflag, nin, nlev, ilev, nuag, lenc;
/*----------------------------------------------------------------------*/
	*iret = 0;

        /*
         *	Establish ownership of the output grid.
         */
	dg_ssub ( iret );

        /*
         *	Get a new grid number.
         */
	dg_nxts ( &ns, iret );
	if ( *iret != 0 )  return;

        /*
         *	Initialize the output grid.
         */
        dg_getg ( &ns, &gs, &kxd, &kyd, &ksub1, &ksub2, iret );
	for ( i = ksub1 -1; i < ksub2; i++ ) gs [ i ] = 0.;

        /*
         *	Split uargs on |.
         */
        cptr[0] = uag[0];
        cptr[1] = uag[1];
	cst_clst ( uargs, '|', "\0", 2, LLMXLN, cptr, &nuag, &ier );
	if  ( strlen(uag[0]) == 0 ) {
	    *iret = -2;
	    return;
        }
	else if ( strlen(uag[1]) == 0 )  {
	    cst_lstr ( glvl, &lenc, &ier );
            cst_ncpy ( uag[1], glvl, lenc, &ier );
        }

        /*
         *	Split uag[0] on &.  If more than one argument is specified,
         *	prompt a warning.
         */
        for ( i=0; i<MAXARG; i++ ) cptr[i] = uin[i];
	cst_clst ( uag [0], '&', "\0", MAXARG, LLMXLN, cptr, &nin, &ier );
	if ( nin > 1 ) *iret = +1;

        /*
         *	Set the list of levels.
         */
	dl_lvls ( cptr, nin, uag [1], &nlev, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute weighting factor. Use Log (P) for isobaric coordinate.
         */
	dg_qlyr ( uin [0], &lflag, &ier );

        /*
         *	Computer depth of the layer.
         */
	bot  = (float) ( _lyrdiag.lyrlvs1[ 0 ] );
	top1 = (float) ( _lyrdiag.lyrlvs1[ nlev-1 ] );
	top2 = (float) ( _lyrdiag.lyrlvs2[ nlev-1 ] );
	if ( lflag != 0 )
	    top = top2;
	else
	    top = top1;

        nval = 1;
        dg_iget ( "LVCORD", &nval, &lvcord, iret );
	if ( lvcord == 1 )
	    dp = fabs ( log ( top ) - log ( bot ) );
	else
	    dp = fabs ( top - bot );
	
	dp2 = dp * 2.;

        /*
         *	Compute Log (P).	
         */
	if  ( lvcord == 1 )  {
	    if ( lflag != 0 ) {
		for ( j = 0; j < nlev; j++ ) {
		    lp [ 0 ][ j ] = log ( (float) _lyrdiag.lyrlvs1[ j ] );
		    lp [ 1 ][ j ] = log ( (float) _lyrdiag.lyrlvs2[ j ] );
		}
            }
	    else { 
		for ( j = 0; j < nlev; j++ ) {
		    lp [ 0 ][ j ] = log ( (float) _lyrdiag.lyrlvs1[ j ] ) ;
		}
	    }
        }
	else {
	    for ( j = 0; j < nlev; j++ ) {
		lp [ 0 ][ j ] = _lyrdiag.lyrlvs1[ j ];
		lp [ 1 ][ j ] = _lyrdiag.lyrlvs2[ j ];
	    }
	}

        /*
         *	Compute the weight.
         */
	if ( lflag != 0 ) {
	    for ( i = 0; i < nlev; i++ ) 
		w [ i ] = fabs ( lp [ 1 ][ i ] - lp [ 0 ][ i ] ) / dp;
        }
	else {
	    for ( i = 0; i < nlev; i++ )  {
		if ( i == 0 )  {
		    top = lp [ 0 ][ i + 1 ];
		    bot = lp [ 0 ][ i ]; 
		}
		else if ( i == (nlev - 1) )  {
		    top = lp [ 0 ][ i ];
		    bot = lp [ 0 ][ i - 1 ];
		}
		else {
		    top = lp [ 0 ][ i + 1 ];
		    bot = lp [ 0 ][ i - 1 ];
		}
		w [ i ] = fabs ( top - bot ) / dp2;
	    }
	}

        /*
         *	Loop over all the levels in LYRLVS.
         */
	for ( ilev = 0; ilev < nlev; ilev++ ) {
	    ldlevl1 = _lyrdiag.lyrlvs1[ ilev ];
	    ldlevl2 = _lyrdiag.lyrlvs2[ ilev ];
            nval = 1;
            dg_iset ( "LDLEVL1", &nval, &ldlevl1, iret );
            dg_iset ( "LDLEVL2", &nval, &ldlevl2, iret );
	    dg_pfun ( uin [ 0 ], iret );
	    if ( *iret != 0 ) {
		er_wmsg ( "DG", iret, " ", &ier, 2,1 );
		*iret = -8;
		return;
	    } 
	    dg_driv ( &one, iret );
	    if ( *iret != 0 ) {
		er_wmsg ( "DG", iret, uin [0], &ier,2,strlen(uin[0]) );
		*iret = -9;
		return;
	    }

            /*
             *	    Retrieve the output grid from the stack.  Check that the
             *	    output is a scalar.
             */
	    dg_tops ( tname, &num, time1, time2, &level1, &level2, &ivcord,
	              pdum, iret );
            dg_getg ( &num, &grnum, &kxd, &kyd, &ksub1, &ksub2, iret );

	    for ( j = ksub1 - 1; j < ksub2; j++ ) {
		d1 = grnum[j];
		d2 = gs[j];
		if ( ERMISS ( d1 ) || ERMISS ( d2 ) )
		    gs[j] = RMISSD;
		else
		    gs[j] = gs[j] + grnum[j] * w [ilev];
	    }
	    dg_frig ( &num, &ier );
	}

        /*
         *	Set internal grid identifier.
         */
	dg_udig ( "LXX_", &ns, &zero, &_lyrdiag.lyrnid, stprm, iret );
	dg_esub ( &ns, &zero, &zero, &zero, &ier );
	if ( ier != 0 )  *iret = ier;

	return;
}
