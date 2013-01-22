#include "dv.h"
#include "../dg/dg.h"

void dv_pvr3 ( int *iret )
/************************************************************************
 * dv_pvr3 								*
 *									*
 * This subroutine computes the 3-D Potential Vorticity for a level:    *
 *     PVR3 ( S, V ) = - CONSTANT * 3D-VOR ( V ) * 3D-GRAD ( S )      	*
 *								  	*
 *		    Where:   S can be THTA, THTE, THWK, or THES  	*
 *                           V has to be GEO or WND                     *
 *                           CONSTANT = GRAVTY in PRES coordinates      *
 *                           CONSTANT = 1/DDEN (density) for HGHT.      *
 *								  	*
 * PVR3 generates a scalar grid on PRES or HGHT vertical coordinates.  	*
 * Reference: McCann (1995; WAF).                                       *
 *									*
 * dv_pvr3 ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 *									*
 * Log:									*
 * C. Melick/SPC	08/11	Created					*
 ************************************************************************/
{
    int		i, ier, nval, kxd, kyd, kxyd, ksub1, ksub2;
    int         tmp, idlun, j, itmp, ilvl, npvr3, num1, ncnst; 
    int         navor, ntrm1, ntrm2, ntrm3, nddxthta, nddythta;
    int         lev1[LLMXLV], lev2[LLMXLV], nlvl, nthta, nthta1;
    int         numu,numv,numu1,numv1,numu2,numv2, nthta2, minus1;
    int		level1, level2, ivcord;
    const int   one=1, zero=0;

    float       *gthta, *gthta1, *gthta2, *grcnst, *grnum1;
    float       *gu1,*gv1,*gu2,*gv2,cnst,dens;
    float       *gavor,*gddxthta,*gddythta,dvc;
    float       *gtrm1,*gtrm2,*gtrm3,*gpvr3;

    char	gp[13], gfunc[13], parm[13], time1[21], time2[21];
    char        gname[13],gfuncv[13],gnamev[13],errst[1024];
/*------------------------------------------------------------------------*/
    *iret  = 0;
    minus1 = -1;
    dg_ssub ( iret );

     /*
     * Get time for grid calculations
     */
    dg_tops ( gfunc, &nthta, time1, time2, &level1, &level2, &ivcord, gname,
              iret ); 
    if  ( *iret != 0 )  return;  

    if  ( strcmp ( gfunc, " " ) == 0 ) {
	sprintf ( gfunc, gname );
    } 

    /*
     * Read the grid from the file.
     */
    dg_rgrd ( time1, time2, &level1, &level2, &ivcord, gfunc,
	      &nthta, iret );
    if  ( *iret != 0 )  return;  

    /*
     * Output for PVR3 only valid at single levels
     */
    if  ( level2 != -1  )  {
         *iret = -75;
         dg_merr ( "", "", "", &level1, &level2,  &minus1,
		   _dgerr.errst, &ier );
        return;
    }

    /*
     * PRES (ivcord = 1) and HGHT (ivcord = 3) are the only
     * valid vertical coordinates
     */
    if  ( ( ivcord == 1 ) || ( ivcord == 3 ) )  {  

	nval = LLMXLV;

	/*
	 * Need vertical levels in order to calculate vertical differences
	 */
	dgc_glev ( &one, time1, time2, &ivcord, &nval, lev1, lev2, &nlvl,
                   iret );
	if  ( *iret != 0 )  return;  

	/*
	 * Only want levels, no layers
	 */
	i = 0;
	while ( i < nlvl ) {
	    if ( lev2[i] != -1 ) {
		if ( i < nlvl - 1 ) {
		    lev1[i] = lev1[nlvl - 1];
		    lev2[i] = lev2[nlvl - 1];
		}
		nlvl--;
	    }
	    else {
		i++;
	    }
	}

	/*
	 * Sort levels
	 */
	for ( i=0; i < nlvl - 1; i++ ) {
	    for ( j = i+1; j < nlvl; j++) {
		if ( ( lev1[i] < lev1[j] && ivcord == 1 ) ||
		     ( lev1[i] > lev1[j] && ivcord == 3 ) )  {
		    itmp = lev1[i];
		    lev1[i] = lev1[j];
		    lev1[j] = itmp;
		    /* Note: don't have to swap lev2 which is always -1 */
		}
	     }
	 }
       
	for ( i=0; i < nlvl; i++ ) {
	    if ( level1 == lev1[i] ) {
		ilvl=i;
	    }
	}

	if ( ilvl == 0 ) {
	    dvc=(float)(lev1[1]-lev1[0]);
	    _dginpt.ldlevl2 = lev1[1];
	}
       	else if (ilvl == (nlvl-1)) {
	    dvc=(float)(lev1[nlvl-1]-lev1[nlvl-2]);
	    _dginpt.ldlevl1 = lev1[nlvl-2];
	    _dginpt.ldlevl2 = lev1[nlvl-1];
	}
       	else {
	    dvc=(float)(lev1[ilvl+1]-lev1[ilvl-1]);
	    _dginpt.ldlevl1 = lev1[ilvl-1];
	    _dginpt.ldlevl2 = lev1[ilvl+1];
	}

	/*
	 * Get the grid numbers for THETA at the top and bottom.
	 */
	dg_getl ( &nthta1, &nthta2, iret );
	if ( *iret != 0 ) return;

	dg_topv ( gfuncv, &numu, &numv, time1, time2,
	       	  &level1, &level2, &ivcord, gnamev,iret);
	if ( *iret != 0 ) return;

	if  ( strcmp ( gfuncv, " " ) == 0 ) {
	    sprintf ( gfuncv, gnamev );
	} 

	/*
	 * Get the grid numbers for the winds at the top and bottom. 
	 * WND and GEO are the only possible selections.
	 */
	if  ( ( strcmp ( gfuncv, "WND" ) == 0 ) || 
	      (	strcmp ( gfuncv, "GEO" ) == 0 ) ) {
	    dg_gtvl ( &numu1, &numv1, &numu2, &numv2, iret );
	    if ( *iret != 0 ) return;
	} else {
	    *iret = -76;
	    strcpy ( _dgerr.errst, gfuncv );
	    return;
	}

	dg_getg ( &nthta1, &gthta1, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &nthta2, &gthta2, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &numu1, &gu1, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &numv1, &gv1, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &numu2, &gu2, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &numv2, &gv2, &kxd, &kyd, &ksub1, &ksub2, iret );

        /*
         * Put THETA back on the stack for the level originally 
         * selected in order to calculate horizontal derivatives. 
         */

	_dginpt.ldlevl1 = lev1[ilvl];
	_dginpt.ldlevl2 = -1;
	dg_puts ( &nthta1, iret);
	dg_rpls ( gfunc, &zero, &ier);

	df_ddx  ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &nddxthta, iret );
	if ( *iret != 0 ) return;

	dg_puts ( &nthta1, iret);
	df_ddy  ( iret );
	if ( *iret != 0 ) return;

	dg_gets ( &nddythta, iret );
	if ( *iret != 0 ) return;

        /*
         * Now, put GEO or WND back on the stack for the level originally 
         * selected in order to calculate absolute vorticity. 
         */

	_dggrid.leveld1[numu-1] = lev1[ilvl];
	_dggrid.leveld2[numu-1] = lev1[ilvl];
	_dggrid.leveld1[numv-1] = lev1[ilvl];
	_dggrid.leveld2[numv-1] = lev1[ilvl];
	dg_putv ( &numu1, &numv1, iret);
	if ( *iret != 0 ) return; 
	dg_rplv ( gfuncv, &zero, &zero, &ier);

	dv_avor  ( iret );
	if ( *iret != 0 ) return;
	dg_gets ( &navor, iret );
	if ( *iret != 0 ) return;

	dg_getg ( &navor, &gavor, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &nthta, &gthta, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &nddxthta, &gddxthta, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &nddythta, &gddythta, &kxd, &kyd, &ksub1, &ksub2, iret );
  
	/*
	 * Compute the numerical "constant" grid for the vertical coordinate.
	 */

	dg_nxts  ( &ncnst, iret );
	if  ( *iret != 0 )  return;
	dg_getg ( &ncnst, &grcnst, &kxd, &kyd, &ksub1, &ksub2, iret);

	if  ( ivcord == 1 )  {
	    /* PRES - convert to Pascal and use gravity. */
	    dvc = 100. * dvc;
	    cnst = GRAVTY;
	    dg_real  ( &cnst, &ncnst, iret );
	}
	else {
	    /* HGHT - get temperature to calculate density.  */
	    dg_temp  ( time1, time2, &level1, &level2, &ivcord, 
                       "TMPC", &ncnst, iret );
	    if  ( *iret != 0 )  return;

	    dg_getg ( &num1, &grnum1, &kxd, &kyd, &ksub1, &ksub2, iret );

	    kxyd = kxd * kyd;
	    pd_dden  ( grnum1, grcnst, &kxyd, grcnst, &ier );

	    for ( i = ksub1 - 1; i < ksub2; i++ ) {
		dens = grcnst[i];
		if  ( ERMISS ( dens ) || G_DIFFT( dens, 0.0F, GDIFFD) ) {
		    grcnst[i] = RMISSD;
		}
		else {
		    grcnst[i] = - 1. / dens;
		}
	    }
	}
    }
    else {
	*iret = -24;
	tmp = -1;
	dg_merr  ( "", "", "", &tmp, &tmp, &ivcord, errst, &ier );
	dg_cset ( "ERRST", errst, &ier );
	return;
    }  

    /*
     * Get four new grids for the computational work.
     */
    if  ( *iret == 0 )  dg_nxts  ( &ntrm1, iret );
    if  ( *iret != 0 )  return;
    if  ( *iret == 0 )  dg_nxts  ( &ntrm2, iret );
    if  ( *iret != 0 )  return;
    if  ( *iret == 0 )  dg_nxts  ( &ntrm3, iret );
    if  ( *iret != 0 )  return;
    if  ( *iret == 0 )  dg_nxts  ( &npvr3, iret );
    if  ( *iret != 0 )  return;

    dg_getg ( &ntrm1, &gtrm1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &ntrm2, &gtrm2, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &ntrm3, &gtrm3, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &npvr3, &gpvr3, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Compute the final result.
     */
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	gtrm1[i] = gddxthta[i] * ((gv2[i] - gv1[i]) / dvc ); 
	gtrm2[i] = gddythta[i] * ((gu2[i] - gu1[i]) / dvc ); 
	gtrm3[i] = gavor[i] * ((gthta2[i] - gthta1[i]) / dvc ); 
	if ( ERMISS ( gtrm1[i] ) ||
	     ERMISS ( gtrm2[i] ) ||
	     ERMISS ( gtrm3[i] ) ||
	     ERMISS ( grcnst[i]) ) {
	    gpvr3[i] = RMISSD;
	}
       	else {
	    gpvr3[i] = grcnst[i] * ( gtrm1[i]-gtrm2[i]-gtrm3[i] ) ;   
	}
    }

    /*
     * Make a name of the form 'PVR3' // V and update header; update
     * stack.
     */
    dg_mnam  ( "PVR3", gfunc, "", gp, iret );
    nval = 1;
    dg_iget ( "IDLUN", &nval, &idlun, iret);
    level1=lev1[ilvl];
    level2=-1;
    dg_upsg  ( time1, time2, &level1, &level2, &ivcord, &idlun, 
		gp, &npvr3, iret );
    dg_puts  ( &npvr3, iret );
    dg_esub  ( &npvr3, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
