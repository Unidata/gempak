#include "df.h"

void df_bvsq ( int *iret )
/************************************************************************
 * df_bvsq								*
 *									*
 * This subroutine computes the square of the Brunt-Vaisala frequency	*
 * in a layer:								*
 *									*
 *     	BVSQ ( THTA ) = [ GRAVTY * LDF (THTA) ] / [ LAV (THTA) * DZ ]	*
 *									*
 * 	DZ = change in height across the layer				*
 *    	= -( RDGAS / GRAVTY ) * LAV (THTA) *				*
 *      ( LAV (PRES) / 1000 ) ** KAPPA * LDF (PRES) / LAV (PRES)	*
 *      in THTA coordinates						*
 *									*
 * df_bvsq ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * D. McCann/NSSFC	12/94	Created from RICH			*
 * K. Tyle/GSC		11/95   Added tname				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * D. McCann/NSSFC	 9/96	Modified to allow for different THTAs	*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		10/05	Recoded from Fortran			*
 ************************************************************************/
{
    char gp[13], gdum[13], pdum[13], time1[21], time2[21], tname[13],
         errst[128];
    int kxd, kyd, ksub1, ksub2;
    int nt1, nt2, level1, level2, ivcord, npbar, ntbar, ndp, ndz, npav,
        ndth, nath, nbvsq, idlun;
    int zero, one, minus1, fidx, cidx, ier;
    float *gndp, *gntbar, *gnpbar, *gndz, *gnpav, *gnt1, *gnt2, *gnath,
          *gndth, *gnbvsq;
    float dp, tbar, pbar, avthta, cnst, dz, avtht, deltht, pav, t1, t2,
          ath, dth;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    one = 1;
    minus1 = -1;

    dg_ssub ( iret );

    /*
     * Read the temperature, levels, and vertical coordinate.
     */
    dg_tops ( tname, &nt1, time1, time2, &level1, &level2, &ivcord, pdum,
              iret );
    if ( *iret != 0 ) return;

    dg_getl ( &nt1, &nt2, iret );
    if ( *iret != 0 ) return;

    dg_puts ( &nt1, iret );
    if ( *iret != 0 ) return;

    /*
     * Compute a grid of DZ depending upon coordinate system.
     *
     * If pressure or sigma coordinate system, replace top of stack 
     * with HGHT.
     */
    if ( ( ivcord == 1 ) || ( ivcord == 4 ) ) {
	dg_rpls ( "HGHT", &zero, iret );
    	if ( *iret != 0 ) return;

	/*
	 * Compute DZ if height grids exist.
	 */
	df_ldf ( iret );

	/*
	 * If no height grids, compute DZ from p and T.
	 */
	if ( *iret != 0 ) {
	    dg_rpls ( "PRES", &zero, iret );
	    if ( *iret != 0 ) return;

	    df_lav ( iret );
	    if ( *iret != 0 ) return;

	    dg_tops ( gdum, &npbar, time1, time2, &level1, &level2,
		      &ivcord, pdum, iret );
	    if ( *iret != 0 ) return;

	    dg_rpls ( "TMPK", &zero, iret );
	    if ( *iret != 0 ) return;

	    df_lav ( iret );
	    if ( *iret != 0 ) return;

	    dg_tops ( gdum, &ntbar, time1, time2, &level1, &level2,
		      &ivcord, pdum, iret );
	    if ( *iret != 0 ) return;

	    dg_rpls ( "PRES", &zero, iret );
	    if ( *iret != 0 ) return;

	    df_ldf ( iret );
	    if ( *iret != 0 ) return;

	    dg_tops ( gdum, &ndp, time1, time2, &level1, &level2,
		      &ivcord, pdum, iret );
	    if ( *iret != 0 ) return;

	    /*
	     * Get a new grid number for DZ.
	     */
	    dg_nxts ( &ndz, iret );
	    if ( *iret != 0 ) return;

	    /*
	     * Grid number to grid.
	     */
	    dg_getg ( &ndp,   &gndp,   &kxd, &kyd, &ksub1, &ksub2, iret );
	    dg_getg ( &ntbar, &gntbar, &kxd, &kyd, &ksub1, &ksub2, iret );
	    dg_getg ( &npbar, &gnpbar, &kxd, &kyd, &ksub1, &ksub2, iret );
	    dg_getg ( &ndz,   &gndz,   &kxd, &kyd, &ksub1, &ksub2, iret );

            for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
		cidx = fidx - 1;
	        dp = gndp[cidx];
	        tbar = gntbar[cidx];
	        pbar = gnpbar[cidx];
	        if ( ERMISS ( dp ) || ERMISS ( tbar ) || ERMISS ( pbar ) ) {
		    gndz[cidx] = RMISSD;
		} else {
		    gndz[cidx] = -RKAP * tbar * dp / pbar;
		}
	    }
	} else {
	    dg_tops ( gdum, &ndz, time1, time2, &level1, &level2,
	              &ivcord, pdum, iret );
	    if ( *iret != 0 ) return;
	}

    /*
     * If isentropic coordinate system, compute DZ.
     */     
    } else if ( ivcord == 2 ) {
        avthta = (level1 + level2 ) / 2.;
        cnst   = -avthta * RKAP;

        dg_rpls ( "PRES", &zero, iret );
        if ( *iret != 0 ) return;

        df_lav ( iret );
        if ( *iret != 0 ) return;

        dg_tops ( gdum, &npav, time1, time2, &level1, &level2, &ivcord,
	          pdum, iret );
        if ( *iret != 0 ) return;

        dg_rpls ( "PRES", &zero, iret );
        if ( *iret != 0 ) return;

        df_ldf ( iret );
        if ( *iret != 0 ) return;

        dg_tops ( gdum, &ndp, time1, time2, &level1, &level2, &ivcord,
	          pdum, iret );
        if ( *iret != 0 ) return;

	/*
	 * Get a new grid number for DZ.
	 */
	dg_nxts ( &ndz, iret );
	if ( *iret != 0 ) return;

	/*
	 * Grid number to grid.
	 */
	dg_getg ( &npav, &gnpav, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &ndp,  &gndp,  &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &ndz,  &gndz,  &kxd, &kyd, &ksub1, &ksub2, iret );

	/*
	 * Compute DZ.
	 */
        for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
	    cidx = fidx - 1;
	    dp  = gndp[cidx];
	    pav = gnpav[cidx];
	    if ( ( G_DIFFT(pav, 0.0F, GDIFFD) ) || ERMISS ( pav ) || ERMISS ( dp ) ) {
	        gndz[cidx] = RMISSD;
	    } else {
	        gndz[cidx] = cnst * pow ( (double)pav / 1000.0, RKAPPA ) * (dp/pav);
	    }
	}

    /*
     * If height coordinate system, generate a (constant) grid of DZ.
     */
    } else if ( ivcord == 3 ) {
        dg_nxts ( &ndz, iret );
        if ( *iret != 0 ) return;

        dz = level1 - level2;
        dg_real ( &dz, &ndz, iret );

    /*
     * If unrecognized coordinate system, signal an error.
     */
    } else {
        *iret = -24;
        dg_merr ( "", "", "", &minus1, &minus1, &ivcord, errst, &ier );
	dg_cset ( "ERRST", errst, &ier );
 	return;
    }

    /*
     * Compute avg. THTA and THTA diff. in layer depending on coord. sys.
     *
     * If pressure, height, or sigma coordinate system, compute
     * delta THTA in the layer.
     */
    if ( ( ivcord == 1 ) || ( ivcord == 3) || ( ivcord == 4 ) ) {
	dg_nxts ( &ndth, iret );
	if ( *iret != 0 ) return;
	dg_nxts ( &nath, iret );
	if ( *iret != 0 ) return;

	/*
	 * Grid number to grid.
	 */
	dg_getg ( &nt1, &gnt1, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &nt2, &gnt2, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &nath, &gnath, &kxd, &kyd, &ksub1, &ksub2, iret );
	dg_getg ( &ndth, &gndth, &kxd, &kyd, &ksub1, &ksub2, iret );

        for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
	    cidx = fidx - 1;
	    t1 = gnt1[cidx];
	    t2 = gnt2[cidx];
	    if ( ERMISS (t1 ) || ERMISS ( t2 ) ) {
	        gndth[cidx] = RMISSD;
	        gnath[cidx] = RMISSD;
	    } else {
	        gndth[cidx] = t1 - t2;
	        gnath[cidx] = ( t1 + t2 ) / 2.0;
	    }
	}

    /*
     * If isentropic coordinate system, constant values are put into
     * new grids.
     */
    } else if ( ivcord == 2 ) {
	dg_nxts ( &nath, iret );
	if ( *iret != 0 ) return;

	/*
	 * Compute average THTA in layer.
	 */
	avtht = ( level1 + level2 ) / 2.;
	dg_real ( &avtht, &nath, iret );

	/*
	 * Compute THTA difference in layer.
	 */
	dg_nxts ( &ndth, iret );
	if ( *iret != 0 ) return;

	deltht = level1 - level2;
	dg_real ( &deltht, &ndth, iret );
    }

    /*
     * Get a new grid for BVSQ and compute it.
     */
    dg_nxts ( &nbvsq, iret );
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &nath,  &gnath,  &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &ndth,  &gndth,  &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &ndz,   &gndz,   &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nbvsq, &gnbvsq, &kxd, &kyd, &ksub1, &ksub2, iret );

    for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
        cidx = fidx - 1;
        ath = gnath[cidx];
	dth = gndth[cidx];
	dz  = gndz[cidx];

	if ( ERMISS ( ath ) || G_DIFFT(ath, 0.0F, GDIFFD) || ERMISS ( dth ) || 
             ERMISS ( dz ) || G_DIFFT(dz, 0.0F, GDIFFD) ) {
	    gnbvsq[cidx] = RMISSD;
    	} else {
	    gnbvsq[cidx] =  ( GRAVTY / dz / ath) * ( dth );
	}
    }

    /*
     * Make a name of the form 'BVSQ' and update header.
     */
    dg_mnam ( "BVSQ", tname, "", gp, iret );

    /*
     * Update stack.
     */
    dg_iget ( "IDLUN", &one, &idlun, iret );
    dg_upsg ( time1, time2, &level1, &level2, &ivcord, &idlun, gp,
              &nbvsq, iret );
    dg_rpls ( "", &nbvsq, iret );
    dg_esub ( &nbvsq, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
