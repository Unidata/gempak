#include "dv.h"

void dv_wshr  ( int *iret )
/************************************************************************
 * dv_wshr								*
 *									*
 * This subroutine computes the magnitude of the vertical wind shear in *
 * a layer.  The following equation is used:				*
 *									*
 *     WSHR ( V ) = MAG ( VLDF ( V ) ) / DZ 				*
 *									*
 *     DZ = change in height across the layer				*
 *        = -( RDGAS / GRAVTY ) * LAV ( THTA ) *			*
 *          ( LAV (PRES) / 1000 ) ** KAPPA *				*
 *          LDF (PRES) / LAV (PRES)					*
 *          in THTA coordinates						*
 *									*
 * WSHR can be evaluated in PRES, THTA, or HGHT vertical coordinate.	*
 *									*
 * dv_wshr ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * D. McCann/NSSFC	12/94	Created from RICH			*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96	Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Trnaslation from Fortran                *
 ************************************************************************/
{
        const int       zero = 0;
        int             i, ier, nval, kxd, kyd, ksub1, ksub2, tmp;

	int		nu, nv, nvldfu, nvldfv, nmag;
        int             ndp, ndz, npbar, ntbar, npav, nwshr;
        float           *grdp, *grdz, *grpbar, *grtbar, *grpav, *grwshr;
        float           *grmag;
 
	char		gp[13], wname[13], gdum[13], pdum[13];
        char            time1[21], time2[21], errst[61];
	int		level1, level2, ivcord, idlun;
        float           dp, tbar, pbar, avthta, cnst, dz, pav, dmag;

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Read the wind vector name, level, and vertical coordinate.
         */
	dg_topv  ( wname, &nu, &nv, time1, time2, &level1, &level2, 
                          &ivcord, pdum, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute the wind vector difference in the layer.
         */
	dv_vldf  ( iret );
	if  ( *iret != 0 )  return;

        /*
         *	Read the grid number of the result.
         */
	dg_topv  ( gdum, &nvldfu, &nvldfv, time1, time2, &level1, 
                          &level2, &ivcord, pdum, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute the magnitude of the wind vector difference.
         */
	dv_mag  ( iret );
	if  ( *iret != 0 )  return;

	dg_tops  ( gdum, &nmag, time1, time2, &level1, &level2, 
                          &ivcord, pdum, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute grid of DZ depending upon coordinate system.
         *
         *	If pressure or sigma coordinate system, replace top of stack
         *	with HGHT.
         */
	if  ( ivcord == 1 || ivcord == 4 ) {
	    dg_rpls  ( "HGHT", &zero, iret );
	    if  ( *iret != 0 )  return;

            /*
             *	    Compute DZ if height grids exist.
             */
	    df_ldf  ( iret );

            /*
             * 	    If no height grids, compute DZ from p and T.
             */
	    if  ( *iret != 0 )  {
	    	dg_rpls ( "PRES", &zero, iret );
	    	if ( *iret != 0 ) return;

	    	df_lav ( iret );
	    	if ( *iret != 0 ) return;

	    	dg_tops  ( gdum, &npbar, time1, time2, &level1, &level2,
                                  &ivcord, pdum, iret );
	    	if ( *iret != 0 ) return;

	    	dg_rpls ( "TMPK", &zero, iret );
	    	if ( *iret != 0 ) return;

	    	df_lav ( iret );
	    	if ( *iret != 0 ) return;

	    	dg_tops  ( gdum, &ntbar, time1, time2, &level1, &level2,
                                  &ivcord, pdum, iret );
            	if ( *iret != 0 ) return;

	    	dg_rpls ( "PRES", &zero, iret );
	    	if ( *iret != 0 ) return;

	    	df_ldf ( iret );
	    	if ( *iret != 0 ) return;

	    	dg_tops  ( gdum, &ndp, time1, time2, &level1, &level2,
                                  &ivcord, pdum, iret );
	    	if ( *iret != 0 ) return;

                /*
                 *	Get a new grid number for DZ.
                 */ 
	    	dg_nxts ( &ndz, iret );
	    	if ( *iret != 0 ) return;

                dg_getg ( &ndp, &grdp, &kxd, &kyd, &ksub1, &ksub2, iret );
                dg_getg ( &ndz, &grdz, &kxd, &kyd, &ksub1, &ksub2, iret );
                dg_getg ( &ntbar, &grtbar, &kxd, &kyd, &ksub1, &ksub2, iret );
                dg_getg ( &npbar, &grpbar, &kxd, &kyd, &ksub1, &ksub2, iret );

	    	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	            dp = grdp[i];
	            tbar = grtbar[i];
	            pbar = grpbar[i];
	            if ( ERMISS ( dp ) || ERMISS ( tbar ) || ERMISS ( pbar ) )
	            	grdz[i] = RMISSD;
		    else
	                grdz[i] = - RKAP * tbar * dp / pbar;
		    
	    	}
            }
	    else {
		dg_tops ( gdum, &ndz, time1, time2, &level1, 
                                 &level2, &ivcord, pdum, iret );
		if  ( *iret != 0 )  return;
	    }

        }
        /*
         *	if isentropic coordinate system, compute DZ.
         */
	else if  ( ivcord == 2 )  {
	    avthta = (level1 + level2) / 2.;
	    cnst   = -avthta * RKAP;

	    dg_rpls  ( "PRES", &zero, iret );
	    if  ( *iret != 0 )  return;

	    df_lav  ( iret );
	    if  ( *iret != 0 )  return;

	    dg_tops  ( gdum, &npav, time1, time2, &level1, &level2, 
                              &ivcord, pdum, iret );
	    if  ( *iret != 0 )  return;

	    dg_rpls  ( "PRES", &zero, iret );
	    if  ( *iret != 0 )  return;

	    df_ldf  ( iret );
	    if  ( *iret != 0 )  return;

	    dg_tops  ( gdum, &ndp, time1, time2, &level1, &level2, 
                              &ivcord, pdum, iret );
	    if  ( *iret != 0 )  return;

            /*
             *	    Get a grid number for DZ .
             */
	    dg_nxts ( &ndz, iret );
	    if  ( *iret != 0 )  return;

            /*
             *	    Compute DZ.
             */
            dg_getg ( &ndp, &grdp, &kxd, &kyd, &ksub1, &ksub2, iret );
            dg_getg ( &ndz, &grdz, &kxd, &kyd, &ksub1, &ksub2, iret );
            dg_getg ( &npav, &grpav, &kxd, &kyd, &ksub1, &ksub2, iret );

	    for ( i = ksub1 - 1; i < ksub2; i++ ) {
		if ( ( G_DIFFT(grpav[i], 0.0F, GDIFFD) ) || ERMISS ( grpav[i] ) || 
     		     ERMISS ( grdp[i] ) ) 
		    grdz[i] = RMISSD;
		else {
		    dp  = grdp[i];
		    pav = grpav[i];
		    grdz[i] = cnst * pow( ((double)pav /1000.0), RKAPPA) * ( dp / pav );
		}
	    }

        }
        /*
         *	If height coordinate system, generate a (constant) grid of DZ.
         */
	else if ( ivcord == 3 ) {
	    dg_nxts  ( &ndz, iret );
	    if  ( *iret != 0 )  return;

	    dz = level1 - level2;
	    dg_real  ( &dz, &ndz, iret );

        }
        /*
         *	If unrecognized coordinate system, signal an error.
         */
	else { 
	    *iret = -24;
            tmp = -1;
	    dg_merr  ( "", "", "", &tmp, &tmp, &ivcord, errst, &ier );
            dg_cset ( "ERRST", errst, &ier );
	    return;

        }

        /*
         *	Get a new grid for the wind shear and compute it.
         */
	dg_nxts ( &nwshr, iret );
	if ( *iret != 0 )  return;

        dg_getg ( &ndz, &grdz, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nmag, &grmag, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nwshr, &grwshr, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    dz   = grdz[i];
	    if ( dz < 0 ) dz = -dz;
	    dmag = grmag[i];

	    if ( ERMISS (dmag) || ERMISS ( dz ) || G_DIFFT(dz, 0.0F, GDIFFD) )
		grwshr[i] = RMISSD;
	    else
		grwshr[i] = dmag / dz;
	    
	}

        /*
         *	Make a name of the form 'WSHR'//V and update header.
         */
	dg_mnam ( "WSHR", wname, "", gp, iret );
        nval = 1;
        dg_iget ( "IDLUN", &nval, &idlun, &ier );
	dg_upsg ( time1, time2, &level1, &level2, &ivcord, &idlun, 
                         gp, &nwshr, iret );

        /*
         *	Update stack.
         */
	dg_rpls ( "", &nwshr, iret );
	dg_esub  ( &nwshr, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
