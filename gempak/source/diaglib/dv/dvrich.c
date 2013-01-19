#include "dv.h"

void dv_rich  ( int *iret )
/************************************************************************
 * dv_rich								*
 *									*
 * This subroutine computes the Richardson stability number in a layer:	*
 *									*
 *     RICH ( V ) = GRAVTY * DZ * LDF (THTA) / 				*
 *                  [ LAV (THTA) * MAG ( VLDF (V) ) ** 2 ]		*
 *									*
 *                  Where: DZ = change in height across the layer	*
 *                            = -( RDGAS / GRAVTY ) * LAV (THTA) *	*
 *                              ( LAV (PRES) / 1000 ) ** KAPPA *	*
 *                              LDF (PRES) / LAV (PRES)			*
 *                              in THTA coordinates			*
 *									*
 * RICH generates a scalar grid.					*
 *									*
 * dv_rich ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * M. desJardins/GSFC	10/86	Added parameter statement for RKAPPA	*
 * G. Huffman/GSC	 9/88	New stack functions			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC  	 8/89	Subsetting				*
 * K. Brill/GSC		10/89	Subsetting				*
 * K. Brill/GSC		12/89	Compute dz from p,T when HGHT is missing*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             i, ier, kxd, kyd, ksub1, ksub2, nval, zero=0, tmp;
	int		nu, nv, nvldfu, nvldfv, nmag, idlun;

        int             ndz, ndp, npbar, ntbar, npav;
        float           *grdz, *grdp, *grpbar, *grtbar, *grpav;

        int             ndth, nath, nwsq, nrich;
        float           *grdth, *grath, *grwsq, *grrich;

        float           dp, tbar, pbar, avthta, cnst, dz, deltht, ath, dth;
        float           dwsq, pav, avtht;

	char	        gp[13], wname[13], gdum[13], pdum[13];
        char            time1[21], time2[21], errst[1024];
	int		level1, level2, ivcord;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Read the wind vector name, level, and vertical coordinate.
         *	Compute the wind shear in the layer and read the grid number
         *	of the result.
         */
	dg_topv  ( wname, &nu, &nv, time1, time2, &level1, &level2, 
                          &ivcord, pdum, iret );
	if  ( *iret != 0 )  return;
	dv_vldf  ( iret );
	if  ( *iret != 0 )  return;
	dg_topv  ( gdum, &nvldfu, &nvldfv, time1, time2, &level1,
                          &level2, &ivcord, pdum, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute the magnitude of the wind shear and square it (by
         *	reading the grid number of the result, putting another copy
         *	on the stack, and multiplying).  Read the grid number of the
         *	result.
         */
	dv_mag  ( iret );
	if  ( *iret != 0 )  return;

	dg_tops  ( gdum, &nmag, time1, time2, &level1, &level2, 
                          &ivcord, pdum, iret );
	if  ( *iret != 0 )  return;
	dg_puts  ( &nmag, iret );
	if  ( *iret != 0 )  return;
	df_mul  ( iret );
	if  ( *iret != 0 )  return;
	dg_tops  ( gdum, &nwsq, time1, time2, &level1, &level2,
                          &ivcord, pdum, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute a grid of delta height depending upon the coordinate
         *	system.
         */
	if  ( ivcord == 1 ) {

            /*
             *	    Pressure coordinate system.
             *	    Replace the top of the stack with HGHT (preserving in-line
             *	    parameters), compute LDF, and read the grid number of the
             *	    result.
             */
	    dg_rpls  ( "HGHT", &zero, iret );
	    if  ( *iret != 0 )  return;
	    df_ldf  ( iret );

	    if  ( *iret != 0 )  {

              /*
               *       Compute dz from p and T.
               */
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
	      dg_nxts ( &ndz, iret );
              if ( *iret != 0 ) return;

              dg_getg ( &ndp, &grdp, &kxd, &kyd, &ksub1, &ksub2, iret );
              dg_getg ( &ndz, &grdz, &kxd, &kyd, &ksub1, &ksub2, iret );
              dg_getg ( &ntbar, &grtbar, &kxd, &kyd, &ksub1, &ksub2, iret );
              dg_getg ( &npbar, &grpbar, &kxd, &kyd, &ksub1, &ksub2, iret );

	      for ( i = ksub1 - 1; i < ksub2; i++ ) {
	          dp   = grdp[i];
	          tbar = grtbar[i];
	          pbar = grpbar[i];
	          if ( ERMISS ( dp ) || ERMISS ( tbar ) || ERMISS ( pbar ) ) 
	            grdz[i] = RMISSD;
	          else
	            grdz[i] = -RKAP * tbar * dp / pbar;
	          
              }
          }
	  else {
	      dg_tops ( gdum, &ndz, time1, time2, &level1, &level2, 
                               &ivcord, pdum, iret );
	      if  ( *iret != 0 )  return;
	  }

        }
	else if  ( ivcord == 2 )  {

            /*
             *	    Isentropic coordinate system.
             *	    Compute the height differences as documented in the header.
             *	    The PRES operations preserve access to in-line parameters.
             */
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
             *	    Get a new grid number for the height field.
             */
	    dg_nxts ( &ndz, iret );
	    if  ( *iret != 0 )  return;

            dg_getg ( &ndp, &grdp, &kxd, &kyd, &ksub1, &ksub2, iret );
            dg_getg ( &ndz, &grdz, &kxd, &kyd, &ksub1, &ksub2, iret );
            dg_getg ( &npav, &grpav, &kxd, &kyd, &ksub1, &ksub2, iret );

	    for ( i = ksub1 - 1; i < ksub2; i++ ) {
		if ( ( G_DIFFT(grpav[i], 0.0F, GDIFFD) ) || 
     		     ERMISS ( grpav[i] ) || ERMISS ( grdp[i] ) )
		    grdz[i] = RMISSD;
		else {
		    dp  = grdp[i];
		    pav = grpav[i];
		    grdz[i] = cnst * pow ( ( pav / 1000. ), RKAPPA ) 
                                   * ( dp / pav );
		}
	    }

        }
	else if ( ivcord == 3 ) {

            /*
             *	    Height coordinate system.
             *	    Generate a (constant) grid of height difference.  No stack
             *	    operations occur.
             */
	    dg_nxts  ( &ndz, iret );
	    if  ( *iret != 0 )  return;

	    dz = level1 - level2;
	    dg_real  ( &dz, &ndz, iret );

        }
	else { 

            /*
             *	    "No" or unrecognized coordinate system.  This is an error.
             */
	    *iret = -24;
            tmp = -1;
	    dg_merr  ( "", "", "", &tmp, &tmp, &ivcord, errst, &ier );
            dg_cset ( "ERRST", errst, iret );
	    return;

        }

        /*
         *	Compute the average THTA and THTA difference in the layer
         *	depending upon the coordinate system.  Stack operations are
         *	designed to preserve access to in-line parameters.
         */
	if (( ivcord == 1 ) || ( ivcord == 3)) {

            /*
             *	    Pressure or height coordinate system.
             *	    Compute the delta THTA in the layer.
             */
	    dg_rpls  ( "THTA", &zero, iret );
	    if  ( *iret != 0 )  return;
	    df_ldf  ( iret );
	    if  ( *iret != 0 )  return;
	    dg_tops  ( gdum, &ndth, time1, time2, &level1, &level2, 
                              &ivcord, pdum, iret );
	    if  ( *iret != 0 )  return;

            /*
             *	    Compute the average THTA in the layer.
             */
	    dg_rpls  ( "THTA", &zero, iret );
	    if  ( *iret != 0 )  return;
	    df_lav  ( iret );
	    if  ( *iret != 0 )  return;
	    dg_tops  ( gdum, &nath, time1, time2, &level1, &level2,
                              &ivcord, pdum, iret );
	    if  ( *iret != 0 )  return;

        }
	else if ( ivcord == 2 ) {

            /*
             *	  Isentropic coordinate system.
             *	  Constant values are put into new grids (no stack operations).
             *	  Compute the average THTA in the layer.
             */
	    dg_nxts ( &nath, iret );
	    if ( *iret != 0 )  return;

	    avtht  = (level1 + level2) / 2.;
	    dg_real ( &avtht, &nath, iret );

            /*
             *	    Compute the THTA difference in the layer.
             */
	    dg_nxts ( &ndth, iret );
	    if ( *iret != 0 )  return;

	    deltht =  level1 - level2;
	    dg_real ( &deltht, &ndth, iret );

        }

        /*
         *	No test for other values of IVCORD was needed because they
         *	were kicked out in the IF for DZ.  Get a new grid for the
         *	Richardson number and compute it.
         */
	dg_nxts ( &nrich, iret );
	if ( *iret != 0 )  return;

        dg_getg ( &ndz, &grdz, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &ndth, &grdth, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nath, &grath, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nwsq, &grwsq, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nrich, &grrich, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    ath  = grath[i];
	    dth  = grdth[i];
	    dz   = grdz[i];
	    dwsq = grwsq[i];

	    if ( ERMISS (ath ) || G_DIFFT(ath,  0.0F, GDIFFD) || 
     	 	 ERMISS (dwsq) || G_DIFFT(dwsq, 0.0F, GDIFFD) ||
     	 	 ERMISS (dth ) || ERMISS (dz ) ) 
		grrich[i] = RMISSD;
	    else
		grrich[i] = ( GRAVTY * dz / ath) * ( dth / dwsq );
	    
	}

        /*
         *	Make a name of the form 'RICH'//V and update header;
         *	update the stack.
         */
	dg_mnam ( "RICH", wname, "", gp, iret );
        nval = 1;
        dg_iget ( "IDLUN", &nval, &idlun, iret);
	dg_upsg ( time1, time2, &level1, &level2, &ivcord, &idlun, gp, 
                         &nrich, iret );
	dg_rpls ( "", &nrich, iret );
	dg_esub  ( &nrich, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
