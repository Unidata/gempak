#include "df.h"

void df_stab ( int *iret )
/************************************************************************
 * df_stab								*
 *									*
 * This subroutine computes STAB, the thermodynamic stability within a	*
 * layer (lapse rate) in degrees / km:					*
 *									*
 *     STAB ( TMPC ) = LDF ( TMPC ) / DZ				*
 *									*
 *                   Where: DZ = change in height across the layer	*
 *                             = -( RDGAS / GRAVTY ) * LAV (THTA) *	*
 *                               ( LAV (PRES) / 1000 ) ** KAPPA *	*
 *                               LDF (PRES) / LAV (PRES)		*
 *                               in THTA coordinates			*
 *									*
 * df_stab ( iret )							*
 *									*
 * Output parameters:							*
 *	IRET		INTEGER		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * G. Huffman/GSC	 8/88	Updated stack operations		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC		 8/89	Subsetting				*
 * K. Brill/GSC		10/89	Subsetting				*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    char time1[21], time2[21], parm[13], gfunc[13], errst[128];
    int level1, level2, ivcord, ndt, ndz, npav, ndp, num;
    int kx, ky, ksub1, ksub2, i, im1, zero, minus1, ier;
    float avthta, cnst, dp, pav, dz, dt;
    float *gndt, *gndz, *gnpav, *gndp, *gnum;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    minus1 = -1;

    dg_ssub ( iret );

    /*
     * Compute the temperature difference in the layer, get the
     * grid information.
     */
    df_ldf ( iret );
    if ( *iret != 0 ) return;
    dg_tops ( gfunc, &ndt, time1, time2, &level1, &level2, &ivcord,
        parm, iret );
    if ( *iret != 0 ) return;

    /*
     * Compute the height difference in the layer depending upon the
     * coordinate system.  In all cases of stack manipulation, the new
     * stack item overwrites an unneeded field, and has access to
     * the correct in-line flags.  
     */
    if ( ivcord == 1 ) {
	/*
	 * Pressure coordinate system.  Put HGHT on the stack,
	 * find the layer difference and get the pointer.
	 */
	dg_rpls ( "HGHT", &zero, iret );
	if ( *iret != 0 ) return;
	df_ldf ( iret );
	if ( *iret != 0 ) return;
	dg_tops ( gfunc, &ndz, time1, time2, &level1, &level2, &ivcord,
	    parm, iret );
	if ( *iret != 0 ) return;
    } else if ( ivcord == 2 ) {
	/*
	 * Isentropic coordinate system.
	 */
	avthta = ( level1 + level2 ) / 2.;
	cnst   = -avthta * RKAP;

	/*
	 * Get the layer-averaged pressure and its pointer.
	 * Then get the layer-difference pressure and its pointer.
	 */
	dg_rpls ( "PRES", &zero, iret );
	if ( *iret != 0 ) return;
	df_lav ( iret );
	if ( *iret != 0 ) return;
	dg_tops ( gfunc, &npav, time1, time2, &level1, &level2,
	    &ivcord, parm, iret );

	dg_rpls ( "PRES", &zero, iret );
	if ( *iret != 0 ) return;
	df_ldf ( iret );
	if ( *iret != 0 ) return;
	dg_tops ( gfunc, &ndp, time1, time2, &level1, &level2,
	    &ivcord, parm, iret );
	if ( *iret != 0 ) return;

	/*
	 * Get a new grid number for the height field.
	 */
	dg_nxts ( &ndz, iret );
	if ( *iret != 0 ) return;

	dg_getg ( &npav, &gnpav, &kx, &ky, &ksub1, &ksub2, iret );
	dg_getg ( &ndp,  &gndp,  &kx, &ky, &ksub1, &ksub2, iret );
	dg_getg ( &ndz,  &gndz,  &kx, &ky, &ksub1, &ksub2, iret );

	for ( i = ksub1; i <= ksub2; i++ ) {
	    im1 = i - 1;
	    if ( ( G_DIFFT(gnpav[im1], 0.0F, GDIFFD) ) ||
	  	ERMISS ( gnpav[im1] ) || ERMISS  ( gndp[im1] ) ) {
		gndz[im1] = RMISSD;
	    } else {
		dp  = gndp[im1];
		pav = gnpav[im1];
		gndz[im1] = cnst * pow ( ((double)pav/1000.0), RKAPPA ) * (dp/pav);
	    }
	}
    } else if ( ivcord == 3 ) {
	/*
	 * Height coordinate system.
	 * Get a new grid number for the height field.
	 */
	dg_nxts ( &ndz, iret );
	if ( *iret != 0 ) return;

	/*
	 * Compute the height difference of the vertical coordinate
	 */
	dz = level1 - level2;
	dg_real ( &dz, &ndz, iret );
    } else {
	/*
	 * Vertical coordinate system is NONE.
	 */
	*iret = -16;
	dg_merr ( "", "", "", &minus1, &minus1, &ivcord, errst, &ier );
	dg_cset ( "ERRST", errst, &ier );
	return;
    }

    /*
     * Get a new grid number for the stability field.
     */
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    /*
     * Compute the lapse rate grid in degrees / kilometer.
     */
    dg_getg ( &ndt, &gndt, &kx, &ky, &ksub1, &ksub2, iret );
    dg_getg ( &ndz, &gndz, &kx, &ky, &ksub1, &ksub2, iret );
    dg_getg ( &num, &gnum, &kx, &ky, &ksub1, &ksub2, iret );
    for ( i = ksub1; i <= ksub2; i++ ) {
        im1 = i - 1;
	if ( ERMISS ( gndt[im1] ) || ERMISS ( gndz[im1] ) || 
	    ( G_DIFFT(gndz[im1], 0.0F, GDIFFD) ) ) {
	    gnum[im1] = RMISSD;
	} else {
	    dt = gndt[im1];
	    dz = gndz[im1] / 1000.0F;
	    gnum[im1] = dt / dz;
	}
    }

    /*
     * Make a name STAB and update header.
     */
    dg_upsg ( time1, time2, &level1, &level2, &ivcord, &zero,
              "STAB", &num, iret );

    /*
     * Replace grid at the top of the stack which has not been
     * popped with the final grid.
     */
    dg_rpls ( "", &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
