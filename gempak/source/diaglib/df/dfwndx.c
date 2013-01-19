#include "df.h"

void df_wndx ( int *iret )
/************************************************************************
 * df_wndx								*
 *									*
 * This subroutine computes the WINDEX, an index for microburst		*
 * potential.  WINDEX estimates the maximum potential wind gust at the	*
 * surface in meters per second.  The following equation is used:	*
 *									*
 * 	WNDX ( S1, S2, S3, S4) = 2.5 * SQRT ( HGHTF * RATIO * ( GAMMA	*
 * 	**2 - 30 + MIXRS - 2 * MIXRF ) )				*
 *									*
 *	GAMMA = TMPCS/HGHTF  						*
 *	RATIO = MIXRS/12 if MIXRS < 12, 1 otherwise			*
 *	TMPCS = surface TMPC 	= S1					*
 *	HGHTF = AGL HGHT of FRZL = S2					*
 *	MIXRS = surface MIXR	= S3 					*
 *	MIXRF = MIXR at FRZL	= S4					*
 *									*
 * df_wndx ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * D. McCann/NSSFC	 8/95						*
 * K. Tyle/GSC		11/95	Documentation and cleanup		*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int kxd, kyd, ksub1, ksub2, i, im1, ier, zero;
    int ntmpcs, nhghtf, nmixrs, nmixrf, nwndx;
    float *gntmpcs, *gnhghtf, *gnmixrs, *gnmixrf, *gnwndx;
    float hghtkm, smixr, fmixr, gamma, ratio, arg;

/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the four grids from the stack.
     */
    dg_gets ( &ntmpcs, iret );
    if ( *iret != 0 ) return;

    dg_gets ( &nhghtf, iret );
    if ( *iret != 0 ) return;

    dg_gets ( &nmixrs, iret );
    if ( *iret != 0 ) return;

    dg_gets ( &nmixrf, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid for the WINDEX.
     */
    dg_nxts ( &nwndx, iret );
    if ( *iret != 0 )  return;

    /*
     * Grid number to grid.
     */
    dg_getg ( &ntmpcs, &gntmpcs, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nhghtf, &gnhghtf, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nmixrs, &gnmixrs, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nmixrf, &gnmixrf, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nwndx,  &gnwndx,  &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Calculate WINDEX.
     */
    for ( i = ksub1; i <= ksub2; i++ ) {
        im1 = i - 1;
	if ( gnhghtf[im1] <= 0.0 ) {
	    gnwndx[im1] = 0.0;
	} else {
	    hghtkm = gnhghtf[im1] / 1000.;
	    smixr  = gnmixrs[im1] * 1000.;
	    fmixr  = gnmixrf[im1] * 1000.;
	    gamma  = gntmpcs[im1] / hghtkm;
	    ratio  = smixr / 12.0;
	    if ( ratio > 1.0 ) ratio = 1.0;

	    arg = hghtkm * ratio * (gamma * gamma - 30.0 + smixr - 2 * fmixr );

	    if ( arg < 0.0 ) {
		gnwndx[im1] = 0.0;
	    } else {
		gnwndx[im1] = 2.5 * sqrt ( arg );
	    }
	}
    }

    /*
     * Make a name of the form 'WNDX' and update header.
     */
    dg_updh ( "WNDX", &nwndx, &ntmpcs, &nhghtf, iret );

    /*
     * Update stack.
     */
    dg_puts ( &nwndx, iret );
    dg_esub ( &nwndx, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
