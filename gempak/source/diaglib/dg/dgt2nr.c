#include "dg.h"

void dg_t2nr ( const float *urel, const float *vrel, float *unor,
               float * vnor, int *iret )
/************************************************************************
 * dg_t2nr								*
 *									*
 * This subroutine converts transfer grid-relative vector components 	*
 * into north-relative components.  The input and output arrays may be	*
 * the same.								*
 *									*
 * dg_t2nr ( urel, vrel, unor, vnor, iret )				*
 *									*
 * Input parameters:							*
 *	*urel		const float	Transfer Grid rel u-component	*
 *	*vrel		const flaot	Transfer Grid rel v-component	*
 *									*
 * Output parameters:							*
 *	*unor		float		North rel u-component		*
 *	*vnor		float		North rel v-component		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/HPC		 3/04	Created from DG_NREL			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float usav;
    int i, im1;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Make sure that the rotation matrix elements are
     * available and rotate all grid points.
     */
    dg_trot ( iret );
    if ( *iret != 0 ) return;

    for ( i = _dgarea.ksub1; i <= _dgarea.ksub2; i++ ) {
        im1 = i - 1;
	if ( ERMISS ( urel[im1] ) || ERMISS ( vrel[im1] ) ||
	     ERMISS ( _dggrid.dgg[_hintrp.icsrot-1].grid[im1] ) ||
	     ERMISS ( _dggrid.dgg[_hintrp.isnrot-1].grid[im1] ) ) {
	    vnor[im1] = RMISSD;
	    unor[im1] = RMISSD;
	} else {
	    usav = _dggrid.dgg[_hintrp.icsrot-1].grid[im1] * urel[im1] +
		   _dggrid.dgg[_hintrp.isnrot-1].grid[im1] * vrel[im1];
	    vnor[im1] = _dggrid.dgg[_hintrp.icsrot-1].grid[im1] * vrel[im1] -
	    		_dggrid.dgg[_hintrp.isnrot-1].grid[im1] * urel[im1];
	    unor[im1] = usav;
	}
    }

    return;
}
