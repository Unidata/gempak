#include "dg.h"

void dg_nrel ( const float *urel, const float *vrel, float *unor,
               float *vnor, int *iret )
/************************************************************************
 * dg_nrel								*
 *									*
 * This subroutine converts grid-relative vector components into	*
 * north-relative components.  The input and output arrays may be	*
 * the same.								*
 *									*
 * dg_nrel ( urel, vrel, unor, vnor, iret )				*
 *									*
 * Input parameters:							*
 *	*urel		const float	Grid rel u-component		*
 *	*vrel		const float	Grid rel v-component		*
 *									*
 * Output parameters:							*
 *	*unor		float		North rel u-component		*
 *	*vnor		float		North rel v-component		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * G. Huffman/GSC	 4/89						*
 * M. desJardins/GSFC	 7/89	Added PA_ subroutines			*
 * M. desJardins/GSFC	 8/89	PA to PD subroutines			*
 * K. Brill/GSC		 9/89   Rewrite for matrix rotation		*
 * K. Brill/NMC          8/90   Correct for input and output same	*
 * K. Brill/EMC		 3/96	CED,MER,MCD rotated for general case	*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids	for rot matrix	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float usav;
    int i, im1;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Make sure that the rotation matrix elements are
     * available and rotate all grid points (KXYD is KXD * KYD).
     */
    dg_grot ( iret );
    if ( *iret != 0 ) return;

    for ( i = _dgarea.ksub1; i <= _dgarea.ksub2; i++ ) {
        im1 = i - 1;
	if ( ERMISS ( urel[im1] ) || ERMISS ( vrel[im1] ) ||
	     ERMISS ( _dggrid.dgg[_dgrtwd.irtcos-1].grid[im1] ) ||
	     ERMISS ( _dggrid.dgg[_dgrtwd.irtsin-1].grid[im1] ) ) {
	    vnor[im1] = RMISSD;
	    unor[im1] = RMISSD;
	} else {
	    usav = _dggrid.dgg[_dgrtwd.irtcos-1].grid[im1] * urel[im1] +
	           _dggrid.dgg[_dgrtwd.irtsin-1].grid[im1] * vrel[im1];
	    vnor[im1] = -_dggrid.dgg[_dgrtwd.irtsin-1].grid[im1] * urel[im1] +
	                 _dggrid.dgg[_dgrtwd.irtcos-1].grid[im1] * vrel[im1];
	    unor[im1] = usav;
	}
    }

    return;
}
