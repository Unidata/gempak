#include "dg.h"

void dg_grel ( const float *unor, const float *vnor, float *urel,
               float *vrel, int *iret )
/************************************************************************
 * dg_grel								*
 *									*
 * This subroutine converts north-relative vector components into	*
 * grid-relative components.  The input and output variables can be	*
 * the same.								*
 *									*
 * dg_grel ( unor, vnor, urel, vrel, iret )				*
 *									*
 * Input parameters:							*
 *	*unor		const float	North rel u-component		*
 *	*vnor		const float	North rel v-component		*
 *									*
 * Output parameters:							*
 *	*urel		float		Grid rel u-component		*
 *	*vrel		float		Grid rel v-component		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * G. Huffman/GSC	 4/89						*
 * M. desJardins/GSFC	 7/89	Added PA subroutines			*
 * M. desJardins/GSFC	 8/89	PA to PD subroutines			*
 * K. Brill/GSC		 9/89   Rewrite for matrix rotation		*
 * K. Brill/NMC          8/90   Fix so input and output can be same	*
 * K. Brill/EMC		 3/96	CED,MER,MCD rotated for	general case	*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids for rot matrix	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float usav;
    int i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Make sure that the rotation matrix elements are
     * available and rotate all grid points (KXYD is KXD * KYD).
     */
    dg_grot ( iret );
    if ( *iret != 0 ) return;

    for ( i = _dgarea.ksub1 - 1; i < _dgarea.ksub2; i++ ) {
	if ( ERMISS ( unor[i] ) || ERMISS ( vnor[i] ) ||
	     ERMISS ( _dggrid.dgg[_dgrtwd.irtcos-1].grid[i] ) ||
	     ERMISS ( _dggrid.dgg[_dgrtwd.irtsin-1].grid[i] ) ) {
	    vrel[i] = RMISSD;
	    urel[i] = RMISSD;
	} else {
	    usav = _dggrid.dgg[_dgrtwd.irtcos-1].grid[i] * unor[i] -
		   _dggrid.dgg[_dgrtwd.irtsin-1].grid[i] * vnor[i];
	    vrel[i] = _dggrid.dgg[_dgrtwd.irtsin-1].grid[i] * unor[i] +
		      _dggrid.dgg[_dgrtwd.irtcos-1].grid[i] * vnor[i];
	    urel[i] = usav;
	}
    }

    return;
}
