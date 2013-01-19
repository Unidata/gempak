#include "nagrib.h"

void nagflv ( const int *intflg, const float *h, const int *nxhv,
              const int *nyhv, float *hv, int *iret )
/************************************************************************
 * nagflv								*
 * 									*
 * This subroutine fills the v points for an ETA H array.  The ETA	*
 * unstaggered grid has this structure:					*
 *									*
 *			H v H v H					*
 *			v H v H v					*
 *			H v H v H					*
 *									*
 * The filled grid has values of H interpolated at the v points.	*
 * 									*
 * nagflv ( intflg, h, nxhv, nyhv, hv, iret )				*
 * 									*
 * Input parameters:							*
 *	*intflg		const int	Interpolation type (linear now) *
 *	*h		const float	Array of H values		*
 *	*nxhv		const int	X dimension of filled grid	*
 *	*nyhv		const int	Y dimension of filled grid	*
 * 									*
 * Output parameters:							*
 *	*hv		float		Filled grid			*
 *	*iret		int		Return code			*
 *				  	  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC		12/94						*
 * R. Tian/SAIC		 8/06		Recoded from Fortran		*
 ************************************************************************/
{
    int idimhv, ih, ihv, ioff, i, j, ip1, im1, indx, iyp1, iym1, ixp1,
        ixm1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    idimhv = (*nxhv) * (*nyhv);

    /* 
     * Load every other value in HV with a value from H.
     */
    ih = 0;
    for ( ihv = 1; ihv <= idimhv; ihv += 2 ) {
	hv[ihv-1] = h[ih++];
    }

    /*
     * Now fill in v points along the bottom and top.
     */
    for ( j = 1; j <= 2; j++ ) {
	if ( j == 1 ) ioff = 0;
	if ( j == 2 ) ioff = (*nxhv) * ( (*nyhv) - 1 );
	for ( i = 2; i <= (*nxhv); i += 2 ) {
	    ihv = ioff + i;
	    hv[ihv-1] = .5 * ( hv[ihv-2] + hv[ihv] );
	}
    }

    /*
     * Now fill in v points along the sides.
     */
    ihv = 0;
    for ( j = 2; j <= (*nyhv); j += 2 ) {
	for ( i = 1; i <= 2; i++ ) {
	    if ( i == 1 ) ihv = ihv + (*nxhv) + 1;
	    if ( i == 2 ) ihv = ihv + (*nxhv) - 1;
	    ip1 = ihv + (*nxhv);
	    im1 = ihv - (*nxhv);
	    hv[ihv-1] = .5 * ( hv[im1-1] + hv[ip1-1] );
	}
    }

    /*
     * Finally fill in v points in the interior.
     */
    for ( i = 2; i <= (*nxhv) - 1; i++ ) { 
	for ( j = 2; j <= (*nyhv) - 1; j++ ) {
	    indx = ( j - 1 ) * (*nxhv) + i;
	    if ( ( indx % 2 ) == 0 ) {
		iyp1 = indx + (*nxhv);
		iym1 = indx - (*nxhv);
		ixp1 = indx + 1;
		ixm1 = indx - 1;
		hv[indx-1] = .25 * ( hv[iyp1-1] + hv[iym1-1] +
                                     hv[ixp1-1] + hv[ixm1-1] );
	    }
	}
    }

    return;
}
