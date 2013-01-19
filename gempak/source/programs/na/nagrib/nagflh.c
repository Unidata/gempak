#include "nagrib.h"

void nagflh ( const int *intflg, const float *v, const int *nxvh,
              const int *nyvh, float *vh, int *iret )
/************************************************************************
 * nagflh								*
 * 									*
 * This subroutine fills the h points for an ETA V array.  The ETA	*
 * unstaggered grid has this structure:					*
 *									*
 *			h V h V h					*
 *			V h V h V					*
 *			h V h V h					*
 *									*
 * The filled grid has values of V interpolated at the h points.	*
 * 									*
 * nagflh ( intflg, v, nxvh, nyvh, vh, iret )				*
 * 									*
 * Input parameters:							*
 *	*intflg		const int	Interpolation type (linear now) *
 *	*v		const float	Array of V values		*
 *	*nxvh		const int	X dimension of filled grid	*
 *	*nyvh		const int	Y dimension of filled grid	*
 * 									*
 * Output parameters:							*
 *	*vh		float		Filled grid			*
 *	*iret		int		Return code			*
 *				  	  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC		12/94						*
 * R. Tian/SAIC		 8/06		Recoded from Fortran		*
 ************************************************************************/
{
    int idimvh, iv, ivh, i, j, ioff, ip1, im1, indx, iyp1, iym1, ixp1,
        ixm1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    idimvh = (*nxvh) * (*nyvh);

    /*
     * Load every other value in VH with a value from V.
     */
    iv = 0;
    for ( ivh = 2; ivh <= idimvh-1; ivh += 2 ) {
	vh[ivh-1] = v[iv++];
    }

    /*
     * Now fill in h points along the bottom and top.
     */
    for ( j = 1; j <= 2; j++ ) {
	if ( j == 1 ) ioff = 0;
	if ( j == 2 ) ioff = (*nxvh) * ( (*nyvh) - 1 );
	for ( i = 3; i <= (*nxvh) - 2; i += 2 ) {
	    ivh = ioff + i;
	    vh[ivh-1] = .5 * ( vh[ivh-2] + vh[ivh] );
	}
    }

    /*
     * Now fill in h points along the sides.
     */
    ivh = *nxvh;
    for ( j = 3; j <= (*nyvh) - 2; j += 2 ) {
	for ( i = 1; i <= 2; i++ ) {
	    if ( i == 1 ) ivh += (*nxvh) + 1;
	    if ( i == 2 ) ivh += (*nxvh) - 1;
	    ip1 = ivh + (*nxvh);
	    im1 = ivh - (*nxvh);
	    vh[ivh-1] = .5 * ( vh[im1-1] + vh[ip1-1] );
	}
    }

    /*
     * Fill in h points in the interior.
     */
    for ( i = 2; i <= (*nxvh) - 1; i++ ) {
	for ( j = 2; j <= (*nyvh) - 1; j++ ) {
	    indx = ( j - 1 ) * (*nxvh) + i;
	    if ( ( indx % 2 ) == 1 ) {
		iyp1 = indx + (*nxvh);
		iym1 = indx - (*nxvh);
		ixp1 = indx + 1;
		ixm1 = indx - 1;
		vh[indx-1] = .25 * ( vh[iyp1-1] + vh[iym1-1] +
			             vh[ixp1-1] + vh[ixm1-1] );
	    }
	}
    }

    /*
     * Finally, extrapolate to corner points.
     */
    vh[0] = .5 * ( 1.5 * ( vh[1] + vh[*nxvh] ) -
		    .5 * ( vh[3] + vh[3*(*nxvh)] ) );
    vh[(*nxvh)-1] = .5 * ( 1.5 * ( vh[(*nxvh)-2] + vh[2*(*nxvh)-1] ) -
		            .5 * ( vh[(*nxvh)-4] + vh[4*(*nxvh)-1] ) );
    indx = (*nxvh) * ( (*nyvh) - 1 ) + 1;
    vh[indx-1] = .5 * ( 1.5 * ( vh[indx] + vh[indx-(*nxvh)-1] ) -
     		         .5 * ( vh[indx+2] + vh[indx-2*(*nxvh)-1] ) );
    indx = idimvh;
    vh[indx-1] = .5 * ( 1.5 * ( vh[indx-2] + vh[indx-(*nxvh)-1] ) -
		         .5 * ( vh[indx-4] + vh[indx-2*(*nxvh)-1] ) );
    
    return;
}
