#include "gdgrib.h"

void bds_pgb ( const float *grid, const int *igx, const int *igy,
               const float *qmin, const float *qmax, const int *nbits,
               int *lendat, unsigned char *cdata, int *iscale, int *iret )
/************************************************************************
 * bds_pgb								*
 *									*
 * This subroutine packs a grid into the simple GRIB packing using the	*
 * number of bits specified.  The packing and unpacking equations are:	*
 *									*
 *	IDATA  =  NINT ( ( GRID - QMIN ) / SCALE )			*
 *	GRID   =  QMIN  +  IDATA * ISCALE				*
 *									*
 * If LENDAT = 0, then the grid is constant everywhere. 		*
 *									*
 * bds_pgb  ( grid, igx, igy, qmin, qmax, nbits, lendat, cdata, iscale,	*
 *            iret )							*
 *									*
 * Input parameters:							*
 *	*grid		const float	Grid data			*
 *	*igx		const int	Number of points in x dir	*
 *	*igy		const int	Number of points in y dir	*
 *	*qmin		const float	Minimum of gridded data		*
 *	*qmax		const float	Maximum of gridded data		*
 *	*nbits		const int	Number of bits			*
 *									*
 * Input and output parameters:						*
 *	*lendat		int		Input: max # of bytes in CDATA	*
 *					Output: # of CDATA bytes needed	*
 *									*
 * Output parameters:							*
 *	*cdata		unsigned char	Packed data			*
 *	*iscale		int		Power of 2 for binary scaling	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-22 = NBITS invalid		*
 *					-23 = invalid data range	*
 *					-26 = array allocation too small*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 3/89						*
 * K. Brill/GSC		 2/90	Fix to find negative max on grid	*
 * K. Brill/NMC		03/92	Fix for constant grid			*
 * K. Brill/HPC		 8/99	Adapted for GDGRIB for byte output	*
 * J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
 *                              DATA statement                          *
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
    int nval[] = { 128, 64, 32, 16, 8, 4, 2, 1 };
    int kxky, lndat, nnnn, imax, idat, k2pwr, iword, ibit, icnt, k2, icval,
        kbit, ii, jj;
    float scale, qdif, gggg, vpow;
/*----------------------------------------------------------------------*/
    *iret = 0;
    kxky = (*igx) * (*igy);

    /*
     * Check for valid input.
     */
    if ( ( *nbits <= 1 ) || ( *nbits > 31 ) ) {
	*iret = -22;
	return;
    }

    /*
     * Compute the number of output words and initialize the output
     * buffer and scaling parameter.
     */
    lndat = (int) ( ( *nbits * kxky ) / 8. );
    if ( lndat * 8 != *nbits * kxky ) lndat += 1;
    if ( lndat > *lendat ) {
	*iret = -26;
	return;
    }
    for ( ii = 0; ii < lndat; ii++ ) {
	cdata[ii] = (unsigned char ) (0);
    }
    scale = 1.;

    /*
     * Find the data range and check that it is valid.
     */
    qdif = (*qmax) - (*qmin);
    if ( qdif < 0. ) {
	*iret = -23;
	return;
    } else if ( G_DIFF ( qdif, 0.0F) ) {
	lendat = 0;
	return;
    }

    /*
     * Find the scaling factor.  The scaling factor is set to a 
     * power of 2.
     */
    nnnn = 0;
    vpow = pow ( 2, *nbits );
    imax = (int)vpow- 1;
    idat = G_NINT ( qdif * pow ( 2., nnnn ) );
    if ( idat >= imax ) {
	while ( idat >= imax ) {
	    nnnn--;
	    idat = G_NINT ( qdif * pow ( 2., nnnn ) );
	}
    } else {
	while ( G_NINT ( qdif * pow ( 2., nnnn+1 ) ) < imax ) {
	    nnnn++;
	}
    }
    scale = pow ( 2., nnnn );
    *iscale = -nnnn;

    /*
     * Add data points to output buffer.
     */
    vpow = pow ( 2, *nbits );
    k2pwr = (int)vpow;
    iword = 0;
    ibit  = 0;
    icnt = 0;
    for ( ii = 0; ii < kxky; ii++ ) {
	if ( ERMISS ( grid[ii] ) ) {
	    /*
	     * Do nothing.
	     */
	} else {
	    /*
	     * Turn grid value into an integer.
	     */
	    icnt++;
	    gggg = grid[ii] - *qmin;
	    if ( gggg < 0. ) gggg = 0.;
	    idat = G_NINT ( gggg * scale );

	    /*
	     * Compute value of each bit and store it.
	     */
	    k2 = k2pwr;
	    for ( jj = *nbits; jj >= 1; jj-- ) {
		k2 /= 2;
		if ( idat - k2 >= 0 ) {
		    kbit = 1;
		    idat -= k2;
		} else {
		    kbit = 0;
		}
		icval = cdata[iword];
		icval += kbit * nval[ibit];
		cdata[iword] = (unsigned char)( icval );
		ibit++;
		if ( ibit > 7 ) {
		    iword++;
		    ibit = 0;
		}
	    }
	}
    }
    *lendat = (int)( *nbits * icnt / 8. );
    if ( *lendat * 8 != *nbits * icnt ) *lendat += 1;

    return;
}
