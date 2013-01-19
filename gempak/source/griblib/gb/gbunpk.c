#include "gbcmn.h"

void gb_unpk ( int *igrid, int npts, int nbits, float *fgrid, int *iret )
/************************************************************************
 * gb_unpk                                                              *
 *                                                                      *
 * This function gets the GRIB data from GB_BDS and returns an unpacked *
 * grid of floating point numbers.                                      *
 *                                                                      *
 * gb_unpk ( igrid, npts, nbits, fgrid, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *igrid		int		Integer array of packed values	*
 *      npts		int		Number of values packed		*
 *                                                                      *
 * Output parameters:                                                   *
 *      nbits		int		Number of bits for packing	*
 *      *fgrid		float		Unpacked grid data              *
 *      *iret		int		Return code                     *
 *                                          -16 = Error on next msg     *
 *					    -18 = Invalid bitmap	*
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI           7/93                                           *
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * K. Brill/NMC          6/94   Input parm documentation        	*
 * L. Oolman/U of WY	 4/95	Changed pow func to left shifts		*
 * S. Jacobs/NCEP	 8/98	Changed igrid from long int to int	*
 * T. Piper/GSC		11/98	Updated prolog 				*
 ***********************************************************************/
{
	int	i, imax, imax2, ibit, iword, idat, idat2, jshft, jshft2;

/*---------------------------------------------------------------------*/
        *iret = 0;

/*
**	Get the grid array values from the buffer.
*/
	imax  = ( 1 << nbits ) - 1;
	ibit  = 1;
	iword = 0;

	for ( i = 0; i < npts; i++ ) {

	    jshft = nbits + ibit - 33;
	    idat = 0;
	    if ( jshft <= 0 ) {
		idat = ( igrid[iword] >> abs(jshft) ) & imax;
	    }
	    else {
		imax2 = imax - ( ( 1 << jshft ) - 1 );
		idat = ( igrid[iword] << jshft ) & imax2;
	    }

/*
**	    Check to see if packed integer overflows into next word.
*/
	    if ( jshft > 0 ) {
		jshft2 = jshft;
		jshft -= 32;
		idat2 = 0;
		if ( jshft <= 0 ) {
		    imax2 = ( 1 << jshft2 ) - 1;
		    idat2 = ( igrid[iword+1] >> abs(jshft) ) & imax2;
		}
		else {
		    imax2 = imax - ( ( 1 << jshft2 ) - 1 );
		    idat2 = ( igrid[iword+1] << jshft ) & imax2;
		}
		idat += idat2;
	    }

/*
**	    Set the temporary floating point grid.
*/
	    fgrid[i] = (float) idat;

/*
**	    Set location for next word.
*/
	    ibit += nbits;
	    if ( ibit > 32 ) {
		ibit -= 32;
		iword++;
	    }
	}

}
