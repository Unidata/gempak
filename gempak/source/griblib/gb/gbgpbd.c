#include "gbcmn.h"

void gb_gpbd ( int *kx, int *ky, int *iuscal, float *ref, 
	float *scale, int *nbits, int *igrid, int *lengrd, int *iret )
/************************************************************************
 * gb_gpbd								*
 *                                                                      *
 * This function gets the GRIB data from GB_BDS and returns a packed	*
 * grid of integers.							*
 *                                                                      *
 * gb_gpbd ( kx, ky, iuscal, ref, scale, nbits, igrid, lengrd, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*kx		int		Number of columns		*
 *	*ky		int		Number of rows			*
 *	*iuscal		int		GEMPAK units scale factor	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*ref		float		Grid refence value		*
 *	*scale		float		Grid scale factor		*
 *	*nbits		int		Number of packing bits		*
 *	*igrid		int		Packed grid data		*
 *	*lengrd		int		Number of grid points		*
 *	*iret		int		Return code                     *
 *                                          -19 = Error on message      *
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI           7/93                                           *
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * L. Sager/NMC     	 8/95	Fix memory allocation error		*
 * S. Jacobs/NCEP	 1/96	Changed DA_READ to CFL_READ		*
 * D.W.Plummer/NCEP	 2/96	Cleanup GBDIAGs and comments		*
 * D.W.Plummer/NCEP	 3/96	Changed cfl_ call sequence		*
 * K. Brill/EMC		 5/98	Add to lengrd for partial word  	*
 * S. Jacobs/NCEP	 8/98	Changed igrid from long int to int	*
 ***********************************************************************/
{
	int		offset, length, ret, nbytes, indx, lentmp;
	unsigned char	*buffer;

/*---------------------------------------------------------------------*/

	offset = cursor + ISLENGTH + pdslength + gdslength + bmslength;

	/* 
	 *      Allocate space for the buffer.
	 */

        buffer = (unsigned char *)
                  malloc ( 3 * sizeof(unsigned char) );
	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	cfl_read ( gbfile.fptr, 3, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -19;
	    return;
	}

	indx = 0;
	bdslength = gb_btoi ( buffer, indx, 3, FALSE );
        free ( buffer );

	/* 
	 *      Allocate space for the buffer.
	 */

        buffer = (unsigned char *)
                  malloc ( bdslength * sizeof(unsigned char) );
	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	cfl_read ( gbfile.fptr, bdslength, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -19;
	    return;
	}

	length = (*kx) * (*ky);

	gb_bds ( buffer, length, igrid );

	lentmp = bdslength - 11;
	*lengrd = ( bdslength - 11 ) / sizeof (int);
	if ( lentmp % sizeof (int) > 0 ) *lengrd = *lengrd + 1;

	*nbits  = bds.num_bits;

	*ref    = bds.ref_value *
		  pow ( 10.0, (double) (*iuscal - pds.dec_scale) );

	*scale  = pow ( 2.0, (double) bds.binary_scale ) *
		  pow ( 10.0, (double) (*iuscal - pds.dec_scale) );

	cfl_read ( gbfile.fptr, 4, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -19;
	    return;
	}

	gb_ends(buffer);

	free ( buffer );

	if ( GBDIAG_BDS == TRUE )
	    printf("PACKED BINARY DATA\n");

}
