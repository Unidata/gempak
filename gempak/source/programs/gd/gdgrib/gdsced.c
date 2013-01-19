#include "gdgrib.h"

void gds_ced ( const float *rnvblk, const int*nnv, int *nbytes,
               unsigned char *cgds, int *iret )
/************************************************************************
 * gds_ced								*
 *									*
 * This subroutine uses the GEMPAK grid navigation for a CED grid to	*
 * generate a GRIB GDS section for this grid.				*
 *									*
 * gds_ced ( rnvblk, nnv, nbytes, cgds, iret )				*
 *									*
 * Input parameters:							*
 *	*rnvblk		const float	GEMPAK grid navigation block	*
 *	*nnv		const int	Size of the navigation block	*
 *									*
 * Input and output parameter:						*
 *	*nbytes		int		Input: # of bytes available in	*
 *					       CGDS			*
 *					Output: # of bytes filled in	*
 *					       CGDS			*
 *									*
 * Output parameters:							*
 *	*cgds		unsigned char	GRIB GDS section		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-61 = not enough GDS bytes	*
 *					-62 = # in i is too large	*
 *					-63 = # in j is too large	*
 *					-64 = latitude 1 is bad		*
 *					-65 = longitude 1 is bad	*
 *					-66 = latitude 2 is bad		*
 *					-67 = longitude 2 is bad	*
 *					-68 = rotated CED not supported *
 **									*
 * Log:									*
 * K. Brill/HPC		 8/99						*
 * K. Brill/HPC		 2/00	Document GDS byte numbers		*
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 ************************************************************************/
{
    int ibase, kx, ky, ibyts[3], ilat, ilon, ii, idx, nb, ier;
    float rlat1, rlon1, rlat2, rlon2;
/*----------------------------------------------------------------------*/
    *iret = 0;
    ibase = 256;

    if ( *nbytes < 32 ) {
	*iret = -61;
	return;
    }
    if ( ! G_DIFF ( rnvblk[12], 0.0F ) ) {
	*iret = -68;
	return;
    }
    *nbytes = 32;

    /*
     * Get information from the navigation block.
     */
    kx = G_NINT ( rnvblk[4] );
    ky = G_NINT ( rnvblk[5] );
    rlat1 = rnvblk[6];
    rlon1 = rnvblk[7];
    rlat2 = rnvblk[8];
    rlon2 = rnvblk[9];

    /*
     * Start filling up the GDS array.
     */
    idx = 0;
    cgds[idx++] = (unsigned char) (0);
    cgds[idx++] = (unsigned char) (0);
    cgds[idx++] = (unsigned char) (32);
    cgds[idx++] = (unsigned char) (0);
    cgds[idx++] = (unsigned char) (255);
    cgds[idx++] = (unsigned char) (0);

    /*
     * Set bytes 7-8.
     */
    nb = 2;
    gdigit ( &kx, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -62;
	return;
    }
    for ( ii = 1; ii >= 0; ii-- )  cgds[idx++] = (unsigned char) ( ibyts [ii] );

    /*
     * Set bytes 9-10.
     */
    nb = 2;
    gdigit ( &ky, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -63;
	return;
    }
    for ( ii = 1; ii >= 0; ii-- )  cgds[idx++] = (unsigned char) ( ibyts [ii] );

    /*
     * Set bytes 11-13.
     */
    ilat = G_ABS ( G_NINT ( rlat1 * 1000. ) );
    nb = 3;
    gdigit ( &ilat, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -64;
	return;
    }
    if ( rlat1 < 0.0F ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[idx++] = (unsigned char) ( ibyts[ii] );

    /*
     * Set bytes 14-16.
     */
    ilon = G_ABS ( G_NINT ( rlon1 * 1000. ) );
    nb = 3;
    gdigit ( &ilon, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -65;
	return;
    }
    if ( rlon1 < 0.0F ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[idx++] = (unsigned char) ( ibyts[ii] );

    /*
     * Set byte 17.
     */
    cgds[idx++] = (unsigned char) (0);

    /*
     * Set bytes 18-20.
     */
    ilat = G_ABS ( G_NINT ( rlat2 * 1000. ) );
    nb = 3;
    gdigit ( &ilat, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -66;
	return;
    }
    if ( rlat2 < 0 ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[idx++] = (unsigned char) ( ibyts[ii] );

    /*
     * Set bytes 21-23.
     */
    ilon = G_ABS ( G_NINT ( rlon2 * 1000. ) );
    nb = 3;
    gdigit ( &ilon, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -67;
	return;
    }
    if ( rlon2 < 0 ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[idx++] = (unsigned char) ( ibyts[ii] );

    /*
     * Set bytes 24-32.
     */
    cgds[idx++] = (unsigned char) (255);
    cgds[idx++] = (unsigned char) (255);
    cgds[idx++] = (unsigned char) (255);
    cgds[idx++] = (unsigned char) (255);
    cgds[idx++] = (unsigned char) (64);
    cgds[idx++] = (unsigned char) (0);
    cgds[idx++] = (unsigned char) (0);
    cgds[idx++] = (unsigned char) (0);
    cgds[idx++] = (unsigned char) (0);

    return;
}
