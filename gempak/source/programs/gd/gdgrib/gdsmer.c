#include "gdgrib.h"

void gds_mer ( const float *rnvblk, const int *nnv, int *nbytes,
               unsigned char *cgds, int *iret )
/************************************************************************
 * gds_mer								*
 *									*
 * This subroutine uses the GEMPAK grid navigation for a CED grid to	*
 * generate a GRIB GDS section for this grid.				*
 *									*
 * gds_mer ( rnvblk, nnv, nbytes, cgds, iret )				*
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
 *					-70 = DX grid incrmnt invalid	*
 *					-71 = DY grid incrmnt invalid	*
 *					-74 = rotated MER not supported *
 **									*
 * Log:									*
 * K. Brill/HPC		 8/99						*
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 ************************************************************************/
{
    int ibase, kx, ky, ibyts[3], ilat, ilon, ii, index, idx, idy, nb, ier;
    float rlat1, rlon1, rlat2, rlon2, rnx, rny, x1, x2, y1, y2, dx, dy;
/*----------------------------------------------------------------------*/
    *iret = 0;
    ibase = 256;

    if ( *nbytes < 42 ) {
	*iret = -61;
	return;
    }
    if ( ! G_DIFF ( rnvblk[12], 0.0 ) || ! G_DIFF ( rnvblk[10], 0.0 ) ) {
	*iret = -74;
	return;
    }
    *nbytes = 42;

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
    index = 0;
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (42);
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (255);
    cgds[index++] = (unsigned char) (1);

    /*
     * Fill bytes 7-8, 9-10.
     */
    nb = 2;
    gdigit ( &kx, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -62;
	return;
    }
    for ( ii = 1; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );
    nb = 2;
    gdigit ( &ky, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -63;
	return;
    }
    for ( ii = 1; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    /*
     * Fill bytes 11-13, 14-16.
     */
    ilat = G_ABS ( G_NINT ( rlat1 * 1000. ) );
    nb = 3;
    gdigit ( &ilat, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -64;
	return;
    }
    if ( rlat1 < 0.0F ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );
    ilon = G_ABS ( G_NINT ( rlon1 * 1000. ) );
    nb = 3;
    gdigit ( &ilon, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -65;
	return;
    }
    if ( rlon1 < 0.0F ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    /*
     * Fill byte 17.
     */
    cgds[index++] = (unsigned char) (128);

    /*
     * Fill bytes 18-20, 21-23.
     */
    ilat = G_ABS ( G_NINT ( rlat2 * 1000. ) );
    nb = 3;
    gdigit ( &ilat, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -66;
	return;
    }
    if ( rlat2 < 0.0F ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );
    ilon = G_ABS ( G_NINT ( rlon2 * 1000. ) );
    nb = 3;
    gdigit ( &ilon, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -67;
	return;
    }
    if ( rlon2 < 0.0F ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    /*
     * True latitude is always 0.0.
     */
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (0);

    /*
     * Byte 27 is reserved.
     * Byte 28 is the scanning mode flag.
     */
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (64);

    /*
     * Fill bytes 29-31, 32-34, the increments.
     */
    rnx = rnvblk[4];
    rny = rnvblk[5];
    rlat1 *= DTR / 2.;
    rlat2 *= DTR / 2.;
    if ( rlon2 < rlon1 ) rlon2 += 360.;
    rlon1 *= DTR;
    rlon2 *= DTR;
    x1 = 0.0;
    y1 = RADIUS * log ( tan ( PI4TH + rlat1 ) );
    x2 = RADIUS * ( rlon2 - rlon1 );
    y2 = RADIUS * log ( tan ( PI4TH + rlat2 ) );
    dx = ( x2 - x1 ) / ( rnx - 1. );
    dy = ( y2 - y1 ) / ( rny - 1. );
    idx = G_NINT ( dx );
    nb = 3;
    gdigit ( &idx, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -70;
	return;
    }
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );
    idy = G_NINT ( dy );
    nb = 3;
    gdigit ( &idy, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -71;
	return;
    }
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    /*
     * *Bytes 35--42 are reserved.
     */
    for ( ii = 35; ii <= 42; ii++ ) cgds[index++] = (unsigned char) (0);

    return;
}
