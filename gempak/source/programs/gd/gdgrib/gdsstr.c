#include "gdgrib.h"

void gds_str ( const int *navchg, const float *rnvblk, const int *nnv,
               int *nbytes, unsigned char *cgds, int *iret )
/************************************************************************
 * gds_str								*
 *									*
 * This subroutine uses the GEMPAK grid navigation for an unrotated	*
 * polar stereographic projection to generate a GRIB GDS section for	*
 * this grid.								*
 *									*
 * gds_str ( navchg, rnvblk, nnv, nbytes, cgds, iret )			*
 *									*
 * Input parameters:							*
 *	*navchg		const int	Navigation change flag		*
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
 *					-69 = rotated STR not supported *
 *					-70 = DX grid incrmnt invalid	*
 *					-71 = DY grid incrmnt invalid	*
 *					-72 = center longitude is bad	*
 **									*
 * Log:									*
 * K. Brill/HPC		 8/99						*
 * K. Brill/HPC		 2/00	Doc byte #'s; Set GDS 17 for north rel	*
 * K. Brill/HPC		 3/00	Set GDS 17 using NAVCHG flag		*
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 ************************************************************************/
{
    int ibase, kx, ky, ibyts[3], ilat, ilon, ii, index, idx, idy, nb, ier;
    float rlat1, rlon1, rlat2, rlon2, polat, rlov, rnx, rny, sign, clon,
          tan1, tan2, dlon1, dlon2, re, x1, x2, y1, y2, dx, dy;
/*----------------------------------------------------------------------*/
    *iret = 0;
    ibase = 256;

    if ( *nbytes < 32 ) {
	*iret = -61;
	return;
    }
    if ( ! G_DIFF ( rnvblk[12], 0.0 ) || 
         G_ABS ( 90.0 - G_ABS ( rnvblk[10] ) ) > .005 ) {
	*iret = -69;
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
    polat = rnvblk[10];
    rlov  = rnvblk[11];

    /*
     * Start filling up the GDS array.
     */
    index = 0;
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (32);
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (255);
    cgds[index++] = (unsigned char) (5);

    /*
     * Set bytes 7-8.
     */
    nb = 2;
    gdigit ( &kx, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -62;
	return;
    }
    for ( ii = 1; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    /*
     * Set bytes 9-10.
     */
    nb = 2;
    gdigit ( &ky, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -63;
	return;
    }
    for ( ii = 1; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

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
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

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
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    /*
     * Set byte 17.
     */
    if ( *navchg == G_TRUE ) {
	cgds[index++] = (unsigned char) (0);
    } else {
	cgds[index++] = (unsigned char) (8);
    }
 
    /*
     * Set bytes 18-20.
     */
    ilon = G_ABS ( G_NINT ( rlov * 1000. ) );
    nb = 3;
    gdigit ( &ilon, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -72;
	return;
    }
    if ( rlov < 0.0F ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    /*
     * Compute the grid increments.
     */
    if ( polat > 0.0 ) {
	sign = -1.0;
    } else {
	sign = 1.0;
    }
    rnx = rnvblk[4];
    rny = rnvblk[5];
    rlat1 *= DTR / 2.;
    rlat2 *= DTR / 2.;
    rlon1 *= DTR;
    rlon2 *= DTR;
    clon = rlov * DTR;
    re = RADIUS;
    tan1 = (float)tan ( PI4TH + sign * rlat1 );
    tan2 = (float)tan ( PI4TH + sign * rlat2 );
    dlon1 = rlon1 - clon;
    dlon2 = rlon2 - clon;
    x1 = re * tan1 * sin ( dlon1 );
    y1 = sign * re * tan1 * cos ( dlon1 );
    x2 = re * tan2 * sin ( dlon2 );
    y2 = sign * re * tan2 * cos ( dlon2 );
    dx = ( x2 - x1 ) * 1.8660254 / ( rnx - 1. );
    dy = ( y2 - y1 ) * 1.8660254 / ( rny - 1. );

    /*
     * Set bytes 21-23.
     */
    idx = G_NINT ( dx );
    nb = 3;
    gdigit ( &idx, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -70;
	return;
    }
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    /*
     * Set bytes 24-26.
     */
    idy = G_NINT ( dy );
    nb = 3;
    gdigit ( &idy, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -71;
	return;
    }
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    if ( polat > 0.0 ) {
	cgds[index++] = (unsigned char) (0);
    } else {
	cgds[index++] = (unsigned char) (128);
    }
    cgds[index++] = (unsigned char) (64);
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (0);

    return;
}
