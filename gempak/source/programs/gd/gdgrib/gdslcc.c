#include "gdgrib.h"

void gds_lcc ( const int *navchg, const float *rnvblk, const int *nnv,
               int *nbytes, unsigned char *cgds, int *iret )
/************************************************************************
 * gds_lcc								*
 *									*
 * This subroutine uses the GEMPAK grid navigation for a Lambert	*
 * conic conformal projection to generate a GRIB GDS section for	*
 * this grid.								*
 *									*
 * gds_lcc ( navchg, rnvblk, nnv, nbytes, cgds, iret )			*
 *									*
 * Input parameters:							*
 *	*navchg		const int	Flag for navigation change	*
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
 *					-70 = DX grid incrmnt invalid	*
 *					-71 = DY grid incrmnt invalid	*
 *					-72 = central longitude is bad	*
 *					-73 = true latitudes are bad	*
 **									*
 * Log:									*
 * K. Brill/HPC		 8/99						*
 * K. Brill/HPC		 2/00	Set byte 17 for north rel wind comps	*
 * K. Brill/HPC		 3/00	Set byte 17 using NAVCHG flag		*
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 ************************************************************************/
{
    int ibase, kx, ky, ibyts[3], ilat, ilon, ii, index, idx, idy, nb, ier;
    float rlat1, rlon1, rlat2, rlon2, rlov, rnx, rny, sign, clon, tan1,
          tan2, dlon1, dlon2, phi1, phi2, trult1, trult2, cc, re, alfa,
	  psi1, psi2, x1, x2, y1, y2, dx, dy;
/*----------------------------------------------------------------------*/
    *iret = 0;
    ibase = 256;

    if ( *nbytes < 42 ) {
	*iret = -61;
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
    phi1  = rnvblk[10];
    rlov  = rnvblk[11];
    phi2  = rnvblk[12];

    /*
     * Start filling up the GDS array.
     */
    index = 0;
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (42);
    cgds[index++] = (unsigned char) (0);
    cgds[index++] = (unsigned char) (255);
    cgds[index++] = (unsigned char) (3);

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
    if ( rlat1 < 0 ) ibyts[2] += 128;
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
    if ( *navchg == G_TRUE ) {
	cgds[index++] = (unsigned char) (0);
    } else {
	cgds[index++] = (unsigned char) (8);
    }

    /*
     * Fill bytes 18-20.
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
    if ( G_ABS ( phi1 ) > G_ABS ( phi2 ) ) {
	trult1 = phi1;
	trult2 = phi2;
	if ( phi1 > 0.0F ) {
	    sign = -1.0;
	} else {
	    sign = 1.0;
	}
    } else if ( G_ABS ( phi2 ) > G_ABS ( phi1 ) ) {
	trult1 = phi2;
	trult2 = phi1;
	if ( phi2 > 0.0F ) {
	    sign = -1.0;
	} else {
	    sign = 1.0;
	}
    } else {
	trult1 = phi1;
	trult2 = phi2;
	if ( phi1 > 0.0F ) {
	    sign = -1.0;
	} else {
	    sign = 1.0;
	}
    }
    rnx = rnvblk[4];
    rny = rnvblk[5];
    rlat1 *= DTR / 2.;
    rlat2 *= DTR / 2.;
    rlon1 *= DTR;
    rlon2 *= DTR;
    clon = rlov * DTR;

    /*
     * Compute the cone constant.
     */
    psi1 = HALFPI + sign * phi1 * DTR;
    psi2 = HALFPI + sign * phi2 * DTR;
    if ( G_DIFF ( phi1, phi2 ) ) {
	cc = cos ( psi1 );
    } else {
	cc = log ( sin ( psi2 ) / sin ( psi1 ) );
	cc /= log ( tan ( psi2 / 2. ) / tan ( psi1 / 2. ) );
    }
    re = RADIUS / cc;
    tan1 = pow ( tan ( PI4TH + sign * rlat1 ), cc );
    tan2 = pow ( tan ( PI4TH + sign * rlat2 ), cc );
    dlon1 = ( rlon1 - clon ) * cc;
    dlon2 = ( rlon2 - clon ) * cc;
    x1 = re * tan1 * sin ( dlon1 );
    y1 = sign * re * tan1 * cos ( dlon1 );
    x2 = re * tan2 * sin ( dlon2 );
    y2 = sign * re * tan2 * cos ( dlon2 );
    alfa = sin ( psi1 ) / pow ( tan ( psi1 / 2. ), cc );
    dx = ( x2 - x1 ) * alfa / ( rnx - 1. );
    dy = ( y2 - y1 ) * alfa / ( rny - 1. );

    /*
     * Fill bytes 21-23, 24-26.
     */
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
     * Fill bytes 27 and 28.
     */
    if ( sign < 0.0F ) {
	cgds[index++] = (unsigned char) (0);
    } else {
	cgds[index++] = (unsigned char) (128);
    }
    cgds[index++] = (unsigned char) (64);

    /*
     * Fill bytes 29-31, 32-34 with true latitudes.
     */
    ilat = G_ABS ( G_NINT ( trult1 * 1000. ) );
    nb = 3;
    gdigit ( &ilat, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -73;
	return;
    }
    if ( trult1 < 0.0F ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    ilat = G_ABS ( G_NINT ( trult2 * 1000. ) );
    nb = 3;
    gdigit ( &ilat, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -73;
	return;
    }
    if ( trult2 < 0.0F ) ibyts[2] += 128;
    for ( ii = 2; ii >= 0; ii-- ) cgds[index++] = (unsigned char) ( ibyts[ii] );

    /*
     * No bipolar projections are supported.  Fill remaining  bytes with zero.
     */
    for ( ii = 35; ii <= 42; ii++ ) cgds[index++] = (unsigned char) (0);

    return;
}
