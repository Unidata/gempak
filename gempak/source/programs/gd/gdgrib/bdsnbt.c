#include "gdgrib.h"

void bds_nbt ( const float *rmin, const float *rmax, const float *rdb,
               int *nmbts, int *iscale, float *rmn, int *iret )
/************************************************************************
 * bds_nbt								*
 *									*
 * This subroutine computes the number of packing bits given the	*
 * maximum number (< 50) of significant digits to preserve or the	*
 * binary precision to store the data.  The binary precision is given	*
 * as zero, a negative integer, or as a postitive integer greater than	*
 * or equal to 50.  If the binary precision is given, ISCALE will	*
 * always be zero in this case.						*
 *									*
 * The binary precision translates as follows:				*
 *     53  =>  store data to nearest 8					*
 *     52  =>  store data to nearest 4					*
 *     51  =>  store data to nearest 2					*
 *     50  =>  store data to nearest 1					*
 *      0  =>  store data to nearest 1					*
 *     -1  =>  store data to nearest 1/2				*
 *     -2  =>  store data to nearest 1/4				*
 *     -3  =>  store data to nearest 1/8				*
 *									*
 * Note that RDB - 50 give the nearest whole power of two for binary	*
 * precision.								*
 *									*
 * Note that a fractional number of significant digits is allowed.	*
 *									*
 * bds_nbt ( rmin, rmax, rdb, nmbts, iscale, rmn, iret )		*
 *									*
 * Input parameters:							*
 *	*rmin 		const float	Minimum value			*
 *	*rmax		const float	Maximum value			*
 *	*rdb		const float	Maximum # of significant digits	*
 *					  OR binary precision if < 0	*
 *									*
 * Output parameters:							*
 *	*nmbts		int		Number of bits for packing	*
 *	*iscale		int		Power of 10 scaling to use	*
 *	*rmn		float		Rounded miniumum		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC		06/92						*
 * K. Brill/EMC		12/95	Added binary precision; added RMN	*
 * K. Brill/EMC		 1/97	Add .5 in rr= & rng2= for better rnd off*
 * K. Brill/EMC		 1/97	Use 10**iscale in rounding the min	*
 * K. Brill/HPC		 8/99	Name change for use in GDGRIB		*
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 ************************************************************************/
{
    float range, po, pp, rr, rng2, tp, x, rln2 = 0.69314718;
    int ibin, ixp, irmn;
/*---------------------------------------------------------------------*/
    *iret = 0;
    *iscale = 0;
    *rmn = *rmin;
    range = *rmax - *rmin;
    if ( range <= 0.0F ) {
	*nmbts = 8;
	return;
    }

    if ( *rdb > 0.0 && *rdb < 50. ) {
	pp = log10 ( range );
	po = (float) ( (int) ( pp ) );
	if ( range < 1.00 ) po -= 1.;
	po += ( 1. - (*rdb) );
	*iscale = - (int) ( po );
	rr = range * pow ( 10., -po ) + .5;
	*nmbts = (int) ( log ( rr ) / rln2 ) + 1;
    } else {
	ibin = G_NINT ( -(*rdb) );
	if ( ibin <= -50. ) ibin += 50;
	rng2 = range * pow ( 2., ibin ) + .5;
	*nmbts = (int) ( log ( rng2 ) / rln2 ) + 1;
    }
    if ( *nmbts <= 0 ) {
	*iret = 1;
	*nmbts = 8;
    }

    /*
     * Compute RMN, the first packable value less than or equal to
     * RMIN.
     */
    tp = pow ( 10., *iscale );
    x = ( log ( range * tp ) - log ( pow ( 2, *nmbts ) - 1. ) ) / rln2;
    ixp = (int) ( x );
    if ( ! G_DIFF ( (float) ( ixp ), x ) && x > 0. ) ixp += 1;
    irmn = G_NINT ( ( *rmin * tp ) / pow ( 2., ixp ) );
    *rmn = (float) ( irmn ) * pow ( 2., ixp );
    if ( *rmn > *rmin * tp ) *rmn -=  pow ( 2., ixp );
    *rmn /= tp;

    return;
}
