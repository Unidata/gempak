#include "dg.h"

#define MININC		30
#define FMINS		( 60.0 / MININC )
#define IDMINS		( MININC / 10 )
#define NBTLON		( 360 * FMINS + 1 )

/*
 * This code is the equivalent of the intrinsic function BTEST in Fortran.
 * It compares the j-th position bit of a number with 1.
 */
static int btest(int i, int j) {
    i >>= j;
    return ( i % 2 != 0 ) ? G_TRUE : G_FALSE;
}

void dg_lncx ( int *iret )
/************************************************************************
 * dg_lncx                                                              *
 *                                                                      *
 * This function sets up the land-sea array.				*
 *                                                                      *
 * The array is set to TRUE if given (x,y) pair is closest to a land  	*
 * point in the ls (land/sea) bit array.  This array is actually a      *
 * (non-ocean/ocean) bit array.                                         *
 *									*
 * The land-sea array is stored in an internal grid to which LNDSEA in	*
 * DGCMN.CMN points.							*
 *									*
 * HISTORICAL NOTE:							*
 *    THE BLOCK DATA PROGRAM RESPONSIBLE FOR INITIALIZING THE LS ARRAY  *
 * IS GENERATED VIA THE MEMBER 'NWS.WD22.DWP.NMCIDAS.SOURCE(GLANSEA)'.  *
 * THE PARAMETER MININC=N (THE MINUTE INCREMENT = 10, 20, 30, 60, ETC.) *
 * MUST BE CONSISTENT BETWEEN THIS FUNCTION AND THE GENERATING PROGRAM. *
 *                                                                      *
 * dg_lncx ( iret )							*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 7/98						*
 * K. Brill/HPC		 5/02	Store mask in an internal grid		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float flon, alon, alat, bitlat, bitlon, bitpos, aword;
    int llnd;
    int iword, ibit, i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check to see if the land/sea mask has been computed.
     */
    dg_cndg ( "LND_SEA_MASK", &_dglndc.lndsea, &llnd, iret );
    if ( llnd == G_TRUE ) return;

    /*
     * Make sure DG lat/lons have been computed
     */
    dg_ltln ( iret );
    if ( *iret != 0 ) return;

    /*
     * Make sure DG land/sea mask has been read in
     */
    dg_lncr ( iret );
    if ( *iret != 0 ) return;

    for ( i = 0; i < _dgfile.kxyd; i++ ) {
	/*
	 * Switch longitude positive east to longitude positive west
	 */
	flon = - ( _dggrid.dgg[_dgfile.idglon-1].grid[i] * RTD );
	alon = fmod ( (double)flon + 360.0, 360.0 );
	if (G_DIFFT(alon, 0.0F, GDIFFD)) alon = 360.0F;
	alat = _dggrid.dgg[_dgfile.idglat-1].grid[i] * RTD;

	/*
	 * Figure land-sea to nearest point
	 */
	bitlat = G_NINT ( ( alat + 90 ) * FMINS ) * NBTLON;
	bitlon = G_NINT ( alon * FMINS );
	bitpos = bitlat + bitlon;
	aword  = bitpos / 32.0 + 1;
	iword  = (int)aword;
	ibit   = (int)( ( aword - iword ) * 32.0 );

	if ( btest ( _dglndc.ls[iword-1], ibit ) == G_TRUE ) {
	    _dggrid.dgg[_dglndc.lndsea-1].grid[i] = 1;
	} else {
	    _dggrid.dgg[_dglndc.lndsea-1].grid[i] = 0;
	}
    }

    return;
}
