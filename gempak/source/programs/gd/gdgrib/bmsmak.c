#include "gdgrib.h"

void bms_mak ( const float *grid, const int *igx, const int *igy,
               int *nbyts, unsigned char *cbms, int *iret )
/************************************************************************
 * bms_mak								*
 *									*
 * This subroutine makes the GRIB BMS from an input grid of data.	*
 *									*
 * bms_mak ( grid, igx, igy, nbyts, cbms, iret )			*
 *									*
 * Input parameters:							*
 *	*grid		const float	Grid data			*
 *	*igx		const int	Number of points in x dir	*
 *	*igy		const int	Number of points in y dir	*
 *									*
 * Input and output parameters:						*
 *	*nbyts		int		Input:  Max # of bytes for BMS	*
 *					Output: # of bytes in the BMS	*
 *									*
 * Output parameters:							*
 *	*cbms		unsigned char	Binary data section		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-41 = BMS section too long	*
 *					-42 = BMS allocation too small	*
 **									*
 * Log:									*
 * K. Brill/HPC		08/99						*	
 * K. Brill/HPC		 3/00	Avoid character assignment to itself	*
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
    double po2;
    int ibase, idx, kxky, nb, num0, ibyts[3], icnt, ipwr, ixx, ii, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    ibase = 256;
    kxky = (*igx) * (*igy);

    /*
     * Compute the length of the bit map section.
     */
    nb = kxky / 8;
    if ( kxky % 8 != 0 ) nb++;
    num0 = nb * 8 - kxky;
    nb += 6;
    if ( nb % 2 != 0 ) {
	nb++;
	num0 += 8;
    }
    if ( nb > *nbyts ) {
	*iret = -42;
	return;
    }
    *nbyts = nb;

    /*
     * Initialize all bytes to zero.
     */
    for ( ii = 0; ii < *nbyts; ii++ ) cbms[ii] = (unsigned char) (0);

    /*
     * Set the length in bytes 1--3.
     */
    idx = -1;
    nb = 3;
    gdigit ( nbyts, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -41;
	return;
    }
    for ( ii = 2; ii >= 0; ii-- )  cbms[++idx] = (unsigned char)( ibyts[ii] );

    /*
     * Set the number of unused bits in byte 4.
     */
    cbms[++idx] = (unsigned char) ( num0 );

    /*
     * Set flag for in-message bit map (bytes 5-6 = 0).
     */
    for ( ii = 0; ii < 2; ii++ ) cbms[++idx] = (unsigned char) (0);

    /*
     * Now generate the bit map itself.
     */
    icnt = 0;
    for ( ii = 0; ii < kxky; ii++ ) {
	if ( icnt % 8 == 0 ) {
	    idx++;
	    ipwr = 8;
	}
	ipwr--;
	if ( ! ERMISS ( grid[ii] ) ) {
	    ixx = (int) ( cbms[idx] );
	    po2 = pow ( 2, ipwr );
	    cbms[idx] = (unsigned char) ( ixx + (int)po2 );
	}
	icnt++;
    }

    return;
}
