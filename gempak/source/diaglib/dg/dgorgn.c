#include "dg.h"

void dg_orgn ( const float *olat, const float *olon, int *iret )
/************************************************************************
 * dg_orgn								*
 *									*
 * This subroutine sets the position of the origin.  The distance from	*
 * this point is used in the calculation of M, psuedo angular momentum.	*
 *									*
 * dg_orgn ( olat, olon, iret )						*
 *									*
 * Input parameters:							*
 *	*olat		const float	Latitude of origin		*
 *	*olon		const float	Longitude of origin		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC		 4/93						*
 * T. Piper/GSC		11/98	Updated prolog				*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int one, ier;
/*----------------------------------------------------------------------*/
    *iret   = 0;
    one = 1;

    _dgorig.orglat = *olat;
    _dgorig.orglon = *olon;

    /*
     * Compute grid relative position of this point.
     */
    gtrans ( sys_M, sys_G, &one, &_dgorig.orglat, &_dgorig.orglon,
             &_dgorig.orgxpt, &_dgorig.orgypt, iret,
	     strlen(sys_M), strlen(sys_G) );
    if ( *iret != 0 ) {
	er_wmsg ( "GEMPLT", iret, " ", &ier, strlen("GEMPLT"), strlen(" ") );
	*iret = -6;
	return;
    }

    return;
}
