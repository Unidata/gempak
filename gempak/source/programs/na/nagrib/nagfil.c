#include "nagrib.h"

void nagfil ( const float *sg, const int *ixf, const int *iyf,
              const char *parm, float *fg, float *fhv, int *iret )
/************************************************************************
 * nagfil								*
 *									*
 * This subroutine fills a staggered ETA native grid.  Input array	*
 * SG may be the same as FG, but, FHV must be a separate array.		*
 *									*
 * nagfil ( sg, ixf, iyf, parm, fg, fhv, iret )				*
 *									*
 * Input parameters:							*
 *	*sg		const float	Staggered grid			*
 *	*ixf		const int	Filled grid x dimension		*
 *	*iyf		const int	Filled grid y dimension		*
 *	*parm		const char	Parameter name			*
 *									*
 * Output parameters:							*
 *	*fg		float		Filled grid			*
 *	*fhv		float		H/V flag:  h=1, v=0		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC		12/94						*
 * K. Brill/EMC		11/95	Added FHV				*
 * K. Brill/HPC		01/02	Use fhv array in NAGFLH & NAGFLV calls; *
 *				eliminate local large grid declaration  *
 * R. Tian/SAIC		 8/06	Recoded from Fortran			*
 ************************************************************************/
{
    int one, ier, i;
/*----------------------------------------------------------------------*/
    *iret = 0;
    one = 1;

    /*
     * Check for wind component.
     */
    if ( strcmp ( parm, "UREL" ) == 0 ||
         strcmp ( parm, "VREL" ) == 0 ) {
	nagflh ( &one, sg, ixf, iyf, fhv, &ier );
    } else {
	nagflv ( &one, sg, ixf, iyf, fhv, &ier );
    }

    /*
     * Move data to output array.
     */
    for ( i = 1; i <= (*ixf) * (*iyf); i++ ) {
	fg[i-1] = fhv[i-1];
	fhv[i-1] = i % 2;
    }

    return;
}
