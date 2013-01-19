#include "pd.h"

void pd_inmm  ( const float *xinch, const int *np, float *xmm, 
                int *iret )
/************************************************************************
 * pd_inmm								*
 *									*
 * This subroutine converts inches to millimeters using:		*
 *									*
 *			XMM = XINCH * 25.4				*
 *									*
 * pd_inmm  ( xinch, np, xmm, iret )					*
 *									*
 * Input parameters:							*
 *	*xinch		const float	Inches				*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*xmm		float		Millimeters			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/NMC		01/92						*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    int npt, i;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
        /*
         * Check for missing data.
         */
        if ( ERMISS ( xinch [i] ) ) {
		xmm [i] = RMISSD;
        } else {
	    xmm [i] = xinch [i] * 25.4F;
	}
    }

    return;
}
