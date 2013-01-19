#include "pd.h"

void pd_mmin  ( const float *xmm, const int *np, float *xinch,
                int *iret )
/************************************************************************
 * pd_mmin								*
 *									*
 * This subroutine converts millimeters to inches using:	 	*
 *									*
 *                  xinch = .0393701 * xmm				*
 *									*
 * pd_mmin  ( xmm, np, xinch, iret )					*
 *									*
 * Input parameters:							*
 *	*xmm		const float	Millimeters			*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*xinch		float		Inches				*
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
        if ( ERMISS ( xmm [i] ) ) {
            xinch [i] = RMISSD;
        } else {
            xinch [i] = xmm [i] * .0393701F;
        }
    }

    return;
}
