#include "pd.h"

void pd_shmr ( const float *spfh, const int *np, float *mixr, int *iret )
/************************************************************************
 * pd_shmr								*
 *									*
 * This subroutine computes MIXR from SPFH.  The following equation is	*
 * used:								*
 *									*
 *         MIXR = SPFC / ( 1 - SPFH ) .					*
 *									*
 * pd_shmr ( spfh, np, mixr, iret )					*
 *									*
 * Input parameters:							*
 *	*spfh		const float	Specific humidity in g/g	*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*mixr		float		Mixing ratio in g/g		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 5/91						*
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
	if ( ERMISS ( spfh [i] ) ) {
	    mixr [i] = RMISSD;
	} else {
	    /*
	     * Calculate mixing ratio.
	     */
	    mixr [i] = spfh [i] / ( 1.0F - spfh [i] );
	}
    }

    return;
}
