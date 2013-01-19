#include "pd.h"

void pd_vpmr ( const float *vapr, const float *pres, const int *np,
               float *mixr, int *iret )
/************************************************************************
 * pd_vpmr								*
 *									*
 * This subroutine computes MIXR from VAPR and PRES.  The following	*
 * equation is used:							*
 *									*
 *         MIXR = ( .622 * VAPR ) / ( PRES - VAPR ) .			*
 *									*
 * pd_vpmr ( vapr, pres, np, mixr, iret )				*
 *									*
 * Input parameters:							*
 *	*vapr		const float	Vapor pressure in millibars	*
 *	*pres		const float	Pressure in millibars		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*mixr		float		Mixing ratio in g/g		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 5/91						*
 * T. Piper/GSC		 3/99	Corrected typo in prolog		*
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
    	if ( ( ERMISS ( vapr [i] ) ) || ( ERMISS ( pres [i] ) ) ) {
	    mixr [i] = RMISSD;
	} else {
	    /*
	     * Calculate mixing ratio.
	     */
	    mixr [i] = ( .622F * vapr [i] ) / ( vapr [i] - pres [i]  );
	}
    }

    return;
}
