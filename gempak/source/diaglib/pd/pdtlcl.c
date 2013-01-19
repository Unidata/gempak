#include "pd.h"

void pd_tlcl ( const float *tmpc, const float *dwpc, const int *np,
               float *tlcl, int *iret )
/************************************************************************
 * pd_tlcl								*
 *									*
 * This subroutine computes the Lifting Condensation Level Temperature 	*
 * from TMPC, DWPC.  							*
 *									*
 * pd_tlcl ( tmpc, dpwc, np, tlcl, iret )				*
 *									*
 * Input parameters:							*
 *	*tmpc		const float	Temperature in Celsius		*
 *	*dwpc		const float	Dewpoint in Celsius		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*tlcl		float		Temperature at LCL		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * S. Chiswell/Unidata	 9/03		Created				*
 * R. Tian/SAIC		 9/05		Translated from FORTRAN		*
 ************************************************************************/
{
    float tmpk, dwpk;
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
	if ( ( ERMISS ( tmpc [i] ) ) || ( ERMISS ( dwpc [i] ) ) ||
	     ( tmpc [i] < -TMCK ) || ( dwpc [i] < -TMCK ) ) {
 	    tlcl [i] = RMISSD;
	} else {
	    /*
	     * Compute temperature at LCL.
	     */
            tmpk = tmpc [i] + TMCK;
	    dwpk = dwpc [i] + TMCK;
            tlcl [i] = ( 800.0F * ( dwpk - 56.0F ) /
	               ( 800.0F + ( dwpk - 56.0F ) *
		       log ( tmpk / dwpk ) ) ) + 56.0F;
	}
    }

    return;
}
