#include "pd.h"

void pd_reli ( const float *tmpc, const float *dwpc, const int *np,
               float *reli, int *iret )
/************************************************************************
 * pd_reli								*
 *									*
 * This subroutine computes RELI from TMPC and DWPC.  The following	*
 * equation is used:							*
 *									*
 *               RELI  =  VAPR / VAPSi * 100				*
 *									*
 *                     VAPR = vapor pressure				*
 *                          = PR_VAPR ( DWPC )				*
 *                    VAPSi = saturation vapor pressure wrt ice		*
 *                          = PR_VAPR ( TMPC )				*
 *									*
 * pd_reli ( tmpc, dwpc, np, reli, iret )				*
 *									*
 * Input parameters:							*
 *	*tmpc		const float	Temperature in Celsius		*
 *	*dwpc		const float	Dewpoint in Celsius		*
 *	*npt		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*reli		float		Relative humidity in percent	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * S. Chiswell/Unidata		 1/07					*
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
	 * Check for missing data, and valid data range.
	 */
	if ( ( ERMISS ( tmpc [i] ) ) || ( ERMISS ( dwpc [i] ) ) ||
	     ( tmpc [i] < -240.0F ) || ( dwpc [i] < -240.0F ) ) {
	    reli [i] = RMISSD;
	} else {

	    /*
	     * Calculate humidity.
	     */
	    reli [i] = pr_reli ( &tmpc[i], &dwpc[i] );
	}
    }

    return;
}
