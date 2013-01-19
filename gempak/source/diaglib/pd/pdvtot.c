#include "pd.h"

void pd_vtot ( const float *t500, const float *t850, const int *np,
               float *vtot, int *iret )
/************************************************************************
 * pd_vtot								*
 *									*
 * This subroutine computes VTOT from t500 and t850.  The following	*
 * equation is used:							*
 *									*
 *	VTOT = vertical totals index					*
 *	     = t850 - t500						*
 *									*
 * pd_vtot ( t500, t850, np, vtot, iret ) 				*
 *									*
 * Input parameters:							*
 *	*t500		const float	500 mb temperature in Celsius	*
 *	*t850		const float	850 mb temperature in Celsius	*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*vtot		float		Vertical Totals Index		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * D. Keiser/GSC	6/95						*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated froom FORTRAN		*
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
    	if ( ( ERMISS ( t500 [i] ) ) || ( ERMISS ( t850 [i] ) ) ) {
	    vtot [i] = RMISSD;
	} else {
	    /*
	     * Find the vertical totals index.
	     */
	    vtot [i] = t850 [i] - t500 [i];
	}
    }
    
    return;
}
