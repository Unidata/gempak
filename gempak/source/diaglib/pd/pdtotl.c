#include "pd.h"

void pd_totl ( const float *t850, const float *td850, const float *t500,
               const int *np, float *totl, int *iret )
/************************************************************************
 * pd_totl                                                              *
 *                                                                      *
 * This subroutine computes TOTL from t850, td850, and t500.		*
 * The following equation is used:                                      *
 *                                                                      *
 *	TOTL = the total totals index					*
 *	     = (t850 - t500) + (td850 - t500)				*
 *                                                                      *
 * pd_totll( t850, td850, t500, np, totl, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *t850		const float	850 mb temperature in Celsius   *
 *      *td850		const float	850 mb dewpoint in Celsius      *
 *      *t500		const float	500 mb temperature in Celsius   *
 *      *np             const int	Number of points                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *totl		float		The Total Totals Index 		*
 *      *iret           int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * D. Keiser/GSC        7/95                                            *
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
	if ( ( ERMISS ( t850 [i] ) ) || ( ERMISS ( td850 [i] ) ) ||
             ( ERMISS ( t500 [i] ) ) ) {
	    totl [i] = RMISSD;
	} else {
	    /*
	     * Find the total totals index.
	     */
	    totl [i] = ( t850 [i] - t500 [i] ) + ( td850 [i] - t500 [i] );
	}
    }
}
