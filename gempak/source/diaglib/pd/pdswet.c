#include "pd.h"

void pd_swet ( const float *t850, const float *td850, const float *t500,
               const float *spd850, const float *spd500,
	       const float *dir850, const float *dir500, const int *np,
	       float *swet, int *iret )
/************************************************************************
 * pd_swet								*
 *									*
 * This subroutine computes SWEAT index. Winds must be input in 	*
 * meters/sec. They are converted to knots for the computation.		*
 * The following equation is used:					*
 *									*
 *	SWET = SWEAT index						*
 *	     = 12 * td850 + 20 * term2 + 2 * skt850 + skt500 + shear	*
 *									*
 * where	TERM2 = MAX ( TOTL - 49, 0 )				*
 *		TOTL = Total totals index				*
 *		SKT850 = 850 mb wind speed in knots			*
 *		SKT500 = 500 mb wind speed in knots			*
 *		SHEAR = 125 * [SIN ( DIR500 - DIR850 ) + 0.2]		*
 *									*
 * If TD850 is negative, then TD850 is set to 0. SHEAR is set to 0 if	*
 * any of the following conditions are met:				*
 *									*
 *	wind direction at 850mb is < 130 or > 250 degrees		*
 *	wind direction at 500mb is < 210 or > 310 degrees		*
 *	DIR500 - DIR850 < 0 						*
 *	SKT500 < 15 or SKT850 < 15 knots				*
 *									*
 * Miller, R.C., 1972: Notes on Severe Storm Forecasting Procedures of	*
 * the Air Force Global Weather Central, AWS Tech. Report 200.		*
 *									*
 * pd_swet ( t850, td850, t500, spd850, spd500, dir850, dir500, np,	*
 *	     swet, iret )						*
 *									*
 * Input parameters:							*
 *	*t850		const float	850 mb temperature in Celsius	*
 *	*td850		const float	850 mb dewpoint in Celsius	*
 *	*t500		const float	500 mb temperature in Celsius	*
 *	*spd850		const float	850 mb wind speed in m/sec	*
 *	*spd500		const float	500 mb wind speed in m/sec	*
 *	*dir850		const float	850 mb wind direction		*
 *	*dir500		const float	500 mb wind direction		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*swet		float		Sweat Index			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * D. Keiser/GSC	 7/95						*
 * J. Whistler/NCEP	 1/96	Changed speed check to use knots	*
 * K. Brill/HPC		 5/02	Eliminate scratch arrays		*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float skt850, skt500, totl, term2, dif, shear, dwp850;
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
	if ( ( ERMISS ( t850   [i] ) ) || ( ERMISS ( td850  [i] ) ) ||
     	     ( ERMISS ( t500   [i] ) ) || ( ERMISS ( spd850 [i] ) ) ||
     	     ( ERMISS ( spd500 [i] ) ) || ( ERMISS ( dir850 [i] ) ) ||
     	     ( ERMISS ( dir500 [i] ) ) ) {
	    swet [i] = RMISSD;
	} else {
	    /*
	     * Convert speed to knots; compute total totals index.
	     */
	    skt850 = 1.9425F * spd850 [i];
	    skt500 = 1.9425F * spd500 [i];
	    totl = ( t850 [i] - t500 [i] ) + ( td850 [i] - t500 [i] );

	    /*
	     * Calculate term2 from the total totals index. 
	     * If < 49, set term to 0.
	     */
	    term2  = totl - 49.0F;
	    if ( totl < 49.0F ) term2 = 0.0F;

	    /*
	     * Compute shear term.
	     */
	    dif = dir500 [i] - dir850 [i];
	    if ( dif > 0.0F ) {
	        shear = 125.0F * ( sin ( dif * DTR ) + 0.2F );
	    } else {
	        shear = 0.0F;
	    }

	    /*
	     * Make various wind checks.
	     */
	    if (( dir850 [i] < 130.0F ) || ( dir850 [i] > 250.0F )) 
	        shear = 0.0F;
	    if (( dir500 [i] < 210.0F ) || ( dir500 [i] > 310.0F ))
	        shear = 0.0F;
	    if (( skt500 < 15.0F  ) || ( skt850 < 15.0F  ))
	        shear = 0.0F;

	    /*
	     * Check for subzero dewpoint.
	     */
	    dwp850 = td850 [i];
	    if ( dwp850 < 0.0F ) dwp850 = 0.0F;

	    /*
	     * Calculate the SWEAT index.
	     */
	    swet [i] = 12.0F * dwp850 + 20.0F * term2 + 2.0F *
     	        skt850 + skt500 + shear ;
	}
    }

    return;
}
