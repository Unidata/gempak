#include "pd.h"

void pd_thwc ( const float *pres, const float *tmpc, const float *dwpc,
               const int *np, float *thwc, int *iret )
/************************************************************************
 * pd_thwc								*
 *									*
 * This subroutine computes wet bulb potential temperature in Celsius	*
 * from PRES, TMPC and DWPC.  The result is obtained by first		*
 * computing THTE of the the air parcel at level PRES.  Then the air	*
 * parcel is brought to 1000 mb moist adiabatically to get THWC.	*
 *									*
 * pd_thwc ( pres, tmpc, dwpc, np, thwc, iret ) 	              	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *pres		const float	Pressure in millibars           *
 *      *tmpc		const float	Temperature in Celsius          *
 *      *dwpc		const float	Dewpoint in Celsius             *
 *      *np             const int	Number of points                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *thwc		float		Wet-bulb pot. temp. in Celsius	*
 *      *iret           int		Return code                     *
 *                                        0 = normal return             *
 **									*
 * Log:									*
 * P. Bothwell/SPC       9/97   Theta-E changed to Theta-W subroutine   *
 * T. Lee/GSC		11/97	Renamed THTW to THWC; Used PR_TMST	*
 * T. Lee/GSC		12/97	Added missing THWC checks		*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float vapr, corr, e, rmix, tmpk, dwpk, thtam, tlcl, thte, p1000;
    int npt, n1, i, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;
    n1 = 1;
    p1000 = 1000.0F;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
	/*
	 * Check for missing data.
	 */
    	if ( ( ERMISS ( pres [i] ) ) || ( ERMISS ( tmpc [i] ) ) ||
     	     ( ERMISS ( dwpc [i] ) ) || ( pres [i] <= 0.0F ) ) {
	    thwc [i] = RMISSD;
	} else {
	    /*
	     * Compute mixing ratio; first calculate vapor pressure.
	     */
	    vapr = 6.112F * exp ( ( 17.67F * dwpc [i] ) / 
     	           ( dwpc [i] + 243.5F ) );

	    /*
	     * CORR is a correction to the vapor pressure
	     * since the atmosphere is not an ideal gas.
	     */
	    corr = ( 1.001F + ( ( pres [i] - 100.0F) / 900.0F) * .0034F );
	    e = corr * vapr;

	    /*
	     * Test for unphysical case of large E at low PRES.
	     */
	    if ( e > ( .5F * pres [i] ) ) {
	        thwc [i] = RMISSD;
	    } else {
		/*
	    	 * Calculate mixing ratio.
		 */
	    	rmix = .62197F * ( e / ( pres [i] - e ) ) * 1000.0F;

		/*
	    	 * Change Celsius to Kelvin.
		 */
	    	tmpk = tmpc [i] + TMCK;
	    	dwpk = dwpc [i] + TMCK;

		/*
		 * Calculate  THETA-M  (theta for moist air).
		 */
	        e = RKAPPA * ( 1.0F - ( .28F * .001F * rmix ) );
	    	thtam = tmpk * pow ( 1000.0F / pres [i], e );

		/*
	  	 * Find the temperature at the LCL.
		 */
         	tlcl = ( 1.0F / ( 1.0F / ( dwpk - 56.0F ) +
                       log ( tmpk / dwpk ) / 800.0F ) ) + 56.0F;
	    	e = ( ( 3.376F / tlcl ) - .00254F ) *
                    ( rmix * ( 1.0F + .81F * .001F * rmix ) );
	    	thte = thtam * exp ( e );

		/*
	    	 * Compute wet-bulb potential temperature.  
		 */
		pd_tmst ( &thte, &p1000, &n1, &thwc[i], &ier );
	    	if ( ! ERMISS ( thwc [i] ) )  thwc [i] -= TMCK;
	    }
	}
    }

    return;
}
