#include "pd.h"

void pd_thte ( const float *pres, const float *tmpc, const float *dwpc,
               const int *np, float *thte, int *iret )
/************************************************************************
 * pd_thte								*
 *									*
 * This subroutine computes THTE from PRES, TMPC, DWPC.  In the		*
 * calculation, MIXR depends on PRES and DWPC; TLCL depends on 		*
 * TMPC and DWPC.  The following equation is used:			*
 *									*
 *           THTE = THTAM * EXP [ ( 3.376/TLCL - .00254 ) *		*
 *                                ( MIXR * ( 1 + .81*.001*MIXR ) ) ]	*
 *									*
 *                THTAM = potential temperature of moist air		*
 *                      = TMPK * (1000 / PRES) ** E			*
 *                    E = RKAPPA * ( 1 - ( .28 * .001 * MIXR ) )	*
 *									*
 * pd_thte ( pres, tmpc, dwpc, np, thte, iret )				*
 *									*
 * Input parameters:							*
 *	*pres		const float	Pressure in millibars		*
 *	*tmpc		const float	Temperature in Celsius		*
 *	*dwpc		const float	Dewpoint in Celsius		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*thte		float		Equivalent potential temp in K	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * G. Krueger/EAI        4/96   Replaced C->K constant with TMCK	*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float vapr, corr, e, rmix, thtam, tmpk, tlcl, dwpk;
    int npt, i;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i <  npt; i++ ) {
	/*
	 * Check for missing data.
	 */
	if ( ( ERMISS ( pres [i] ) ) || ( ERMISS ( tmpc [i] ) ) ||
     	     ( ERMISS ( dwpc [i] ) ) || ( pres [i] <= 0.0F ) ) {
	    thte [i] = RMISSD;
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
	    corr = ( 1.001F + ( ( pres [i] - 100.0F ) / 900.0F ) * .0034F );
	    e = corr * vapr;

	    /*
	     * Test for unphysical case of large E at low PRES.
	     */
	    if ( e > ( .5F * pres [i] ) ) {
	    	thte [i] = RMISSD;
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
	    	thte [i] = thtam * exp ( e );
	    }
	}
    }

    return;
}
