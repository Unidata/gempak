#include "pd.h"

void pd_mixr ( const float *dwpc, const float *pres, const int *np,
               float *rmix, int *iret )
/************************************************************************
 * pd_mixr								*
 *									*
 * This subroutine computes MIXR from DWPC and PRES.  The following	*
 * equation is used:							*
 *									*
 *         MIXR = .62197 * ( e / ( PRES - e ) ) 			*
 *									*
 *              e    =  VAPR * corr					*
 *              corr =  (1.001 + ( ( PRES - 100. ) / 900. ) * .0034)	*
 *									*
 * This function can also be used for the following computations:	*
 *            MIXS from TMPC and PRES					*
 *            SMXR from DWPC and PALT					*
 *            SMXS from TMPC and PALT					*
 * Note: This function now computes the mixing ratio in g/g.		*
 *									*
 * pd_mixr ( dwpc, pres, np, rmix, iret )				*
 *									*
 * Input parameters:							*
 *	*dwpc		const float	Dewpoint in Celsius		*
 *	*pres		const float	Pressure in millibars		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*rmix		float		Mixing ratio in g/kg		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * M. desJardins/NMC	 3/92	Compute g/g rather than g/kg		*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float vapr, corr, e;
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
        if ( ( ERMISS ( dwpc [i] ) ) ||
             ( ERMISS ( pres [i] ) ) ||
             ( dwpc [i] < -240.0F ) ) {
	    rmix [i] = RMISSD;
        } else {
	    /*
	     * Calculate vapor pressure.
	     */
	    vapr = 6.112F * exp ( ( 17.67F * dwpc [i] ) / 
     			          ( dwpc [i] + 243.5F ) );
	    /*
	     * CORR is a correction to the vapor pressure
	     * since the atmosphere is not an ideal gas.
	     */
	    corr = ( 1.001F + ( ( pres [i] - 100.0F) / 900.0F) * 0.0034F );
	    e = corr * vapr;

	    /*
 	     * Test for unphysical case of large E at low PRES.
	     */
	    if ( e > ( 0.5F * pres [i] ) ) {
	        rmix [i] = RMISSD;
	    } else {
	        /*
	    	 * Calculate mixing ratio.
		 */
	    	rmix [i] = 0.62197F * ( e / ( pres [i] - e ) ) ;
	    }
	}
    }

    return;
}
