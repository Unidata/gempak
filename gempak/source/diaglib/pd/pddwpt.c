#include "pd.h"

void pd_dwpt ( const float *rmix, const float *pres, const int *np,
               float *dwpc, int *iret )
/************************************************************************
 * pd_dwpt								*
 *									*
 * This subroutine computes DWPC from MIXR and PRES.  The following	*
 * equation is used:							*
 *									*
 *   DWPC = ALOG (E / 6.112) * 243.5 / ( 17.67 - ALOG (E / 6.112) )	*
 *									*
 *        E = vapor pressure						*
 *          = e / ( 1.001 + ( (PRES - 100.) / 900. ) * .0034 ) 		*
 *        e = ( PRES * MIXR ) / ( .62197 + MIXR ) 			*
 *									*
 * Bolton.								*
 *									*
 * This subroutine also computes TMPC from MIXS and PRES.		*
 *									*
 * pd_dwpt  ( rmix, pres, np, dwpc, iret )				*
 *									*
 * Input parameters:							*
 *	*rmix		const float	Mixing ratio in g/kg		*
 *	*pres		const float 	Pressure in millibars		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*dwpc		float   	Dewpoint in Celsius		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 2/90	GEMPAK 5				*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float ratio, e;
    int i, npt;
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
        if ( ( ERMISS ( rmix [i] ) ) || ( rmix [i] <= 0.0F ) ||
     	     ( ERMISS ( pres [i] ) ) || ( pres [i] <= 0.0F ) ) {
            dwpc [i] = RMISSD;
        } else {
            /*
             * Convert G/KG to G/G if neccessary.
             */
            ratio = rmix [i];
	    if ( ratio > .05F ) ratio /= 1000.0F;

            /*
             * Calculate vapor pressure from mixing ratio and pressure.
             */
	    e = ( pres [i] * ratio ) / ( .62197F + ratio );

            /*
             * Correct vapor pressure.
             */
	    e /= ( 1.001F + ((pres [i] - 100.0F) / 900.0F) * .0034F );

            /*
             * Calculate dewpoint.
             */
	    dwpc [i] = log (e/6.112F) * 243.5F / (17.67F - log (e/6.112F));
        }
    }

    return;
}
