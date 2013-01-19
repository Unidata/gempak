#include "pd.h"

void pd_rhdp ( const float *tmpc, const float *relh, const int *np,
               float *dwpc, int *iret )
/************************************************************************
 * pd_rhdp								*
 *									*
 * This subroutine computes DWPC from TMPC and RELH.  The following	*
 * equation is used:							*
 *									*
 *           DWPC = 243.5 * LN (6.112) - 243.5 * LN (VAPR) /		*
 *                  ( LN (VAPR) - LN (6.112) - 17.67 )			*
 *									*
 *                VAPR = VAPS * RELH					*
 *                VAPS = saturation vapor pressure			*
 *                     = PR_VAPR ( TMPC )				*
 *									*
 * Note: If DWPC is less than -190 degrees C, it is treated as		*
 * missing data.							*
 *									*
 * pd_rhdp ( tmpc, relh, np, dwpc, iret )				*
 *									*
 * Input parameters:							*
 *	*tmpc		const float	Temperature in Celsius		*
 *	*relh		const float	Relative humidity in percent	*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*dwpc		float		Dewpoint in Celsius		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float vaps, vapr;
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
	if ( ( ERMISS ( tmpc [i] ) ) || ( ERMISS ( relh [i] ) ) ||
    	     ( tmpc [i] < -240.0F ) ) {
	    dwpc [i] = RMISSD;
	} else {
	    /*
	     * Calculate saturation vapor pressure; test for existence.
	     */
	    vaps = 6.112F * exp ( ( 17.67F * tmpc [i] ) / 
	        ( tmpc [i] + 243.5F ) );

	    /*
	     * Calculate vapor pressure.
	     */
	    vapr = relh [i] * vaps / 100.0F;

	    /*
	     * Calculate dewpoint.  The VAPR test prevents LOG blowups.
	     */
	    if ( vapr < 1.E-30 ) {
	    	dwpc [i] = RMISSD;
	    } else {
	   	dwpc [i] = 243.5F * ( log (6.112) - log (vapr) ) /
     		    ( log (vapr) - log (6.112) - 17.67F );
	    }
	}
    }

    return;
}
