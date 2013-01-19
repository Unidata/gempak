#include "pd.h"

void pd_thta ( const float *tmpc, const float *pres, const int *np,
               float *thta, int *iret )
/************************************************************************
 * pd_thta								*
 *									*
 * This subroutine computes THTA from TMPC and PRES using Poisson's	*
 * equation:								*
 *									*
 *            THTA = TMPK * ( 1000 / PRES ) ** RKAPPA			*
 *									*
 * It can also be used to compute STHA from TMPC and PALT, THTV from	*
 * TVRC and PRES, and THTV from TVRC and PALT.				*
 *									*
 * pd_thta ( tmpc, pres, np, thta, iret )				*
 *									*
 * Input parameters:							*
 *	*tmpc		const float	Temperature in Celsius		*
 *	*pres		const float	Pressure in millibars		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*thta		float		Potential temperature in K	*
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
    float tmpk;
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
    	if ( ( ERMISS ( tmpc [i] ) ) || ( ERMISS ( pres [i] ) ) || 
             ( pres [i] <= 0.0F ) ) {
	    thta [i] = RMISSD;
	} else {
	    /*
	     * Change temperature in Celsius to Kelvin.
	     */
	    tmpk = tmpc [i] + TMCK;

	    /*
	     * Calculate theta.
	     */
	    thta [i] = tmpk * pow ( 1000.0F / pres [i], RKAPPA ) ;
	}
    }

    return;
}
