#include "pd.h"

void pd_relh ( const float *tmpc, const float *dwpc, const int *np,
               float *relh, int *iret )
/************************************************************************
 * pd_relh								*
 *									*
 * This subroutine computes RELH from TMPC and DWPC.  The following	*
 * equation is used:							*
 *									*
 *               RELH  =  VAPR / VAPS * 100				*
 *									*
 *                     VAPR = vapor pressure				*
 *                          = PR_VAPR ( DWPC )				*
 *                     VAPS = saturation vapor pressure			*
 *                          = PR_VAPR ( TMPC )				*
 *									*
 * pd_relh ( tmpc, dwpc, np, relh, iret )				*
 *									*
 * Input parameters:							*
 *	*tmpc		const float	Temperature in Celsius		*
 *	*dwpc		const float	Dewpoint in Celsius		*
 *	*npt		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*relh		float		Relative humidity in percent	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * M. Linda/GSC		 9/97	Corrected right border of prologue	*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float e, es;
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
	if ( ( ERMISS ( tmpc [i] ) ) || ( ERMISS ( dwpc [i] ) ) ||
     	     ( tmpc [i] < -240.0F )  || ( dwpc [i] < -240.0F ) ) {
	    relh [i] = RMISSD;
	} else {
	    /*
	     *Find the vapor pressure .
	     */
	    e = 6.112F * exp ( ( 17.67F * dwpc [i] ) / ( dwpc [i] + 243.5F ) );

	    /*
	     * Find the saturated vapor pressure.
	     */
	    es = 6.112F * exp ( ( 17.67F * tmpc [i] ) / ( tmpc [i] + 243.5F ) );

	    /*
	     * Calculate humidity.
	     */
	    relh [i] = ( e / es ) * 100.0F;
	}
    }

    return;
}
