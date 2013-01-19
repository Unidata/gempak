#include "pd.h"

void pd_plcl ( const float *pres, const float *tmpc, const float *dwpc,
               const int *np, float *plcl, int *iret )
/************************************************************************
 * pd_plcl								*
 *									*
 * This subroutine computes the Lifting Condensation Level Pressure 	*
 * from PRES, TMPC, DWPC.  The temperature of the LCL must be 		*
 * calculated as an intermidiate quantity.				*
 *									*
 * pd_plcl ( pres, tmpc, dwpc, np, plcl, iret )				*
 *									*
 * Input parameters:							*
 *	*pres		const float	Pressure in millibars		*
 *	*tmpc		const float	Temperature in Celsius		*
 *	*dpwc		const float	Dewpoint in Celsius		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*plcl		float		Pressure at LCL			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * S. Chiswell/Unidata	10/02		Created				*
 * R. Tian/SAIC		 9/05		Translated from FORTRAN		*
 ************************************************************************/
{
    float tmpk, tlcl;
    int npt, n1, i, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;
    n1 = 1;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
	/*
	 * Check for missing data.
	 */
	if ( ( ERMISS ( pres [i] ) ) || ( ERMISS ( tmpc [i] ) ) ||
     	     ( ERMISS ( dwpc [i] ) ) || ( pres [i] <= 0.0F ) ) {
		plcl [i] = RMISSD;
	} else {
	    /*
	     * Compute temperature at LCL.
	     */
            tmpk = tmpc [i] + TMCK;
	    pd_tlcl ( &tmpc [i], &dwpc [i], &n1, &tlcl, &ier );
            plcl [i] = pres [i] * pow ( tlcl/tmpk, 1/RKAPPA );
	}
    }

    return;
}
