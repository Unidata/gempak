#include "pd.h"

void pd_tmst ( const float *thte, const float *pres, const int *np,
               float *tmst, int *iret )
/************************************************************************
 * pd_tmst								*
 *									*
 * This function computes TMST from THTE and PRES.  TMST is the 	*
 * parcel temperature at level PRES on the moist adiabat corresponding	*
 * to the input THTE.  THTE's value is valid at the input GLEVEL,	*
 * which is assumed to be in p-coordinates.				*
 * The computation uses an iterative Newton-Raphson technique.		*
 *									*
 * pd_tmst ( thte, pres, np, tmst, iret )				*
 *									*
 * Input parameters:							*
 *	*thte		const float	Equivalent potential temp in K  *
 *	*pres		const float	Pressure in millibars		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*tmst		float		Equivalent potential temp in K	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * J. Whistler/NSSFC	 6/95						*
 * K. Tyle/GSC		10/95	First guess set internally		*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN & impl PR_TMST	*
 ************************************************************************/
{
    float tguess, epsi, tgnu, tgnup, tenu, tenup, cor;
    int npt, n1, i, j, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;
    n1 = 1;
    epsi = .01F;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
	/*
	 * Check for missing data.
	 */
	if ( ( ERMISS ( thte [i] ) ) || ( ERMISS ( pres [i] ) ) ) {
	    tmst [i] = RMISSD;
	} else {
	    /*
	     * Compute the guess filed from an MIT scheme.
	     */
    	    tguess = (thte [i] - 0.5F * 
	             pow ( G_MAX ( thte [i] - 270.0F, 0.0F ), 1.05F ) ) *
                     pow ( pres [i] / 1000.0F, .2F );
	    tgnu = tguess - TMCK;

	    /*
	     * Set a limit of 100 iterations.  Compute TENU, TENUP, the
	     * THTE's at, one degree above the guess temperature.
	     * If failed to converge, return missing.
	     */
	    tmst [i] = RMISSD;
	    for ( j = 0; j < 100; j++ ) {
	        tgnup = tgnu + 1.0F;
		pd_thte ( &pres[i], &tgnu, &tgnu, &n1, &tenu, &ier );
		pd_thte ( &pres[i], &tgnup, &tgnup, &n1, &tenup, &ier );

		/*
		 * Check that the THTE's exist.
		 */
		if ( ERMISS ( tenu ) || ERMISS ( tenup ) ) {
		    break;
		}

		/*
		 * Compute the correction, DELTG; return on convergence.
		 */
          	cor = ( thte [i] - tenu ) / ( tenup - tenu );
           	tgnu += cor;
           	if ( ( cor < epsi ) && ( -cor < epsi ) ) {
		    tmst [i] = tgnu + TMCK;
		    break;
		}
	    }
	}
    }

    return;
}
