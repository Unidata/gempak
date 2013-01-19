#include "pd.h"

void pd_tvrk ( const float *tmpk, const float *rmix, const int *np,
               float *tvrk, int *iret )
/************************************************************************
 * pd_tvrk 								*
 *									*
 * This subroutine computes TVRK from TMPK, RMIX.  The following	*
 * equation is used:							*
 *									*
 *   TVRK = TMPK * (1 + .001 * MIXR / .62197) / (1 + .001 * MIXR)	*
 *	                                                                *
 * Note that this subroutine requires different inputs than the		*
 * function PR_TVRK.							*
 *									*
 * pd_tvrk ( tmpk, rmix, np, tvrk, iret )				*
 *									*
 * Input parameters:							*
 *	*tmpk		const float	Temperature in Kelvin		*
 *	*rmix		const float	Mixing ratio in g/g		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*tvrk		float		Virtual temp in Kelvin		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 2/90	GEMPAK 5				*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
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
	if ( ERMISS ( tmpk [i] ) ) {
	    tvrk [i] = RMISSD;
	} else if ( ERMISS ( rmix [i] ) ) {
	    /*
	     * If mixing ratio is missing, return temperature.
	     */
	    tvrk [i] = tmpk [i];
	} else {
	    tvrk [i] = tmpk [i] * ( 1.0F + rmix [i] / .62197F ) / 
     		       ( 1.0F + rmix [i] );
	}
    }
    
    return;
}
