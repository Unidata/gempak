#include "pd.h"

void pd_sped ( const float *uwnd, const float *vwnd, const int *np,
               float *sped, int *iret )
/************************************************************************
 * pd_sped 								*
 *									*
 * This subroutine computes SPED from UWND and VWND.  The following	*
 * equation is used:				 			*
 * 									*
 *          SPED = SQRT ( (UWND**2) + (VWND**2) )			*
 *									*
 * pd_sped ( uwnd, vwnd, np, sped, iret )				*
 *									*
 * Input parameters:							*
 *	*uwnd		const float	U component			*
 *	*vwnd		const float	V component			*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*sped		float		Wind speed 			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float uuu, vvv;
    int npt, i;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
        uuu = uwnd [i];
        vvv = vwnd [i];

  	/*
	 * Check for missing data.
	 */
        if ( ERMISS ( uuu ) || ERMISS ( vvv ) ) {
	    sped [i] = RMISSD;
	} else {
	    sped [i] = sqrt ( uuu * uuu + vvv * vvv );
	}
    }

    return;
}
