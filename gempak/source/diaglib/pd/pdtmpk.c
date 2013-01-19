#include "pd.h"

void pd_tmpk ( const float *pres, const float *thta, const int *np,
               float *tmpk, int *iret )
/************************************************************************
 * pd_tmpk								*
 *									*
 * This subroutine computes TMPK from pressure and potential		*
 * temperature.  The following equation is used:			*
 *									*
 *                TMPK = THTA * ( P / 1000 ) ** RKAPPA			*
 *									*
 * pd_tmpk ( pres, thta, np, tmpk, iret )				*
 *									*
 * Input parameters:							*
 *	*pres		const float	Pressure in millibars		*
 *	*thta		const float	THETA in Kelvin			*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*tmpk		float		Temperature in Kelvin		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/GSC		09/89						*
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
        if ( ERMISS ( pres [i] ) || ERMISS ( thta [i] ) ) {
	    tmpk [i] = RMISSD;
	} else {
	    tmpk [i] = thta [i] * pow ( pres [i] / 1000.0F, RKAPPA );
	}
    }

    return;
}
