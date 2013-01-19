#include "pd.h"

void pd_uvsd ( const float *uwnd, const float *vwnd, const int *np,
               float *sped, float *drct, int *iret )
/************************************************************************
 * pd_uvsd 								*
 *									*
 * This subroutine computes SPED and DRCT from UWND and VWND.  The	*
 * following equations are used:		 			*
 * 									*
 *             SPED = SQRT ( (UWND**2) + (VWND**2) )			*
 *             DRCT = ATAN2 ( -UWND, -VWND ) * RTD			*
 *									*
 * The input and output arrays may be the same.				*
 *									*
 * pd_uvsd ( uwnd, vwnd, np, sped, drct, iret )				*
 *									*
 * Input parameters:							*
 *	*uwnd		const float	U component			*
 *	*vwnd		const float	V component			*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*sped		float		Wind speed 			*
 *	*drct		float		Wind direction in degrees	*
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
    int npt, ii;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;

    /*
     * Loop through array.
     */
    for ( ii = 0; ii < npt; ii++ ) {
        uuu = uwnd[ii];
        vvv = vwnd[ii];

	/*
	 * Check for missing data.
	 */
	if ( ERMISS ( uuu ) || ERMISS ( vvv) ) {
	    sped[ii] = RMISSD;
	    drct[ii] = RMISSD;
	} else {
	    sped[ii] = sqrt ( uuu * uuu + vvv * vvv );
	    if ( G_DIFF(uuu, 0.0F) && G_DIFF(vvv, 0.0F) ) {
	      	drct[ii] = 0.0F;
	    } else {
	    	drct[ii] = atan2 ( -uuu, -vvv ) * RTD;
	    	if ( drct[ii] <= 0.0F ) drct[ii] +=  360.0F;
	    }
	}
    }

    return;
}
