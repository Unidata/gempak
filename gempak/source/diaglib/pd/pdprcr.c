#include "pd.h"

static const float VALUE0 = -0.254F;

void pd_prcr ( const float *prate, const int *nhr, const int *np,
               float *prmm, int *iret ) 
/************************************************************************
 * pd_prcr								*
 *									*
 * This subroutine computes precipitation in mm from an instantaneous	*
 * precipitation rate using the following equation:			*
 *									*
 *		PRMM = PRATE * 60 * 60 * NHRS				*
 *									*
 * pd_prcr ( prate, nhr, np, prmm, iret )				*
 *									*
 * Input parameters:							*
 *	*prate		const float	Precipitation rate		*
 *	*nhr		const int	Accumulation time in hours	*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*prmm		float		Precipitation in millimeters	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/NMC	 4/94						*
 * M. desJardins/NMC	 5/94	Set negative value when precip is 0	*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    int npt, nhrs, ii;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;
    nhrs = *nhr;
/*
 * Loop through all the points.
 */
    for ( ii = 0; ii < npt; ii++ ) {
/*
 * Check for missing data.
 */
	if ( ( ERMISS ( prate[ii] ) ) ) {
	    prmm[ii] = RMISSD;
	} else if ( G_DIFF(prate[ii], 0.0F) ) {
	    prmm[ii] = VALUE0;
	} else {
	    prmm[ii] = prate[ii] * 60 * 60 * nhrs;
	}
    }

    return;
}
