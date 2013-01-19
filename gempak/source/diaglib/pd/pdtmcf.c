#include "pd.h"

static const float RPRM = 9.0F / 5.0F;

void pd_tmcf ( const float *tmpc, const int *np, float *tmpf, int *iret )
/************************************************************************
 * pd_tmcf								*
 *									*
 * This subroutine computes TMPF from TMPC.  The following equation	*
 * is used:								*
 *									*
 *            TMPF = ( TMPC * 9 / 5 ) + 32				*
 *									*
 * pd_tmcf ( tmpc, np, tmpf, iret )					*
 *									*
 * Input parameters:							*
 *	*tmpc		const float	Temperature in Celsius		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*tmpf		float		Temperature in Fahrenheit	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
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
	if ( ERMISS ( tmpc [i] ) ) {
	    tmpf [i] = RMISSD;
	} else {
	    tmpf [i] = ( tmpc [i] * RPRM ) + 32.0F;
	}
    }

    return;
}
