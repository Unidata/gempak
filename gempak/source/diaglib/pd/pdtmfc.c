#include "pd.h"

static const float RPRM = 5.0F / 9.0F;

void pd_tmfc ( const float *tmpf, const int *np, float *tmpc, int *iret )
/************************************************************************
 * pd_tmfc								*
 *									*
 * This subroutine computes TMPC from TMPF.  The following equation is	*
 * used:			 					*
 *									*
 *                TMPC = ( TMPF - 32 ) * 5 / 9				*
 *									*
 * pd_tmfc ( tmpf, np, tmpc, iret )					*
 *									*
 * Input parameters:							*
 *	*tmpf		const float	Temperature in Fahrenheit	*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*tmpc		float		Temperature in Celsius		*
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
	if ( ERMISS ( tmpf [i] ) ) {
	    tmpc [i] = RMISSD;
	} else {
	    tmpc [i] = ( tmpf [i] - 32.0F ) * RPRM;
	}
    }

    return;
}
