#include "pd.h"

static const float RPRM = 5.0F / 9.0F;

void pd_tmfk ( const float *tmpf, const int *np, float *tmpk, int *iret )
/************************************************************************
 * pd_tmfk								*
 *									*
 * This subroutine computes TMPK from TMPF.  The following equation	*
 * is used:								*
 *									*
 *           TMPK = ( TMPF - 32 ) * 5 / 9 + TMCK			*
 *									*
 * pd_tmfk ( tmpf, np, tmpk, iret )					*
 *									*
 * Input parameters:							*
 *	*tmpf		const float	Temperature in Fahrenheit	*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*tmpk		float		Temperature in Kelvin		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * G. Krueger/EAI        4/96   Replaced C->K constant with TMCK	*
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
	    tmpk [i] = RMISSD;
	} else {
	    tmpk [i] = ( tmpf [i] - 32.0F ) * RPRM + TMCK;
	}
    }

    return;
}
