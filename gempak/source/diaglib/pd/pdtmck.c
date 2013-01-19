#include "pd.h"

void pd_tmck ( const float *tmpc, const int *np, float *tmpk, int *iret )
/************************************************************************
 * pd_tmck								*
 *									*
 * This subroutine computes TMPK from TMPC.  The following equation	*
 * is used:								*
 *									*
 *                  TMPK = TMPC + TMCK					*
 *									*
 * pd_tmck ( tmpc, npt, tmpk, iret )					*
 *									*
 * Input parameters:							*
 *	*tmpc		const float	Temperature in Celsius		*
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
	if ( ERMISS ( tmpc [i] ) ) {
	    tmpk [i] = RMISSD;
	} else {
	    tmpk [i] = tmpc [i] + TMCK;;
	}
    }

    return;
}
