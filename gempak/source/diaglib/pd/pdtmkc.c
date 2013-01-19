#include "pd.h"

void pd_tmkc ( const float *tmpk, const int *np, float *tmpc, int *iret )
/************************************************************************
 * pd_tmkc								*
 *									*
 * This subroutine computes TMPC from TMPK.  The following equation	*
 * is used:								*
 *									*
 *                TMPC = TMPK - TMCK					*
 *									*
 * pd_tmkc ( tmpk, np, tmpc, iret )					*
 *									*
 * Input parameters:							*
 *	*tmpk		const float	Temperature in Kelvin		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*tmpc		float		Temperature in Celsius		*
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
    	if ( ERMISS ( tmpk [i] ) ) {
	    tmpc [i] = RMISSD;
	} else {
	    tmpc [i] = tmpk [i] - TMCK;
	}
    }

    return;
}
