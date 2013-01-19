#include "pd.h"

void pd_mskn ( const float *sped, const int *np, float *sknt, int *iret )
/************************************************************************
 * pd_mskn								*
 *									*
 * This subroutine computes SKNT from SPED.  The following equation	*
 * is used:					 			*
 *									*
 *                 SKNT  =  SPED * 1.9425				*
 *									*
 * pd_mskn ( sped, np, sknt, iret )					*
 *									*
 * Input parameters:							*
 *	*sped		const float	Speed in meters/second		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*sknt		float		Speed in knots			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * L. Sager/NCEP	 4/96	Updated knots to m/sec conversion 	*
 *				factor to NCEP standard			*
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
        if ( ERMISS ( sped [i] ) ) {
	    sknt [i] = RMISSD;
        } else {
	    sknt [i] = 1.9425F * sped [i];
        }
    }

    return;
}
