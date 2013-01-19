#include "pd.h"

void pd_knms ( const float *sknt, const int *np, float *sped, int *iret )
/************************************************************************
 * pd_knms								*
 *									*
 * This subroutine computes SPED from SKNT.  The following equation is	*
 * used:								*
 *									*
 *               SPED = SKNT / 1.9425					*
 *									*
 * pd_knms ( sknt, np, sped, iret )					*
 *									*
 * Input parameters:							*
 *	*sknt		const float	Speed in knots			*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*sped		float		Speed in meters/second		*
 *	iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 *Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * L. Sager/NCEP	 4/96	Updated value of knots to m/sec 	*
 *				conversion				*
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
        if ( ERMISS ( sknt [i] ) ) {
	    sped [i] = RMISSD;
        } else {
	    sped [i] = sknt [i] / 1.9425F;
        }
    }

    return;
}
