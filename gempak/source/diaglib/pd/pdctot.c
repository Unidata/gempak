#include "pd.h"

void pd_ctot  ( const float *t500, const float *td850, const int *np,
                float *ctot, int *iret )
/************************************************************************
 * pd_ctot								*
 *									*
 * This subroutine computes CTOT from t500 and td850.  The following	*
 * equation is used:							*
 *									*
 *	CTOT = cross totals index					*
 *	     = td850 - t500						*
 *									*
 * pd_ctot ( t500, TD850, np, ctot, iret ) 				*
 *									*
 * Input parameters:							*
 *	*t500		const float	500 mb temperature in Celsius	*
 *	*td850		const float	850 mb dewpoint in Celsius	*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*ctot		float		Cross Totals Index		*
 *	*int		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * D. Keiser/GSC	 6/95						*
 * T. Piper/GSC		 3/99	Corrected typo in prolog		*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    int i, npt;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;

    /*
     *	Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
        /*
         * Check for missing data.
         */
        if ( ( ERMISS ( t500 [i] ) ) || ( ERMISS ( td850 [i] ) ) ) {
	    ctot [i] = RMISSD;
        } else  {
            /*
             * Find the cross totals index.
             */
	    ctot [i] = td850 [i] - t500 [i];
        }
    }

    return;
}
