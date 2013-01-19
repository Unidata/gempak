#include "pd.h"

void pd_kinx ( const float *t850, const float *t700, const float *t500,
               const float *td850, const float *td700, const int *np,
               float *rkinx, int *iret )
/************************************************************************
 * pd_kinx								*
 *									*
 * This subroutine computes KINX from t850, t700, t500, td850, and	* 
 * td850. The following equation is used:				*
 *									*
 *									*
 *	RKINX = the 'K' index						*
 *	      = (t850 - t500) + td850 - (t700 - td700 )			*
 *									*
 * pd_kinx ( t850, t700, t500, td850, td700, np, rkinx, iret )		*
 *									*
 * Input parameters:							*
 *	*t850		const float	850 mb temperature in Celsius	*
 *	*t700		const float     700 mb temperature in Celsius   *
 *	*t500		const float	500 mb temperature in Celsius	*
 *      *td850		const float	850 mb dewpoint in Celsius	*
 *      *td700		const float	700 mb dewpoint in Celsius   	*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*rkinx		float		The 'K' Index			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * D. Keiser/GSC	 6/95						*
 * T. Piper/GSC		 3/99	Corrected prolog			*
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
        if ( ( ERMISS ( t850 [i] ) ) || ( ERMISS ( t700 [i] ) ) ||
      	     ( ERMISS ( t500 [i] ) ) || ( ERMISS ( td850 [i] ) ) ||
       	     ( ERMISS ( td700 [i] ) ) ) {
	    rkinx [i] = RMISSD;
        } else {
            /*
             * Find the 'K' index.
             */
	    rkinx [i] = ( t850 [i] - t500 [i] ) + td850 [i] -
     		        ( t700 [i] - td700 [i] );
        }
    }

    return;
}
