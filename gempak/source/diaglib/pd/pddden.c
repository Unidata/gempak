#include "pd.h"

void pd_dden ( const float *pres, const float *tmpc, const int *np,
               float *dden, int *iret )
/************************************************************************
 * pd_dden								*
 *									*
 * This subroutine computes DDEN from PRES, TMPC.  The following	*
 * equation is used:							*
 *									*
 *           DDEN = 100 * PRES / ( RDGAS * TMPK )			*
 *									*
 *                100 = conversion from millibars to pascals		*
 *									*
 * pd_dden  ( pres, tmpc, np, dden, iret )				*
 *									*
 * Input parameters:							*
 *	*pres		const float 	Pressure in millibars		*
 *	*tmpc		const float	Temperature in Celsius		*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*dden		float	 	Dry air density in kg/(m**3)	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * G. Krueger/EAI	 4/96	Replaced C->K constant with TMCK	*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float tmpk;
    int i, npt;
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
        if ( ( ERMISS ( pres [i] ) ) || ( ERMISS ( tmpc [i] ) ) ||
     	     ( tmpc [i] < -TMCK ) ) {
	    dden [i] = RMISSD;
        } else {
            /*
             * Convert temperature and compute.
             */
            tmpk = tmpc [i] + TMCK;
            dden [i] = 100.0F * pres [i] / ( RDGAS * tmpk );
        }
    }

    return;
}
