#include "pd.h"

void pd_hans ( const float *tc1, const float *tc2, const float *dwpc,
               const int *np, const int *type, float *haines, int *iret )
/************************************************************************
 * pd_hans								*
 *									*
 * This subroutine computes low, middle, and high elevation Haines 	*
 * Indices from TMPC and DWPC.						*
 *									*
 * pd_hans ( tc1, tc2, dwpc, np, type, haines, iret )			*
 *									*
 * Input parameters:							*
 *	*tc1		const float	Temperature in Celsius		*
 *	*tc2		const float	Temperature in Celsius		*
 *	*dwpc		const float	Dewpoint in Celsius		*
 *	*np		const int	Number of points		*
 *	*type		const int	Type of Haines index		*
 *					  1 = Low			*
 *					  2 = Middle			*
 *					  3 = High			*
 *									*
 * Output parameters:							*
 *	*haines		float		Haines index			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 6/03	Created					*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float a, b;
    int i, npt, itype;    
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;
    itype = *type;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
        /*
         * Check for missing data.
         */
        if ( ( ERMISS ( tc1 [i] ) ) || ( ERMISS ( tc2 [i] ) ) ||
     	     ( ERMISS ( dwpc [i] ) ) ) {
	    haines [i] = RMISSD;
        } else {
            /*
             * Compute the Haines index.
             */
	    if ( itype == 1 ) {
	        a = ( ( tc2 [i] -  tc1 [i] ) - 3.0F ) * (2.0F/5.0F) + 1.0F;
		b = ( ( tc1 [i] - dwpc [i] ) - 5.0F ) * (2.0F/5.0F) + 1.0F;
            } else if ( itype == 2 ) {
	        a = ( ( tc1 [i] -  tc2 [i] ) - 5.0F ) * (2.0F/6.0F) + 1.0F;
		b = ( ( tc1 [i] - dwpc [i] ) - 5.0F ) * (2.0F/8.0F) + 1.0F;
            } else if ( itype == 3 ) {
	        a = ( ( tc1 [i] -  tc2 [i] ) - 17.0F) * (2.0F/5.0F) + 1.0F;
		b = ( ( tc1 [i] - dwpc [i] ) - 14.0F) * (2.0F/7.0F) + 1.0F;
            }
	    a = G_MAX ( a, 0.9F );
	    a = G_MIN ( a, 3.1F );
	    b = G_MAX ( b, 0.9F );
	    b = G_MIN ( b, 3.1F );
	    haines [i] = a + b;
        }
    }
    
    return;
}
