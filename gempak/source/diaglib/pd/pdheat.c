#include "pd.h"

void pd_heat  ( const float *tmpf, const float *relh, const int *np,
                float *heat, int *iret )
/************************************************************************
 * pd_heat								*
 *									*
 * This subroutine computes HEAT, the Southern Region/CPC Rothfusz heat	*
 * index, from TMPF and RELH.  The output will be calculated in degrees *
 * Fahrenheit.								*
 *									*
 *	Source:  NWS Southern Region SSD Technical Attachment SR 90-23	*
 * 		 7/1/90.  Heat Index was originally known as the	*
 *		 apparent temperature index (Steadman, JAM, July, 1979).*
 *									*
 * The Rothfusz regression is optimal for TMPF > ~80 and RELH > ~40%.	*
 * This code applies a simple heat index formula and then resorts to	*
 * the Rothfusz regression only if the simple heat index exceeds 80,	*
 * implying temperatures near, but slightly below 80.  To make the	*
 * simple calculation continuous with the values obtained from the	*
 * Rothfusz regression, the simple result is averaged with TMPF in	*
 * computing the simple heat index value.				*
 *									*
 * This code includes adjustments made by the CPC for low RELH at high	*
 * TMPF and high RELH for TMPF in the mid 80's.				*
 *									*
 * pd_heat ( tmpf, relh, np, heat, iret )				*
 *									*
 * Input parameters:							*
 *	*tmpf		const float	Temperature in Fahrenheit	*
 *	*relh		const float	Relative humidity in percent	*
 *	*np		const int	Number of points		*
 *									*
 * Output parameters:							*
 *	*heat		float		Heat Index in Fahrenheit	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/02	Initial coding				*
 * K. Brill/HPC		11/02	Remove SUBFLG input logical array	*
 * K. Brill/HPC		 1/03	Fix discontinuity around 77 F		*
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float t2, r2, adj1, tval, adj2, adj;
    int npt, i;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
        /*
         * If either the Temperature or Relative Humidity are missing,
         * then set the result to missing.
         */
        if ( ERMISS (tmpf[i]) || ERMISS (relh[i]) ) {
	    heat[i] = RMISSD;
        } else {
            /*
    	     * If the temperature is less than 40 degrees, then set the
	     * heat index to the temperature.
             */
	    if ( tmpf[i] <= 40.0F ) {
	        heat[i] = tmpf[i];
            } else {
                /*
	         * Compute a simple heat index. If the value is less
	         * than 80 degrees, use it.
                 */
		heat[i] = 61.0F + (tmpf[i]-68.0F) * 1.2F + relh[i] * .094F;
		heat[i] = .5F * ( tmpf[i] + heat[i] );
		if ( heat[i] >= 80.0F ) {
                    /*
	    	     * Otherwise, compute the full regression value
	    	     * of the heat index.
                     */
		    t2 = tmpf[i] * tmpf[i];
		    r2 = relh[i] * relh[i];
		    heat[i] = -42.379F
     			      +  2.04901523F * tmpf[i]
     			      + 10.14333127F * relh[i] 
     			      -  0.22475541F * tmpf[i] * relh[i]
     			      -  0.00683783F * t2 
     			      -  0.05481717F * r2
     			      +  0.00122874F * t2 * relh[i]
     			      +  0.00085282F * tmpf[i] *r2
     			      -  0.00000199F * t2 * r2;
                    /*
	    	     * Adjust for high regression at low RH for temps
	    	     * above 80 degrees F.
                     */
		    if ( ( relh[i] <= 13.0F ) &&
     			 ( ( tmpf[i] >=  80.0F ) &&
     			   ( tmpf[i] <= 112.0F ) ) ) {
		        adj1 = ( 13.0F - relh[i] ) / 4.0F;
			tval = 17.0F - fabs ( tmpf[i] - 95.0F );
			adj2 = sqrt ( tval / 17.0F );
			adj  = adj1 * adj2;
			heat[i] -= adj;

                    /*
		     * Adjust for low regression at high RH and temps
		     * in the mid 80s.
		     */
		    } else if ( ( relh[i] > 85.0F ) && 
     			      ( ( tmpf[i] >= 80.0F ) &&
     				( tmpf[i] <= 87.0F ) ) ) {
		        adj1 = ( ( relh[i] - 85.0F ) / 10.0F );
			adj2 = ( ( 87.0F - tmpf[i] ) / 5.0F );
			adj  = adj1 * adj2;
			heat[i] += adj;
                    }
		}
	    }
	}
    }

    return;
}
