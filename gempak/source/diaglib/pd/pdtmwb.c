#include "pd.h"

static const float A = 6.112F;
static const float B = 17.67F;
static const float C = 243.5F;
static const float EPSI = .622F;
static const float G = 4302.645F;	/* ( G = B * C ) */
static const float ERRMAX = .001F;

void pd_tmwb ( const float *pres, const float *tmpk, const float *rmix,
               const int *np, float *wetbt, int *iret )
/************************************************************************
 * pd_tmwb                                                              *
 *                                                                      *
 * This function computes wet bulb temperature from the pressure        *
 * temperature, and mixing ratio.  The result is obtained by solving    *
 * for the temperature at which saturation occurs when the latent       *
 * heat required to vaporize the water is provided by a cooling of      *
 * the air.  The equation representing the process is                   *
 *                                                                      *
 *      ( TMPK - TMWB ) * CP - ( Rsat (TMWB) - RMIX ) * LVAP = 0        *
 *                                                                      *
 * This implicit equation is solved by Newton's method, since the       *
 * saturation mixing ratio Rsat is a transcendental function of         *
 * TMWB.                                                                *
 *                                                                      *
 * The expressions for the heat of vaporization (LVAP) and saturation   *
 * vapor pressure are equations (2) and (10) from Bolton (MWR, 1980).   *
 *                                                                      *
 * pd_tmwb ( pres, tmpk, rmix, np, wetbt, iret )                      	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *pres		const float	Pressure (mb)                   *
 *      *tmpk		const float	Temperature (K)                 *
 *      *rmix		const float	Mixing ratio (g/g)              *
 *      *np             const int	Number of points                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *wetbt		float		Wet bulb temperature (K)        *
 *      *iret           int		Return status                   *
 **                                                                     *
 * Log:                                                                 *
 * L. WilliamS/EAI       4/94   Modified from PR_TMWB                   *
 * G. Krueger/EAI        4/96   Replaced C->K constant with TMCK        *
 * L. Sager/NCEP         4/96   Updated constant in lvap equation       *
 * T. Lee/GSC           11/96   Switched parameter order in PD_TMWB     *
 * S. Jacobs/NCEP        8/97   Changed latent heat calc to be more     *
 *                              like calc in PR_LHVP                    *
 * T. Lee/GSC            8/97   Set TMWB <= temp; Simplify computation  *
 * T. Piper/GSC          3/99   Corrected prolog			*
 * K. Brill/HPC         11/02   Remove SUBFLG input logical array       *
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float tmp, cp, rlocp, twb, bt, tpc, d, df, cor, dm1, f;
    int npt, lvap, i, iter, convrg;
/*----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
        /*
	 * Check for missing values.
	 */
        if ( ERMISS ( pres[i] ) || ERMISS ( tmpk[i] ) || ERMISS ( rmix[i] ) ||
	     ( pres[i] <= 0.0F ) || ( rmix[i] < 0.0F ) ) {
	    wetbt[i] = RMISSD;
        } else {
	    /*
	     * Change temperature to degrees Celsius.
	     */
	    tmp = tmpk[i] - TMCK;

	    /*
	     * Compute the latent heat of vaporization.
	     */
	    lvap = ( int ) ( ( 2.500F - .00237F * tmp ) * 1.0E6 );

	    /*
	     * Compute the specific heat of moist air.
	     */
	    cp = 1005.7F * ( 1.0F + .887F * rmix[i] );

	    /*
	     * Compute L / cp.
	     */
	    rlocp = lvap / cp;

	    /*
	     * Do Newton iteration.
	     */
	    iter = 0;
            twb = tmp;
            convrg = G_FALSE; 
            while ( iter <= 50 && ! convrg ) {
                iter++;
                bt = B * twb;
                tpc = twb + C;
                d = ( pres[i] / A ) * exp ( -bt / tpc );
                dm1 = d - 1.0F;
                f = ( tmp - twb ) - rlocp * ( EPSI / dm1 - rmix[i] );
                df = - G / ( tpc * tpc );
                df = d * df * rlocp * EPSI / ( dm1 * dm1 ) - 1.0F;
                cor = f / df;
                twb -= cor;
                if ( fabs ( cor ) <= ERRMAX ) convrg = G_TRUE;
            }

            if ( ! convrg )  {
              wetbt[i] = RMISSD;
            } else {
                wetbt[i] = twb + TMCK;
                if ( wetbt[i] > tmpk[i] ) wetbt[i] = tmpk[i];
            }
	}
    }
}
