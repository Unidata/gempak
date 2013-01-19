#include "pd.h"

static const float A = 0.03229F;
static const float B = 0.281073F;
static const float C = 0.000578F;
static const float D = 2.22749F;
static const float E = 0.160107F;
static const float F = 0.014784F;
static const float G = 21.0606F;
static const float H = 0.005565F;
static const float P = 0.00035F;
static const float Q = 0.483199F;
static const float R = 0.3002F;
static const float T = 9.0F / 5.0F;
static const float U = 1.9425F;
static const float V = 0.868976F;

void pd_fosb ( const float *tmpc, const float *relh, const float *sped,
               const int *np, float *fosb, int *iret )
/************************************************************************
 * pd_fosb                                                              *
 *                                                                      *
 * This function computes Fosberg index from temperature, relative      *
 * humidity and wind speed.                                             *
 *                                                                      *
 *                                                                      *
 * pd_fosb  ( tmpc, relh, sped, np, fosb, iret )                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *tmpc		const float	Temperature (C)                 *
 *      *relh		const float	Relative humidity in percent    *
 *      *sped		const float	Wind speed in meters/second     *
 *      *np             const int	Number of points                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *fosb		float		Fosberg index                   *
 *      *iret           int		Return status                   *
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/GSC            6/03   Created                                 *
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/

{
    float tf, sknt, smph, rh, fw, sss, fwd, fwd2, fwd3, fire;
    int i, npt;
/*-----------------------------------------------------------------------*/
    *iret = 0;
    npt = *np;

    /*
     * Loop through all the points.
     */
    for ( i = 0; i < npt; i++ ) {
        /*
	 * Check for missing values.
	 */
        if ( ERMISS ( tmpc[i] ) || ERMISS ( relh[i] ) ||
	     ERMISS ( sped[i] ) || ( relh[i] < 0.0F ) ) {
	    fosb[i] = RMISSD;
        } else {
	    /*
	     * Change temperature to degrees Fahrenheit, wind speed
	     * from meters/second to miles/hr.
	     */
	    tf = tmpc[i] * T + 32.0F;
	    sknt = sped[i] * U;
	    smph = sknt / V;
	    rh = relh[i];

	    if ( rh <= 10.0F ) {
	        fw = A + B * rh - C * rh * tf;
	    } else if ( rh <= 50.0F ) {
	        fw = D + E * rh - F * tf;
	    } else {
	        fw = G + H * rh * rh - P * rh * tf - Q * rh;
	    }

	    sss = sqrt ( 1.0F + smph * smph );
	    fwd = fw / 30.0F;
	    fwd2 = fwd * fwd;
	    fwd3 = fwd2 * fwd;
	    fire = 1.0F - 2.0F * fwd + 1.5F * fwd2 - 0.5F * fwd3;

	    fosb[i] = fire * sss / R;
	}
    }

    return;
}
