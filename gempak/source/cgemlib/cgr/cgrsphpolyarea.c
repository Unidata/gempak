#include "geminc.h"
#include "gemprm.h"

static const double
    HalfPi = 1.5707963267948966192313,
    Degree = 57.295779513082320876798;	/* degrees per radian */

static double Hav ( double X )
/*  Haversine function: hav(x)= (1-cos(x))/2.  */
{
    return ( 1.0 - cos(X) ) / 2.0;
}


void cgr_sphpolyarea ( int *npts, float *lat, float *lon, float *radius, 
				 float *area, int *iret )
/************************************************************************
 * cgr_sphpolyarea                                                    	*
 *                                                                      *
 * This function computes the area of a spherical polygon. The output	*
 * units for the area will be the same as the input units of the radius.*
 * If the radius is <= 0, the Earth's radius (~6371200.0 meters ) will	*
 * be used for calculation.  						*
 *                                                      		*
 * Note: The ANSI C code is adapted and modified from the article	*
 *       "Computing the Area of a Spherical Polygon" 			*
 *       by Robert D. Miller, 						*
 *       in "Graphics Gems IV", Academic Press, 1994.			*
 *                                                      		*
 * cgr_sphpolyarea  ( npts, lat, lon, radius, area, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*npts	int	Number of polygon vertices 			*
 *	*lat	float	Latitude of the polygon vertices in degrees	*
 *	*lon	float	Longitude of the polygon vertices in degrees	*
 *	*radius	float	Radius of the sphere 				*
 *									*
 * Output parameters:                                                   *
 *	*area	float	Area of the spherical polygon			*
 *	*iret	int	Return value                		        *
 *			=  0  - OK					*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           05/05   initial coding				*
 * J. Wu/SAIC           07/05   keep input arrays unchanged		*
 ***********************************************************************/
{
    int  	jj, kk;
    float	*tmplat, *tmplon;
    double	Lam1, Lam2, Beta1, Beta2, CosB1, CosB2, HavA;
    double	T, A, B, C, S, sum, excess;    
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Convert from degrees to radians and save into local ararys
     */
    G_MALLOC ( tmplat, float, *npts, "CGR_SPHPOLYAREA: tmplat" );
    G_MALLOC ( tmplon, float, *npts, "CGR_SPHPOLYAREA: tmplon" );
    
    for ( jj = 0; jj < *npts; jj++ ) {
    
        tmplat[jj] = lat[jj] / Degree;
        tmplon[jj] = lon[jj] / Degree;	
    }
    

    /*
     *  Calculate the area in spherical degrees.
     */
    sum = 0;
    for ( jj = 0; jj < *npts; jj++ )
    {
	kk = jj + 1;
	if ( jj == 0 )
	{
	    Lam1= (double)tmplon[jj];	Beta1= (double)tmplat[jj];
	    Lam2= (double)tmplon[jj+1];	Beta2= (double)tmplat[jj+1];
	    CosB1= cos(Beta1);		CosB2= cos(Beta2);
	}
	else
	{
	    kk = (jj+1) % (*npts);
	    Lam1= Lam2;			Beta1= Beta2;
	    Lam2= (double)tmplon[kk];	Beta2= (double)tmplat[kk];
	    CosB1= CosB2;		CosB2= cos(Beta2);
	}

	if ( !G_DIFF(Lam1, Lam2) )
	{
	    HavA = Hav(Beta2-Beta1) + CosB1*CosB2*Hav(Lam2-Lam1);
	    A = 2 * asin ( sqrt ( HavA ) );
	    B = HalfPi - Beta2;
	    C = HalfPi - Beta1;
	    S = 0.5 * ( A + B + C );
	    T = tan(S/2) * tan((S-A)/2) * tan((S-B)/2) * tan((S-C)/2);

	    excess = fabs ( 4 * atan ( sqrt ( fabs(T) ) ) ) * Degree;
	    if ( Lam2 < Lam1 ) excess = -excess;

	    sum = sum + excess;
	}	
    }
    
    *area = fabs ( sum );    

    
    /*
     *  Calculate the area.
     */
    if ( *radius <= 0 ) *radius = RADIUS; /* The Earth's radius in meters*/
    *area *= ( (*radius) * (*radius) / Degree );

    G_FREE ( tmplat, float );
    G_FREE ( tmplon, float );
    
}

/************************************************************************/
