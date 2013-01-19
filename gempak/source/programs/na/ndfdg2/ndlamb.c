#include "geminc.h"
#include "gemprm.h"

void nd_lamb ( int *gds, float *gdsarr, int *irltv, int *nrmflg );

void nd_lamb ( int *gds, float *gdsarr, int *irltv, int *nrmflg )
/************************************************************************
 * nd_lamb								*
 *                                                                      *
 * This routine will get the GDS information for a Lambert grid.  All   *
 * of the information is put into the data structure for the GDS.       *
 *                                                                      *
 * nd_lamb ( gds, gdsarr, irltv, nrmflg )				*
 *									*
 * Input parameters:                                                    *
 *      *gds		int	Unpacked Grid Definition Template       *
 * Output parameters:                                                   *
 *      *gdsarr         float   GDS information array (GRIB projection 	*
 *				number, number of columns of data,   	*
 *                              number of rows of data, lat/lon of 	*
 *				corners)          			*
 *      *irltv          int     Resolution flag                 	*
 *      *nrmflg         int     Flag for "normal" grid (i.e., the grid 	*
 *				is NOT rotated or stretched and the 	*
 *				scanning mode is OK)     		*
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	10/02	Modified from gb_lamb & gb_ggds		*
 * T. Piper/SAIC	04/03	Eliminated compiler warnings		*
 ***********************************************************************/
{

int		Nx, Ny, flag1, flag2, mode, La1, idrct, jdrct, consec;
double		earth_rad, const_cone, Xll, Yll, Xur, Yur,
		lat1, lon1, loncnt, angle1, angle2, LoV, Latin1, Latin2,
		X1, X2, Y1, Y2, alpha, rtemp, Dx, Dy, Lo1;

/*---------------------------------------------------------------------*/
	/*
	 * BYTES 31-34 
	 * Nx - number of points along X-axis
	 */
	Nx = gds[30]; 

	/*
	 * BYTES 35-38 
	 * Ny - number of points along Y-axis
	 */
	Ny = gds[34]; 

	/*
	 * BYTES 39-42 
	 * La1 - latitude of first grid point
	 */
	La1 = gds[38]; 
	lat1 = ( (double)La1 / 1000000.0 ) * DTR;
	/*
	 * BYTES 43-46
	 * Lo1 - longitude of first grid point
	 */
	Lo1 = (double)gds[42] / 1000000.0; 
	if ( Lo1 >  180.0 ) Lo1 = Lo1 - 360.0;
	if ( Lo1 < -180.0 ) Lo1 = Lo1 + 360.0;
	lon1 =  Lo1 * DTR;
	/*
	 * BYTE 47
	 * Resolution and component flags
	 */
	flag1 = gds[46]; 

	/*
	 * BYTES 52-55 
	 * Lov - orientation of the grid (center longitude)
	 */
	LoV = (double)gds[51] / 1000000.0; 
	if ( LoV > 180.0 ) LoV = LoV - 360.0;
	if ( LoV < -180.0 ) LoV = LoV + 360.0;
	loncnt =  LoV * DTR;
	/*
	 * BYTES 56-59
	 * Dx - X-direction grid length
	 */
	Dx = (double)gds[55] / 1000.0; 
	/*
	 * BYTES 60-63 
	 * Dy - Y-direction grid length
	 */
	Dy = (double)gds[59] / 1000.0;
	/*
	 * Projection center flag [bit1=0=NP, bit1=1=SP;
	 * 			bit2=0-one projection center,
	 * 			bit2=1-bipolar and symmetric]
	 * BYTE 64 
	 */
	flag2 = gds[63];

	/*
	 * BYTE 65 
	 * Scanning mode
	 */
	mode = gds[64];

	/*
	 * BYTES 66-69 
	 * Latin1 - first latitude from the pole at which the secant
	 * 	 cone cuts the sphere.
	 */
	Latin1 = (double)gds[65] / 1000000.0;

	/*
	 * BYTES 70-73
	 * Latin2 - second latitude from the pole at which the secant
	 * 	 cone cuts the sphere.
	 */
	Latin2 = (double)gds[69] / 1000000.0;

	/*
	 * BYTES 74-77 
	 * Latitude of the southern pole in millidegrees
	 */

	/*
	 * BYTES 78-81 
	 * Longitude of the southern pole in millidegrees
	 */

	/*
	 * Compute the constant of the cone from the colatitudes of the
	 * true latitudes.
	 */
	angle1 = HALFPI - ( fabs ( Latin1 ) * DTR );
	angle2 = HALFPI - ( fabs ( Latin2 ) * DTR );
	if ( Latin1 == Latin2 ) {
	const_cone = cos ( angle1 );
	}
	else
	    const_cone = ( log ( sin ( angle2 ) ) - log ( sin ( angle1 ) ) ) /
	    	( log ( tan ( angle2/2. ) ) - log ( tan ( angle1/2. ) ) );

	/*
	 * Compute the linear coordinates for the first grid point.
	 */
	earth_rad = (double)RADIUS / const_cone;

	if ( ( flag2 >> 7 ) == 0 ) {
	/*
	 *     Northern Hemisphere
	 */
	    X1 =  earth_rad * pow (tan((HALFPI-lat1)/2.), const_cone) *
		      sin ( const_cone * ( lon1 - loncnt ) );
	    Y1 = -earth_rad * pow (tan((HALFPI-lat1)/2.), const_cone) *
		      cos ( const_cone * ( lon1 - loncnt ) );
	}
	else{
	/*
	 *     Southern Hemisphere
	 */
	X1 =  earth_rad * pow (tan((HALFPI+lat1)/2.), const_cone) *
		      sin ( const_cone * ( lon1 - loncnt ) );
	    Y1 =  earth_rad * pow (tan((HALFPI+lat1)/2.), const_cone) *
		      cos ( const_cone * ( lon1 - loncnt ) );
	}

	/*
	 * ALPHA is the map scale factor at Latin1.
	 */
	alpha = pow ( tan ( angle1 / 2. ), const_cone ) / sin ( angle1 );

	/*
	 * Compute the linear coordinates for the second grid point.
	 * Check the scanning mode to determine +/- grid spacing.
	 */
	if ( ( mode >> 7 ) == 1 )
	    X2 = X1 - (double)( Nx - 1 ) * alpha * Dx;
	else
	    X2 = X1 + (double)( Nx - 1 ) * alpha * Dx;

	if ( ( ( mode >> 6 ) & 1 ) == 1 )
	    Y2 = Y1 + (double)( Ny - 1 ) * alpha * Dy;
	else
	    Y2 = Y1 - (double)( Ny - 1 ) * alpha * Dy;
	/*
	 * Get the point coordinates paired as Lower Left and Upper Right.
	 */
	Xll = G_MIN( X1, X2 );
	Yll = G_MIN( Y1, Y2 );
	Xur = G_MAX( X1, X2 );
	Yur = G_MAX( Y1, Y2 );

	/*
	 * Compute the lat/lon coordinates of the first point.
	 */
	if ( ( flag2 >> 7 ) == 0 ) {
	/*
	 *     Northern Hemisphere
	 */
	gdsarr[3] = (float)(( HALFPI - 2. *
	      atan ( pow ( sqrt (pow(Xll,2.0)+pow(Yll,2.0)) / earth_rad,
			 ( 1. / const_cone ) ) ) ) * RTD);
	    rtemp = atan2 ( Xll, -Yll ) * ( 1. / const_cone ) + loncnt;
	}
	else {
	/*
	 *     Southern Hemisphere
	 */
	    gdsarr[3] = (float)(( -1. *  ( HALFPI - 2. *
	      atan ( pow ( sqrt (pow(Xll,2.0)+pow(Yll,2.0)) / earth_rad,
			 ( 1. / const_cone ) ) ) ) ) * RTD);
	    rtemp = atan2 ( Xll,  Yll ) * ( 1. / const_cone ) + loncnt;
	}

	if ( rtemp > PI )
	    gdsarr[4] = (float)(( rtemp - TWOPI ) * RTD);
	else if ( rtemp < -PI )
	    gdsarr[4] = (float)(( rtemp + TWOPI ) * RTD);
	else
	    gdsarr[4] = (float)(rtemp * RTD);

	/*
	 * Compute the lat/lon coordinates of the second point.
	 */
	if ( ( flag2 >> 7 ) == 0 ) {
	/*
	 *     Northern Hemisphere
	 */
	    gdsarr[5] = (float)(( HALFPI - 2. *
	      atan ( pow ( sqrt (pow(Xur,2.0)+pow(Yur,2.0)) / earth_rad,
			 ( 1. / const_cone ) ) ) ) * RTD);
	    rtemp = atan2 ( Xur, -Yur ) * ( 1. / const_cone ) + loncnt;
	}
	else {
	/*
	 *     Southern Hemisphere
	 */
	    gdsarr[5] = (float)(( -1. *  ( HALFPI - 2. *
	      atan ( pow ( sqrt (pow(Xur,2.0)+pow(Yur,2.0)) / earth_rad,
			 ( 1. / const_cone ) ) ) ) ) * RTD);
	    rtemp = atan2 ( Xur,  Yur ) * ( 1. / const_cone ) + loncnt;
	}

        if ( rtemp > PI )
	    gdsarr[6] = (float)(( rtemp - TWOPI ) * RTD);
        else if ( rtemp < -PI )
	    gdsarr[6] = (float)(( rtemp + TWOPI ) * RTD);
        else
	    gdsarr[6] = (float)(rtemp * RTD);

	/*
	 * Set the projection angle information, the number of x,y points,
	 * and the coded flags.
	 */
	gdsarr[7] = (float) Latin1;
	gdsarr[8] = (float) LoV;
	gdsarr[9] = (float) Latin2;

	gdsarr[0] = (float)gds[12];
	gdsarr[1] = (float) Nx;
	gdsarr[2] = (float) Ny;

	*irltv = ( flag1 >> 3 ) & 1;
	idrct = ( mode >> 7 ) & 1;
	jdrct = ( mode >> 6 ) & 1;
	consec = ( mode >> 5 ) & 1;
	if ( idrct == 0 && jdrct == 1 && consec == 0 ) {
	    *nrmflg = TRUE;
	}
	else {
	    *nrmflg = FALSE;
	}

}
