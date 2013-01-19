#include "gb2def.h"

void gb2_lamb ( gribfield *gfld, float *gdsarr, int *scan_mode, int *iret )
/************************************************************************
 * gb2_lamb								*
 *                                                                      *
 * This routine will get the GDS information for a Lambert grid. All    *
 * of the information is put into the data array for the GDS.           *
 *                                                                      *
 * gb2_lamb ( gfld, gdsarr, scan_mode, iret )				*
 *									*
 * Input parameters:                                                    *
 *      *gfld   struct gribfield        Decoded GRIB2 structure         *
 *									*
 * Output parameters:                                                   *
 *  *gdsarr             float       GDS information array               *
 *                             (GRIB projection number,                 *
 *                             number of columns of data,               *
 *                             number of rows of data,                  *
 *                             lat/lon of corners)                      *
 *  *scan_mode          int             GRIB2 scanning mode flag        *
 *  *iret               int     Return code                             *
 *                          -19 = error on message                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI          02/93                                           *
 * S. Jacobs/EAI        10/93           Copied from GBUTIL              *
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * S. Jacobs/NMC	10/94		Changed 'abs' to 'fabs'		*
 * S. Jacobs/NMC	10/94		Changed computation of alpha	*
 * S. Jacobs/NMC	12/94		Changed equality check to use	*
 *					integral angles			*
 * D.W.Plummer/NCEP	 2/96		Cleanup GBDIAGs and comments	*
 * K. Brill/EMC		 5/98		LoV, Lo1 in -180 -> 180 range	*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 * S. Gilbert           11/04       Modified from gb_ltln for use w/ GRIB2  *
 ***********************************************************************/
{

        int	Latin1, Latin2, Nx, Ny, flag2, mode,
		La1, Lo1, LoV, Dx, Dy;
        int     Lo1e, LoVe;
/*	int	dummy, flag1, LaD; */

        double	earth_rad, const_cone, Xll, Yll, Xur, Yur,
		lat1, lon1, loncnt, angle1, angle2,
		X1, X2, Y1, Y2, alpha, rtemp;

/*---------------------------------------------------------------------*/
	*iret = 0;

        gdsarr[0] = (float) 3.0;         /* Specifies Lambert Conformal */

	/*
	 * Nx - number of points along X-axis
	 */
	Nx = (int)gfld->igdtmpl[7];

	/*
	 * Ny - number of points along Y-axis
	 */
	Ny = (int)gfld->igdtmpl[8];

	/*
	 * La1 - latitude of first grid point
	 */
	La1 = (int)gfld->igdtmpl[9];
	lat1 = ( La1 / 1000000.0 ) * DTR;

	/*
	 * Lo1 - longitude of first grid point
	 */
	Lo1e = (int)gfld->igdtmpl[10];
        Lo1 = Lo1e;
	if ( Lo1 > 180000000 ) Lo1 = Lo1 - 360000000;
	lon1 = ( Lo1 / 1000000.0 ) * DTR;

	/*
	 * Resolution and component flags
	 */
/*	flag1 =  (int)gfld->igdtmpl[11];  NOT used */

	/*
	 * LaD - Latitude where Dx and Dy are specified
	 */
/*	LaD = (int)gfld->igdtmpl[12];  NOT used */

	/*
	 * Lov - orientation of the grid (center longitude)
	 */
	LoVe = (int)gfld->igdtmpl[13];
        LoV=LoVe;
	if ( LoV > 180000000 ) LoV = LoV - 360000000;
	loncnt = ( LoV / 1000000.0 ) * DTR;

	/*
	 * Dx - X-direction grid length
	 */
	Dx = (int)gfld->igdtmpl[14];

	/*
	 * Dy - Y-direction grid length
	 */
	Dy = (int)gfld->igdtmpl[15];

	/*
	 * Projection center flag [bit1=0=NP, bit1=1=SP;
	 * 			bit2=0-one projection center,
	 * 			bit2=1-bipolar and symmetric]
	 */
	flag2 = (int)gfld->igdtmpl[16];

	/*
	 * Scanning mode
	 */
	mode = (int)gfld->igdtmpl[17];
        *scan_mode=mode;

	/*
	 * Latin1 - first latitude from the pole at which the secant
	 * 	 cone cuts the sphere.
	 */
	Latin1 = (int)gfld->igdtmpl[18];

	/*
	 * Latin2 - second latitude from the pole at which the secant
	 * 	 cone cuts the sphere.
	 */
	Latin2 = (int)gfld->igdtmpl[19];

	/*
	 * Latitude of the southern pole in millidegrees
	 */
/*	dummy = (int)gfld->igdtmpl[20];  NOT used */

	/*
	 * Longitude of the southern pole in millidegrees
	 */
/*	dummy = (int)gfld->igdtmpl[21];  NOT used */

	/*
	 * Compute the constant of the cone from the colatitudes of the
	 * true latitudes.
	 */
	angle1 = HALFPI - ( fabs ( Latin1 / 1000000.0 ) * DTR );
	angle2 = HALFPI - ( fabs ( Latin2 / 1000000.0 ) * DTR );

	if ( Latin1 == Latin2 )
	    const_cone = cos ( angle1 );
	else
	    const_cone = ( log ( sin ( angle2 ) ) - log ( sin ( angle1 ) ) ) /
	    	( log ( tan ( angle2/2 ) ) - log ( tan ( angle1/2 ) ) );

	/*
	 * Compute the linear coordinates for the first grid point.
	 */
	earth_rad = RADIUS / const_cone;

	if ( ( flag2 >> 7 ) == 0 ) {
	/*
	 *     Northern Hemisphere
	 */
	    X1 =  earth_rad * pow (tan((HALFPI-lat1)/2), const_cone) *
		      sin ( const_cone * ( lon1 - loncnt ) );
	    Y1 = -earth_rad * pow (tan((HALFPI-lat1)/2), const_cone) *
		      cos ( const_cone * ( lon1 - loncnt ) );
	}
	else{
	/*
	 *     Southern Hemisphere
	 */
	    X1 =  earth_rad * pow (tan((HALFPI+lat1)/2), const_cone) *
		      sin ( const_cone * ( lon1 - loncnt ) );
	    Y1 =  earth_rad * pow (tan((HALFPI+lat1)/2), const_cone) *
		      cos ( const_cone * ( lon1 - loncnt ) );
	}

	/*
	 * ALPHA is the map scale factor at Latin1.
	 */
	alpha = pow ( tan ( angle1 / 2 ), const_cone ) / sin ( angle1 );

	/*
	 * Compute the linear coordinates for the second grid point.
	 * Check the scanning mode to determine +/- grid spacing.
	 */
	if ( ( mode >> 7 ) == 1 )
	    X2 = X1 - ( Nx - 1 ) * alpha * Dx / 1000.0;
	else
	    X2 = X1 + ( Nx - 1 ) * alpha * Dx / 1000.0;

	if ( ( ( mode >> 6 ) & 1 ) == 1 )
	    Y2 = Y1 + ( Ny - 1 ) * alpha * Dy / 1000.0;
	else
	    Y2 = Y1 - ( Ny - 1 ) * alpha * Dy / 1000.0;

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
	    gdsarr[3] = ( HALFPI - 2 *
	      atan ( pow ( sqrt (pow(Xll,2.0)+pow(Yll,2.0)) / earth_rad,
			 ( 1 / const_cone ) ) ) ) * RTD;
	    rtemp = atan2 ( Xll, -Yll ) * ( 1 / const_cone ) + loncnt;
	}
	else {
	/*
	 *     Southern Hemisphere
	 */
	    gdsarr[3] = ( -1 *  ( HALFPI - 2 *
	      atan ( pow ( sqrt (pow(Xll,2.0)+pow(Yll,2.0)) / earth_rad,
			 ( 1 / const_cone ) ) ) ) ) * RTD;
	    rtemp = atan2 ( Xll,  Yll ) * ( 1 / const_cone ) + loncnt;
	}

	if ( rtemp > PI )
	    gdsarr[4] = ( rtemp - TWOPI ) * RTD;
	else if ( rtemp < -PI )
	    gdsarr[4] = ( rtemp + TWOPI ) * RTD;
	else
	    gdsarr[4] = rtemp * RTD;

	/*
	 * Compute the lat/lon coordinates of the second point.
	 */
	if ( ( flag2 >> 7 ) == 0 ) {
	/*
	 *     Northern Hemisphere
	 */
	    gdsarr[5] = ( HALFPI - 2 *
	      atan ( pow ( sqrt (pow(Xur,2.0)+pow(Yur,2.0)) / earth_rad,
			 ( 1 / const_cone ) ) ) ) * RTD;
	    rtemp = atan2 ( Xur, -Yur ) * ( 1 / const_cone ) + loncnt;
	}
	else {
	/*
	 *     Southern Hemisphere
	 */
	    gdsarr[5] = ( -1 *  ( HALFPI - 2 *
	      atan ( pow ( sqrt (pow(Xur,2.0)+pow(Yur,2.0)) / earth_rad,
			 ( 1 / const_cone ) ) ) ) ) * RTD;
	    rtemp = atan2 ( Xur,  Yur ) * ( 1 / const_cone ) + loncnt;
	}

        if ( rtemp > PI )
	    gdsarr[6] = ( rtemp - TWOPI ) * RTD;
        else if ( rtemp < -PI )
	    gdsarr[6] = ( rtemp + TWOPI ) * RTD;
        else
	    gdsarr[6] = rtemp * RTD;

	/*
	 * Set the projection angle information, the number of x,y points,
	 * and the coded flags.
	 */
	gdsarr[7] = (float) ( Latin1 / 1000000.0 );
	gdsarr[8] = (float) ( LoV    / 1000000.0 );
	gdsarr[9] = (float) ( Latin2 / 1000000.0 );

	gdsarr[1] = (float)Nx;
	gdsarr[2] = (float)Ny;


}
