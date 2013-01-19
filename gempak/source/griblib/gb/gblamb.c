#include "gbcmn.h"

void gb_lamb ( unsigned char *ptarray )
/************************************************************************
 * gb_lamb								*
 *                                                                      *
 * This routine will get the GDS information for a Lambert grid. All    *
 * of the information is put into the data structure for the GDS.       *
 *                                                                      *
 * gb_lamb ( ptarray )							*
 *									*
 * Input parameters:                                                    *
 *      *ptarray	unsigned char	Data buffer                     *
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
 ***********************************************************************/
{

int		Latin1, Latin2, Nx, Ny, flag1, flag2, mode,
		La1, Lo1, LoV, Dx, Dy, indx;

double		earth_rad, const_cone, Xll, Yll, Xur, Yur,
		lat1, lon1, loncnt, angle1, angle2,
		X1, X2, Y1, Y2, alpha, rtemp;

/*---------------------------------------------------------------------*/
	/*
	 * BYTES 7-8
	 * Nx - number of points along X-axis
	 */
	indx = 6;
	Nx = gb_btoi(ptarray, indx, 2, FALSE );

	/*
	 * BYTES 9-10
	 * Ny - number of points along Y-axis
	 */
	indx = 8;
	Ny = gb_btoi(ptarray, indx, 2, FALSE );

	/*
	 * BYTES 11-13
	 * La1 - latitude of first grid point
	 */
	indx = 10;
	La1 = gb_btoi(ptarray, indx, 3, TRUE );
	lat1 = ( La1 / 1000.0 ) * DEG_TO_RAD;

	/*
	 * BYTES 14-16
	 * Lo1 - longitude of first grid point
	 */
	indx = 13;
	Lo1 = gb_btoi(ptarray, indx, 3, TRUE );
	if ( Lo1 > 180000 ) Lo1 = Lo1 - 360000;
	if ( Lo1 < -180000 ) Lo1 = Lo1 + 360000;
	lon1 = ( Lo1 / 1000.0 ) * DEG_TO_RAD;

	/*
	 * BYTE 17
	 * Resolution and component flags
	 */
	indx = 16;
	flag1 =  gb_btoi(ptarray, indx, 1, FALSE );

	/*
	 * BYTES 18-20
	 * Lov - orientation of the grid (center longitude)
	 */
	indx = 17;
	LoV = gb_btoi(ptarray, indx, 3, TRUE );
	if ( LoV > 180000 ) LoV = LoV - 360000;
	if ( LoV < -180000 ) LoV = LoV + 360000;
	loncnt = ( LoV / 1000.0 ) * DEG_TO_RAD;

	/*
	 * BYTES 21-23
	 * Dx - X-direction grid length
	 */
	indx = 20;
	Dx = gb_btoi(ptarray, indx, 3, TRUE );

	/*
	 * BYTES 24-26
	 * Dy - Y-direction grid length
	 */
	indx = 23;
	Dy = gb_btoi(ptarray, indx, 3, TRUE );

	/*
	 * Projection center flag [bit1=0=NP, bit1=1=SP;
	 * 			bit2=0-one projection center,
	 * 			bit2=1-bipolar and symmetric]
	 * BYTE 27
	 */
	indx = 26;
	flag2 = gb_btoi(ptarray, indx, 1, FALSE );

	/*
	 * BYTE 28
	 * Scanning mode
	 */
	indx = 27;
	mode = gb_btoi(ptarray, indx, 1, FALSE );

	/*
	 * BYTES 29-31
	 * Latin1 - first latitude from the pole at which the secant
	 * 	 cone cuts the sphere.
	 */
	indx = 28;
	Latin1 = gb_btoi(ptarray, indx, 3, TRUE );

	/*
	 * BYTES 32-34
	 * Latin2 - second latitude from the pole at which the secant
	 * 	 cone cuts the sphere.
	 */
	indx = 31;
	Latin2 = gb_btoi(ptarray, indx, 3, TRUE );

	/*
	 * BYTES 35-37
	 * Latitude of the southern pole in millidegrees
	 */
	indx = 34;
	gb_btoi(ptarray, indx, 3, TRUE );

	/*
	 * BYTES 38-40
	 * Longitude of the southern pole in millidegrees
	 */
	indx = 37;
	gb_btoi(ptarray, indx, 3, TRUE );

	/*
	 * BYTES 41-42
	 * Reserved - set to zero
	 */
	indx = 40;
	gb_btoi(ptarray, indx, 2, TRUE );

	/*
	 * Compute the constant of the cone from the colatitudes of the
	 * true latitudes.
	 */
	angle1 = HALFPI - ( fabs ( Latin1 / 1000.0 ) * DEG_TO_RAD );
	angle2 = HALFPI - ( fabs ( Latin2 / 1000.0 ) * DEG_TO_RAD );

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
	    X2 = X1 - ( Nx - 1 ) * alpha * Dx;
	else
	    X2 = X1 + ( Nx - 1 ) * alpha * Dx;

	if ( ( ( mode >> 6 ) & 1 ) == 1 )
	    Y2 = Y1 + ( Ny - 1 ) * alpha * Dy;
	else
	    Y2 = Y1 - ( Ny - 1 ) * alpha * Dy;

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
	    gds.latll = ( HALFPI - 2 *
	      atan ( pow ( sqrt (pow(Xll,2.0)+pow(Yll,2.0)) / earth_rad,
			 ( 1 / const_cone ) ) ) ) * RAD_TO_DEG;
	    rtemp = atan2 ( Xll, -Yll ) * ( 1 / const_cone ) + loncnt;
	}
	else {
	/*
	 *     Southern Hemisphere
	 */
	    gds.latll = ( -1 *  ( HALFPI - 2 *
	      atan ( pow ( sqrt (pow(Xll,2.0)+pow(Yll,2.0)) / earth_rad,
			 ( 1 / const_cone ) ) ) ) ) * RAD_TO_DEG;
	    rtemp = atan2 ( Xll,  Yll ) * ( 1 / const_cone ) + loncnt;
	}

	if ( rtemp > PI )
	    gds.lonll = ( rtemp - TWOPI ) * RAD_TO_DEG;
	else if ( rtemp < -PI )
	    gds.lonll = ( rtemp + TWOPI ) * RAD_TO_DEG;
	else
	    gds.lonll = rtemp * RAD_TO_DEG;

	/*
	 * Compute the lat/lon coordinates of the second point.
	 */
	if ( ( flag2 >> 7 ) == 0 ) {
	/*
	 *     Northern Hemisphere
	 */
	    gds.latur = ( HALFPI - 2 *
	      atan ( pow ( sqrt (pow(Xur,2.0)+pow(Yur,2.0)) / earth_rad,
			 ( 1 / const_cone ) ) ) ) * RAD_TO_DEG;
	    rtemp = atan2 ( Xur, -Yur ) * ( 1 / const_cone ) + loncnt;
	}
	else {
	/*
	 *     Southern Hemisphere
	 */
	    gds.latur = ( -1 *  ( HALFPI - 2 *
	      atan ( pow ( sqrt (pow(Xur,2.0)+pow(Yur,2.0)) / earth_rad,
			 ( 1 / const_cone ) ) ) ) ) * RAD_TO_DEG;
	    rtemp = atan2 ( Xur,  Yur ) * ( 1 / const_cone ) + loncnt;
	}

        if ( rtemp > PI )
	    gds.lonur = ( rtemp - TWOPI ) * RAD_TO_DEG;
        else if ( rtemp < -PI )
	    gds.lonur = ( rtemp + TWOPI ) * RAD_TO_DEG;
        else
	    gds.lonur = rtemp * RAD_TO_DEG;

	/*
	 * Set the projection angle information, the number of x,y points,
	 * and the coded flags.
	 */
	gds.angle1 = (float) ( Latin1 / 1000.0 );
	gds.angle2 = (float) ( LoV    / 1000.0 );
	gds.angle3 = (float) ( Latin2 / 1000.0 );

	gds.kx = Nx;
	gds.ky = Ny;

	gds.flag1 = flag1;
	gds.flag2 = flag2;
	gds.scan_mode = mode;

	if ( GBDIAG_GDS == TRUE )  {
	    printf ( " GDS bytes  7 -  8 (Nx)            = %d\n", Nx );
	    printf ( " GDS bytes  9 - 10 (Ny)            = %d\n", Ny );
	    printf ( " GDS bytes 11 - 13 (La1)           = %d\n", La1 );
	    printf ( " GDS bytes 14 - 16 (Lo1)           = %d\n", Lo1 );
	    printf ( " GDS byte       17 (flag1)         = %d\n", flag1 );
	    printf ( " GDS bytes 18 - 20 (LoV)           = %d\n", LoV );
	    printf ( " GDS bytes 21 - 23 (Dx)            = %d\n", Dx );
	    printf ( " GDS bytes 24 - 26 (Dy)            = %d\n", Dy );
	    printf ( " GDS byte       27 (flag2)         = %d\n", flag2 );
	    printf ( " GDS byte       28 (mode)          = %d\n", mode );
	    printf ( " GDS bytes 29 - 31 (Latin1)        = %d\n", Latin1 );
	    printf ( " GDS bytes 32 - 34 (Latin2)        = %d\n", Latin2 );
	    printf ( " GDS bytes 35 - 37 (skipped)\n" );
	    printf ( " GDS bytes 38 - 40 (skipped)\n" );
	    printf ( " GDS bytes 41 - 42 (skipped)\n" );
	}

}
