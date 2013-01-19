#include "gbcmn.h"

void gb_polr ( unsigned char *ptarray )
/************************************************************************
 * gb_polr                                                              *
 *                                                                      *
 * This routine will get the GDS information for a Polar Stereographic	*
 * grid. All of the information is put into the data structure for	*
 * the GDS.								*
 *                                                                      *
 * gb_polr ( ptarray )							*
 *									*
 * Input parameters:                                                    *
 *      *ptarray	unsigned char	Data buffer                     *
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI          02/93                                           *
 * S. Jacobs/EAI        10/93           Copied from GBUTIL              *
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * D.W.Plummer/NCEP      2/96      	Cleanup GBDIAGs and comments	*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 ***********************************************************************/
{
int	Dx, Dy, flag1, flag2, mode, Nx, Ny, La1, Lo1, LoV, indx;

double	loncnt, lat1, lon1, rtemp, X1, X2, Y1, Y2,
	TDx, TDy, Xll, Xur, Yll, Yur;

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
	lon1 = ( Lo1 / 1000.0 ) * DEG_TO_RAD;

	/*
	 * BYTE 17
	 * Resolution and component flags
	 */
	indx = 16;
	flag1 = gb_btoi(ptarray, indx, 1, FALSE );

	/*
	 * BYTES 18-20
	 * Lov - orientation of the grid (center longitude)
	 */
	indx = 17;
	LoV = gb_btoi(ptarray, indx, 3, TRUE );
	loncnt = ( LoV / 1000.00 ) * DEG_TO_RAD;

	/*
	 * BYTES 21-23
	 * Dx - X-direction grid length
	 */
	indx = 20;
	Dx = gb_btoi(ptarray, indx, 3, TRUE );

	/*
	 * BYTES 24-26
	 * Dy - Y-dirction grid length
	 */
	indx = 23;
	Dy = gb_btoi(ptarray, indx, 3, TRUE );

	/*
	 * BYTE 27
	 * Projection center flag  [0=NP, 1=SP]
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
	 * BYTES 29-32
	 * Reserved - set to zero
	 */
	indx = 28;
	gb_btoi(ptarray, indx, 4, FALSE );

	/*
	 * Compute the linear coordinates of the first point.
	 */
	if ( flag2 == 0 ) {
		/*
		 * Northern Hemisphere
		 */
		X1 =  RADIUS * tan ( PI4TH - lat1/2 ) * sin ( lon1-loncnt );
		Y1 = -RADIUS * tan ( PI4TH - lat1/2 ) * cos ( lon1-loncnt );
	}
	else {
		/*
		 * Southern Hemisphere
		 */
		X1 = RADIUS * tan ( PI4TH + lat1/2 ) * sin ( lon1-loncnt );
		Y1 = RADIUS * tan ( PI4TH + lat1/2 ) * cos ( lon1-loncnt );
	}

	/*
	 * Compute the grid spacing
	 */
	TDx = Dx / ( 1 + sin ( PI3RD ) );
	TDy = Dy / ( 1 + sin ( PI3RD ) );

	/*
	 * Compute the linear coordinates of the second point.
	 * Check the scanning mode to determine +/- grid spacing.
	 */
	if ( ( mode >> 7 ) == 1 )
	    X2 = X1 + ( Nx - 1 ) * ( -TDx );
	else
	    X2 = X1 + ( Nx - 1 ) * TDx;

	if ( ( ( mode >> 6 ) & 1 ) == 1 )
	    Y2 = Y1 + ( Ny - 1 ) * TDy;
	else
	    Y2 = Y1 + ( Ny - 1 ) * ( -TDy );

	/*
	 * Get the point coordinates paired as Lower Left and Upper Right.
	 */
	Xll = G_MIN( X1, X2 );
	Yll = G_MIN( Y1, Y2 );
	Xur = G_MAX( X1, X2 );
	Yur = G_MAX( Y1, Y2 );

	/*
	 * Compute the lat/lon of the first point.
	 */
	if ( flag2 == 0 ) {
		/*
		 * Northern Hemisphere
		 */
		gds.latll = ( HALFPI - 2 *
		      atan2 ( sqrt ( pow (Xll,2.0) + pow (Yll,2.0) ),
			      RADIUS ) ) * RAD_TO_DEG;
		rtemp = loncnt + atan2 ( Xll, -Yll );
	}
	else {
		/*
		 * Southern Hemisphere
		 */
		gds.latll = - ( HALFPI - 2 *
		      atan2 ( sqrt ( pow (Xll,2.0) + pow (Yll,2.0) ),
			      RADIUS ) ) * RAD_TO_DEG;
		rtemp = loncnt + atan2 ( Xll,  Yll );
	}

	if ( rtemp > PI )
	    gds.lonll = ( rtemp - TWOPI ) * RAD_TO_DEG;
	else if ( rtemp  < -PI )
	    gds.lonll = ( rtemp + TWOPI ) * RAD_TO_DEG;
	else
	    gds.lonll = rtemp * RAD_TO_DEG;

	/*
	 * Compute the lat/lon of the second point.
	 */
	if ( flag2 == 0 ) {
		/*
		 * Northern Hemisphere
		 */
		gds.latur = ( HALFPI - 2 *
		      atan2 ( sqrt ( pow (Xur,2.0) + pow (Yur,2.0) ),
			      RADIUS ) ) * RAD_TO_DEG;
		rtemp = loncnt + atan2 ( Xur, -Yur );
	}
	else {
		/*
		 * Southern Hemisphere
		 */
		gds.latur = - ( HALFPI - 2 *
		      atan2 ( sqrt ( pow (Xur,2.0) + pow (Yur,2.0) ),
			      RADIUS ) ) * RAD_TO_DEG;
		rtemp = loncnt + atan2 ( Xur,  Yur );
	}

	if ( rtemp > PI )
	    gds.lonur = ( rtemp - TWOPI ) * RAD_TO_DEG;
	else if ( rtemp < -PI )
	    gds.lonur = ( rtemp + TWOPI ) * RAD_TO_DEG;
	else
	    gds.lonur = rtemp * RAD_TO_DEG;

		/*
		 * Set the proj angle information, the number of x,y points,
 		 * and the coded flags.
		 */
	if ( flag2 == 0 ) {
		/*
		 * North Pole
		 */
		gds.angle1 = 90.0;
	}
	else {
		/*
		 * South Pole
		 */
 		gds.angle1 = -90.0;
	}
	gds.angle2 = (float) ( LoV / 1000.0 );
	gds.angle3 = 0.0;

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
	    printf ( " GDS byte       28 (scan_mode)     = %d\n", mode );
	    printf ( " GDS bytes 29 - 32 (skipped)\n" );
	}

}
