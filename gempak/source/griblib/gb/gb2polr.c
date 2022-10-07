#include "gb2def.h"

void gb2_polr ( gribfield *gfld, float *gdsarr, int *scan_mode, int *iret )
/************************************************************************
 * gb2_polr                                                             *
 *                                                                      *
 * This routine will get the GDS information for a Polar Stereographic	*
 * grid. All of the information is put into the data array for	        *
 * the GDS.								*
 *                                                                      *
 * gb2_polr ( gfld, gdsarr, scan_mode, iret )	     	         	*
 *									*
 * Input parameters:                                                    *
 *      *gfld   struct gribfield        Decoded GRIB2 structure         *
 *									*
 * Ouput parameters:                                                    *
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
 * D.W.Plummer/NCEP      2/96      	Cleanup GBDIAGs and comments	*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 * S. Gilbert           11/04       Modified from gb_polr for use w/ GRIB2  *
 ***********************************************************************/
{
        int	Dx, Dy, flag2, mode, Nx, Ny, La1, Lo1, LoV;
        int     Lo1e, LoVe;
/*	int	flag1, LaD; */

        double	loncnt, lat1, lon1, rtemp, X1, X2, Y1, Y2,
        	TDx, TDy, Xll, Xur, Yll, Yur;

/*---------------------------------------------------------------------*/
     	*iret = 0;

	gdsarr[0] = (float) 5.0;     /*  Specifies Polar Stereographic  */

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
        /*if ( Lo1 > 180000000 ) Lo1 = Lo1 - 360000000;*/
        Lo1 = Lo1 - 360000000;
	lon1 = ( Lo1 / 1000000.0 ) * DTR;

	/*
	 * Resolution and component flags
	 */
/*	flag1 = (int)gfld->igdtmpl[11];  NOT used */

	/*
	 * LaD - Latitude where Dx and Dy are specified.
	 */
/*	LaD = (int)gfld->igdtmpl[12];  NOT used */

	/*
	 * Lov - orientation of the grid (center longitude)
	 */
	LoVe = (int)gfld->igdtmpl[13];
        LoV = LoVe;
        /*if ( LoV > 180000000 ) LoV = LoV - 360000000;*/
        LoV = LoV - 360000000;
	loncnt = ( LoV / 1000000.00 ) * DTR;

	/*
	 * Dx - X-direction grid length
	 */
	Dx = (int)gfld->igdtmpl[14];

	/*
	 * Dy - Y-direction grid length
	 */
	Dy = (int)gfld->igdtmpl[15];

	/*
	 * Projection center flag  [0=NP, 1=SP]
	 */
	flag2 = (int)gfld->igdtmpl[16];

	/*
	 * Scanning mode
	 */
	mode = (int)gfld->igdtmpl[17];
        *scan_mode=mode;


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
	TDx = ((float)Dx / 1000.0) / ( 1 + sin ( PI3RD ) );
	TDy = (Dy / 1000.0) / ( 1 + sin ( PI3RD ) );

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
		gdsarr[3] = ( HALFPI - 2 *
		      atan2 ( sqrt ( pow (Xll,2.0) + pow (Yll,2.0) ),
			      RADIUS ) ) * RTD;
		rtemp = loncnt + atan2 ( Xll, -Yll );
	}
	else {
		/*
		 * Southern Hemisphere
		 */
		gdsarr[3] = - ( HALFPI - 2 *
		      atan2 ( sqrt ( pow (Xll,2.0) + pow (Yll,2.0) ),
			      RADIUS ) ) * RTD;
		rtemp = loncnt + atan2 ( Xll,  Yll );
	}

	if ( rtemp > PI )
	    gdsarr[4] = ( rtemp - TWOPI ) * RTD;
	else if ( rtemp  < -PI )
	    gdsarr[4] = ( rtemp + TWOPI ) * RTD;
	else
	    gdsarr[4] = rtemp * RTD;

	/*
	 * Compute the lat/lon of the second point.
	 */
	if ( flag2 == 0 ) {
		/*
		 * Northern Hemisphere
		 */
		gdsarr[5] = ( HALFPI - 2 *
		      atan2 ( sqrt ( pow (Xur,2.0) + pow (Yur,2.0) ),
			      RADIUS ) ) * RTD;
		rtemp = loncnt + atan2 ( Xur, -Yur );
	}
	else {
		/*
		 * Southern Hemisphere
		 */
		gdsarr[5] = - ( HALFPI - 2 *
		      atan2 ( sqrt ( pow (Xur,2.0) + pow (Yur,2.0) ),
			      RADIUS ) ) * RTD;
		rtemp = loncnt + atan2 ( Xur,  Yur );
	}

	if ( rtemp > PI )
	    gdsarr[6] = ( rtemp - TWOPI ) * RTD;
	else if ( rtemp < -PI )
	    gdsarr[6] = ( rtemp + TWOPI ) * RTD;
	else
	    gdsarr[6] = rtemp * RTD;

		/*
		 * Set the proj angle information, the number of x,y points,
 		 * and the coded flags.
		 */
	if ( flag2 == 0 ) {
		/*
		 * North Pole
		 */
		gdsarr[7] = 90.0;
	}
	else {
		/*
		 * South Pole
		 */
 		gdsarr[7] = -90.0;
	}
	gdsarr[8] = (float) ( LoV / 1000000.0 );
	gdsarr[9] = 0.0;

	gdsarr[1] = (float)Nx;
	gdsarr[2] = (float)Ny;


}
