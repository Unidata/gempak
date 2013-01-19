#include "gbcmn.h"

void gb_ltln ( unsigned char *ptarray )
/************************************************************************
 * gb_ltln								*
 *									*
 * This routine will get the GDS information for a Lat-Lon grid. All 	*
 * of the information is put into the data structure for the GDS.	*
 *									*
 * gb_ltln ( ptarray )							*
 *									*
 * Input parameters:							*
 *	*ptarray	unsigned char	Data buffer			*
 **									*
 * Log:									*
 * J. Chou/EAI		02/93						*
 * S. Jacobs/EAI	10/93		Copied from GBUTIL		*
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * D.W.Plummer/NCEP	 2/96		Cleanup GBDIAGs and comments	*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 ***********************************************************************/
{
int	La1, La2, Lo1, Lo2, Nx, Ny, flag, mode;
int	indx;

/*---------------------------------------------------------------------*/
	if ( gds.length >= 32 ) {

	    /*
	     * Nx - number of points along a parallel
	     * BYTES 7-8
	     */
	    indx = 6;
	    Nx = gb_btoi(ptarray, indx, 2, FALSE );

	    /*
	     * Ny - number of points along a meridian
	     * BYTES 9-10
	     */
	    indx = 8;
	    Ny = gb_btoi(ptarray, indx, 2, FALSE );

	    /*
	     * La1 - latitude of first grid point
	     * BYTES 11-13
	     */
	    indx = 10;
	    La1 = gb_btoi(ptarray, indx, 3, TRUE );

	    /*
	     * Lo1 - longitude of first grid point
	     * BYTES 14-16
	     */
	    indx = 13;
	    Lo1 = gb_btoi(ptarray, indx, 3, TRUE );

	    /*
	     * Resolution and component flags
	     * BYTE 17
	     */
	    indx = 16;
	    flag = gb_btoi(ptarray, indx, 1, FALSE );

	    /*
	     * La2 - latitude of last grid point
	     * BYTES 18-20
	     */
	    indx = 17;
	    La2 = gb_btoi(ptarray, indx, 3, TRUE );

	    /*
	     * Lo2 - longitude of last grid point
	     * BYTES 21-23
	     */
	    indx = 20;
	    Lo2 = gb_btoi(ptarray, indx, 3, TRUE );

	    /*
	     * Di - i direction increment
	     * BYTES 24-25
	     */
	    indx = 23;
	    gb_btoi(ptarray, indx, 2, TRUE );

	    /*
	     * Dj - j direction increment
	     * BYTES 26-27
	     */
	    indx = 25;
	    gb_btoi(ptarray, indx, 2, TRUE );

	    /*
	     * Scanning mode
	     * BYTE 28
	     */
	    indx = 27;
	    mode = gb_btoi(ptarray, indx, 1, FALSE );

	    /*
	     * Reserved - set to zero
	     * BYTES 29-32
	     */
	    indx = 28;
	    gb_btoi(ptarray, indx, 4, FALSE );

	}

	if ( gds.length >= 42 ) {

	    /*
	     * Latitude of the southern pole in millidegrees (integer)
	     * BYTES 33-35
	     */
	    indx = 32;
	    gb_btoi(ptarray, indx, 3, TRUE );

	    /*
	     * Longitude of the southern pole in millidegrees (integer)
	     * BYTES 36-38
	     */
	    indx = 35;
	    gb_btoi(ptarray, indx, 3, TRUE );

	    /*
	     * Angle of rotation OR Stretching factor
	     * BYTES 39-42
	     */
	    indx = 38;
	    gb_btoi(ptarray, indx, 1, FALSE );

	    indx = 39;
	    gb_btoi(ptarray, indx, 3, FALSE );

	}

	if ( gds.length >= 52 ) {

	    /*
	     * Latitude of pole of stretching in millidegrees (integer)
	     * BYTES 43-45
	     */
	    indx = 42;
	    gb_btoi(ptarray, indx, 3, FALSE );

	    /*
	     * Longitude of the southern pole in millidegrees (integer)
	     * BYTES 46-48
	     */
	    indx = 45;
	    gb_btoi(ptarray, indx, 3, FALSE );

	    /*
	     * Stretching factor
	     * BYTES 49-52
	     */
	    indx = 48;
	    gb_btoi(ptarray, indx, 1, FALSE );

	    indx = 49;
	    gb_btoi(ptarray, indx, 3, FALSE );

	}

	/*
	 * Compute the lat/lon coordinates for the Lower Left and
	 * Upper Right points.
	 */
	gds.latll = G_MIN( La1, La2 ) / 1000.00;
	gds.latur = G_MAX( La1, La2 ) / 1000.00;

	/*
	 * Check the scanning mode to determine which longitude is on
	 * the left and which is on the right.
	 */
	if ( ( mode >> 7 ) == 0 ) { 
	    gds.lonll = Lo1 / 1000.00;
	    gds.lonur = Lo2 / 1000.00;
	}
	else {
	    gds.lonll = Lo2 / 1000.00;
	    gds.lonur = Lo1 / 1000.00;
	}

	/*
	 * Set the projection angle information, the number of x,y points,
	 * and the coded flags.
	 */
	gds.angle1 = 0.0;
	gds.angle2 = 0.0;
	gds.angle3 = 0.0;

	gds.kx = Nx;
	gds.ky = Ny;

	gds.flag1 = flag;
	gds.scan_mode = mode;

	if ( GBDIAG_GDS == TRUE )  {
		printf ( " GDS bytes  7 -  8 (Nx)            = %d\n", Nx );
		printf ( " GDS bytes  9 - 10 (Ny)            = %d\n", Ny );
		printf ( " GDS bytes 11 - 13 (La1)           = %d\n", La1 );
		printf ( " GDS bytes 14 - 16 (Lo1)           = %d\n", Lo1 );
		printf ( " GDS byte       17 (flag)          = %d\n", flag );
		printf ( " GDS bytes 18 - 20 (La2)           = %d\n", La2 );
		printf ( " GDS bytes 21 - 23 (Lo2)           = %d\n", Lo2 );
		printf ( " GDS bytes 24 - 25 (skipped)\n" );
		printf ( " GDS bytes 26 - 27 (skipped)\n" );
		printf ( " GDS byte       28 (mode)          = %d\n", mode );
		printf ( " GDS bytes 29 - 32 (skipped)\n" );
		printf ( " GDS bytes 33 - 35 (skipped)\n" );
		printf ( " GDS bytes 36 - 38 (skipped)\n" );
		printf ( " GDS byte       39 (skipped)\n" );
		printf ( " GDS bytes 40 - 42 (skipped)\n" );
		printf ( " GDS bytes 43 - 45 (skipped)\n" );
		printf ( " GDS bytes 46 - 48 (skipped)\n" );
		printf ( " GDS byte       49 (skipped)\n" );
		printf ( " GDS bytes 50 - 52 (skipped)\n" );
	}

}
