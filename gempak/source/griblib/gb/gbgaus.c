#include "gbcmn.h"

void gb_gaus ( unsigned char *ptarray )
/************************************************************************
 * gb_gaus								*
 *									*
 * This routine will get the GDS information for a Gaussian grid. All 	*
 * of the information is put into the data structure for the GDS.	*
 *									*
 * gb_gaus ( ptarray )							*
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
	int	La1, La2, Lo1, Lo2, Nx, Ny, flag, mode, indx;

/*---------------------------------------------------------------------*/
	/*	
	 * Check for different lengths of the GDS.
	 */

	if ( gds.length >= 32 ) {

	/*	
	 * BYTES 7-8
	 * Nx - number of points along a parallel
	 */
	indx = 6;
	    Nx = gb_btoi(ptarray, indx, 2, FALSE );

	/*	
	 * BYTES 9-10
	 * Ny - number of points along a meridian
	 */
	indx = 8;
	    Ny = gb_btoi(ptarray, indx, 2, FALSE );

	/*	
	 * BYTES 11-13
	 * La1 - latitude of first grid point
	 */
	indx = 10;
	    La1 = gb_btoi(ptarray, indx, 3, TRUE );

	/*	
	 * BYTES 14-16
	 * Lo1 - longitude of first grid point
	 */
	indx = 13;
	    Lo1 = gb_btoi(ptarray, indx, 3, TRUE );

	/*	
	 * BYTE 17
	 * Resolution and component flags
	 */
	indx = 16;
	    flag = gb_btoi(ptarray, indx, 1, FALSE );

	/*	
	 * BYTES 18-20
	 * La2 - latitude of last grid point
	 */
	indx = 17;
	    La2 = gb_btoi(ptarray, indx, 3, TRUE );

	/*	
	 * BYTES 21-23
	 * Lo2 - longitude of last grid point
	 */
	indx = 20;
	    Lo2 = gb_btoi(ptarray, indx, 3, TRUE );

	/*	
	 * BYTES 24-25
	 * Di - i direction increment
	 */
	indx = 23;
	    gb_btoi(ptarray, indx, 2, TRUE );

	/*	
	 * BYTES 26-27
	 * N - Number of parallels between a Pole and the Equator
	 */
	indx = 25;
	    gb_btoi(ptarray, indx, 2, TRUE );

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

	}

	if ( gds.length >= 42 ) {

	/*	
	 * BYTES 33-35
	 * Latitude of the southern pole in millidegrees (integer)
	 */
	indx = 32;
	    gb_btoi(ptarray, indx, 3, TRUE );

	/*	
	 * BYTES 36-38
	 * Longitude of the southern pole in millidegrees (integer)
	 */
	indx = 35;
	    gb_btoi(ptarray, indx, 3, TRUE );

	/*	
	 * BYTES 39-42
	 * Angle of rotation OR Stretching factor
	 */
	indx = 38;
	    gb_btoi(ptarray, indx, 1, FALSE );

	indx = 39;
	    gb_btoi(ptarray, indx, 3, FALSE );

	}

	if ( gds.length >= 52 ) {

	/*	
	 * BYTES 43-45
	 * Latitude of pole of stretching in millidegrees (integer)
	 */
	indx = 42;
	    gb_btoi(ptarray, indx, 3, FALSE );

	/*	
	 * BYTES 46-48
	 * Longitude of the southern pole in millidegrees (integer)
	 */
	indx = 45;
	    gb_btoi(ptarray, indx, 3, FALSE );

	/*	
	 * BYTES 49-52
	 * Stretching factor
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
