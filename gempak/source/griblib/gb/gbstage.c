#include "gbcmn.h"
#include "math.h"

void gb_stage ( unsigned char *ptarray )
/************************************************************************
 * gb_stage Arakawa staggered filled E.					*
 *									*
 * This routine will get the GDS information for a Lat-Lon grid. All 	*
 * of the information is put into the data structure for the GDS.	*
 *									*
 * from gb_ltln ( ptarray )						*
 *									*
 * Input parameters:							*
 *	*ptarray	unsigned char	Data buffer			*
 **									*
 * Log:									*
 ***********************************************************************/
{
int	La1, La2, Lo1, Lo2, Di, Dj, Nx, Ny, flag, mode;
int	indx, idrct, jdrct;

/*---------------------------------------------------------------------*/
	if ( gds.length >= 32 ) {

	    /*
	     * Nx - number of points along a parallel
	     * BYTES 7-8
	     */
	    indx = 6;
	    Nx = gb_btoi(ptarray, indx, 2, FALSE );
	    Nx = Nx * 2 - 1;

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
	     * La2 - latitude (ang1 of CED projection)
	     * BYTES 18-20
	     */
	    indx = 17;
	    La2 = gb_btoi(ptarray, indx, 3, TRUE );

	    /*
	     * Lo2 - longitude (ang2 of CED projection)
	     * BYTES 21-23
	     */
	    indx = 20;
	    Lo2 = gb_btoi(ptarray, indx, 3, TRUE );

	    /*
	     * Di - i direction increment
	     * BYTES 24-25
	     */
	    indx = 23;
	    Di = gb_btoi(ptarray, indx, 2, TRUE );

	    /*
	     * Dj - j direction increment
	     * BYTES 26-27
	     */
	    indx = 25;
	    Dj = gb_btoi(ptarray, indx, 2, TRUE );

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

	/*
	 * Check the scanning mode to determine which longitude is on
	 * the left and which is on the right.
	 */
	if ( ( ( mode >> 7 ) & 1 ) == 0 ) 
	    idrct = 1;
	else 
	    idrct = -1;

	if ( ( ( mode >> 6 ) & 1 ) == 0 )
	    jdrct = -1;
	else 
	    jdrct = 1;


	/*
	 * Compute the lat/lon coordinates for the Lower Left and
	 * Upper Right points.
	 */

        /* convert Di from 0,0 to rotated center latitude */
	Di = Di / cos ( (float)La2 * DEG_TO_RAD / 1000.00);

	gds.latll = La1 / 1000.00;
        gds.lonll = Lo1 / 1000.00;

        gds.latur = (float)( La1 + ( Dj * (Ny - 1) ) ) / 1000.00;

        /* already subtracted "1" from Nx, also 1 filled extra row */
        gds.lonur = (float)( Lo1 + ( Di * (Nx - 0) ) ) / 1000.00;

	/*
	 * Set the projection angle information, the number of x,y points,
	 * and the coded flags.
	 */
	gds.angle1 = La2 / 1000.00;
	gds.angle2 = Lo2 / 1000.0;
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
		printf ( " GDS bytes 24 - 25 (Di)            = %d\n", Di  );
		printf ( " GDS bytes 26 - 27 (Dj)            = %d\n", Dj  );
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
