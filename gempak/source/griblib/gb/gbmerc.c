#include "gbcmn.h"

void gb_merc ( unsigned char *ptarray )
/************************************************************************
 * gb_merc								*
 *                                                                      *
 * This routine will get the GDS information for a Mercator grid. All   *
 * of the information is put into the data structure for the GDS.       *
 *                                                                      *
 * gb_merc ( ptarray )							*
 *									*
 * Input parameters:                                                    *
 *      *ptarray	unsigned char	Data buffer                     *
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI          02/93                                           *
 * S. Jacobs/EAI        10/93           Copied from GBUTIL              *
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * L. Sager/NMC		 8/95		Fix nonstandard values of	*
 *                                          longitude			*
 * D.W.Plummer/NCEP	 2/96		Cleanup GBDIAGs and comments	*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 ***********************************************************************/
{
int	La1, La2, Lo1, Lo2, Nx, Ny, Latin, mode, flag;
int	indx;

/*---------------------------------------------------------------------*/
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
	 * BYTES 24-26
	 * Latin - latitude at which the Mercator projection
	 * 	cylinder interests the Earth
	 */
	indx = 23;
        Latin = gb_btoi(ptarray, indx, 3, TRUE );

	/*
	 * BYTE 27
	 * Reserved - set to zero
	 */
	indx = 26;
        gb_btoi(ptarray, indx, 1, FALSE );

	/*
	 * BYTE 28
	 * Scanning mode
	 */
	indx = 27;
        mode = gb_btoi(ptarray, indx, 1, FALSE );

	/*
	 * BYTES 29-31
	 * Di - longitudinal direction grid length
	 */
	indx = 28;
        gb_btoi(ptarray, indx, 3, FALSE );

	/*
	 * BYTES 32-34
	 * Dj - latitudinal direction grid length
	 */
	indx = 31;
        gb_btoi(ptarray, indx, 3, FALSE );

	/*
	 * BYTES 35-42
	 * Reserved - set to zero
	 */
	indx = 34;
        gb_btoi(ptarray, indx, 4, FALSE );

	indx = 38;
        gb_btoi(ptarray, indx, 4, FALSE );

	/*
	 * Compute the lat/lon of the first and second grid points.
	 * Put the coordinates into pairs representing the Lower Left
	 * and Upper Right points.
	 */
        gds.latll = G_MIN(La1, La2) / 1000.00;
        gds.latur = G_MAX(La1, La2) / 1000.00;

	/*
	 * Make sure that the longitudes are between -180 and +180 
	 */
        Lo1 = Lo1 % 360000;
	Lo2 = Lo2 % 360000;
	
	if ( Lo1 >  180000 ) Lo1 = Lo1 - 360000;
	if ( Lo1 < -180000 ) Lo1 = Lo1 + 360000;
 	if ( Lo2 >  180000 ) Lo2 = Lo2 - 360000;
	if ( Lo2 < -180000 ) Lo2 = Lo2 + 360000;
        
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
	 * Set the projection angle information
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
	    printf ( " GDS bytes 24 - 26 (Latin)         = %d\n", Latin );
	    printf ( " GDS byte       27 (skipped)\n" );
	    printf ( " GDS byte       28 (mode)          = %d\n", mode );
	    printf ( " GDS bytes 29 - 31 (skipped)\n" );
	    printf ( " GDS bytes 32 - 34 (skipped)\n" );
	    printf ( " GDS bytes 35 - 38 (skipped)\n" );
	    printf ( " GDS bytes 39 - 42 (skipped)\n" );
	}

}
