#include "gb2def.h"

void gb2_merc ( gribfield *gfld, float *gdsarr, int *scan_mode, int *iret )
/************************************************************************
 * gb2_merc								*
 *                                                                      *
 * This routine will get the GDS information for a Mercator grid. All   *
 * of the information is put into the data array for the GDS.           *
 *                                                                      *
 * gb2_merc ( gfld, gdsarr, scan_mode, iret )				*
 *									*
 * Input parameters:                                                    *
 *      *gfld   struct gribfield        Decoded GRIB2 structure         *
 *                                                                      *
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
 * L. Sager/NMC		 8/95		Fix nonstandard values of	*
 *                                          longitude			*
 * D.W.Plummer/NCEP	 2/96		Cleanup GBDIAGs and comments	*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 * S. Gilbert           11/04       Modified from gb_ltln for use w/ GRIB2  *
 ***********************************************************************/
{
        int La1, La2, Lo1, Lo2, Nx, Ny, mode;
        int Lo1e, Lo2e;
/*	int Di, Dj, flag, LaD, Latin, Ornt */

/*---------------------------------------------------------------------*/
	*iret = 0;
 
        gdsarr[0] = (float) 1.0;            /* specifies Mercator  */

	/*
	 * Nx - number of points along a parallel
	 */
        Nx = (int)gfld->igdtmpl[7];

	/*
	 * Ny - number of points along a meridian
	 */
        Ny = (int)gfld->igdtmpl[8];

	/*
	 * La1 - latitude of first grid point
	 */
        La1 = (int)gfld->igdtmpl[9];

	/*
	 * Lo1 - longitude of first grid point
	 */
        Lo1e = (int)gfld->igdtmpl[10];
        Lo1=Lo1e;
        if ( Lo1 > 180000000 ) Lo1 = Lo1 - 360000000;

	/*
	 * Resolution and component flags
	 */
/*        flag = (int)gfld->igdtmpl[11];  NOT used */

	/*
	 * LaD - latitude at which the Mercator projection intersects the Earth
	 */
/*        LaD = (int)gfld->igdtmpl[12];  NOT used */

	/*
	 * La2 - latitude of last grid point
	 */
        La2 = (int)gfld->igdtmpl[13];

	/*
	 * Lo2 - longitude of last grid point
	 */
        Lo2e = (int)gfld->igdtmpl[14];
        Lo2 = Lo2e;
        if ( Lo2 > 180000000 ) Lo2 = Lo2 - 360000000;

	/*
	 * Latin - latitude at which the Mercator projection
	 * 	cylinder interests the Earth
	 */
/*        Latin = (int)gfld->igdtmpl[12];  NOT used */

	/*
	 * Scanning mode
	 */
        mode = (int)gfld->igdtmpl[15];
        *scan_mode=mode;

	/*
	 * Ornt - Orientation of the grid
	 */
/*        Ornt = (int)gfld->igdtmpl[16];  NOT used */

	/*
	 * Di - longitudinal direction grid length
	 */
/*        Di = (int)gfld->igdtmpl[17];  NOT used */

	/*
	 * Dj - latitudinal direction grid length
	 */
/*        Dj = (int)gfld->igdtmpl[18];  NOT used */


	/*
	 * Compute the lat/lon of the first and second grid points.
	 * Put the coordinates into pairs representing the Lower Left
	 * and Upper Right points.
	 */
        gdsarr[3] = G_MIN(La1, La2) / 1000000.00;
        gdsarr[5] = G_MAX(La1, La2) / 1000000.00;

	/*
	 * Make sure that the longitudes are between -180 and +180 
	 */
/*   shouldn't need anymore  .....
        Lo1 = Lo1 % 360000000;
	Lo2 = Lo2 % 360000000;
	
	if ( Lo1 >  180000000 ) Lo1 = Lo1 - 360000000;
	if ( Lo1 < -180000000 ) Lo1 = Lo1 + 360000000;
 	if ( Lo2 >  180000000 ) Lo2 = Lo2 - 360000000;
	if ( Lo2 < -180000000 ) Lo2 = Lo2 + 360000000;
*/
        
	/*
	 * Check the scanning mode to determine which longitude is on
	 * the left and which is on the right.
	 */
	if ( ( mode >> 7 ) == 0 ) {
	    gdsarr[4] = Lo1 / 1000000.00;
	    gdsarr[6] = Lo2 / 1000000.00;
	}
	else {
	    gdsarr[4] = Lo2 / 1000000.00;
	    gdsarr[6] = Lo1 / 1000000.00;
	}

	/*
	 * Set the projection angle information
	 */
	gdsarr[7] = 0.0;
	gdsarr[8] = 0.0;
	gdsarr[9] = 0.0;

	gdsarr[1] = (float)Nx;
	gdsarr[2] = (float)Ny;


}
