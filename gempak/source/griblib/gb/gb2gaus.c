#include "gb2def.h"

void gb2_gaus ( gribfield *gfld, float *gdsarr, int *scan_mode, int *iret )
/************************************************************************
 * gb2_gaus								*
 *									*
 * This routine will get the GDS information for a Gaussian grid. All 	*
 * of the information is put into the data array for the GDS.	        *
 *									*
 * gb2_gaus ( gfld, gdsarr, scan_Mode, iret )				*
 *									*
 * Input parameters:							*
 *      *gfld   struct gribfield        Decoded GRIB2 structure         *
 *									*
 * Output parameters:							*
 *  *gdsarr             float       GDS information array               *
 *                             (GRIB projection number,                 *
 *                             number of columns of data,               *
 *                             number of rows of data,                  *
 *                             lat/lon of corners)                      *
 *  *scan_mode          int             GRIB2 scanning mode flag        *
 *  *iret               int     Return code                             *
 *                          -19 = error on message                      *
 **									*
 * Log:									*
 * J. Chou/EAI		02/93						*
 * S. Jacobs/EAI	10/93		Copied from GBUTIL		*
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * D.W.Plummer/NCEP	 2/96		Cleanup GBDIAGs and comments	*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 * S. Gilbert/NCEP      11/04       Modified from gb_ltln for use w/ GRIB2  *
 ***********************************************************************/
{
	int	La1, La2, Lo1, Lo2, Nx, Ny, mode;
        int     Lo1e, Lo2e;
/*	int	Di, flag, Num; */
/*---------------------------------------------------------------------*/
	*iret = 0;

         gdsarr[0] = (float) 4.0;      /* Specifies Gaussian  */


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
	 La1 = (int)gfld->igdtmpl[11];

	/*	
	 * Lo1 - longitude of first grid point
	 */
	 Lo1e = (int)gfld->igdtmpl[12];
         Lo1 = Lo1e;
         if ( Lo1 > 180000000 ) Lo1 = Lo1 - 360000000;

	/*	
	 * Resolution and component flags
	 */
/*	 flag = (int)gfld->igdtmpl[13];  NOT used */

	/*	
	 * La2 - latitude of last grid point
	 */
	 La2 = (int)gfld->igdtmpl[14];

	/*	
	 * Lo2 - longitude of last grid point
	 */
	 Lo2e = (int)gfld->igdtmpl[15];
         Lo2 = Lo2e;
         if ( Lo2 > 180000000 ) Lo2 = Lo2 - 360000000;

	/*	
	 * Di - i direction increment
	 */
/*	 Di = (int)gfld->igdtmpl[16];  NOT used */

	/*	
	 * Num - Number of parallels between a Pole and the Equator
	 */
/*	 Num = (int)gfld->igdtmpl[17];  NOT used */

	/*	
	 * Scanning mode
	 */
	 mode = (int)gfld->igdtmpl[18];
        *scan_mode=mode;


	/*	
	 * Compute the lat/lon coordinates for the Lower Left and
	 * Upper Right points.
	 */
	gdsarr[3] = G_MIN( La1, La2 ) / 1000000.00;
	gdsarr[5] = G_MAX( La1, La2 ) / 1000000.00;

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
	 * Set the projection angle information, the number of x,y points,
	 * and the coded flags.
	 */
	gdsarr[7] = 0.0;
	gdsarr[8] = 0.0;
	gdsarr[9] = 0.0;

	gdsarr[1] = (float)Nx;
	gdsarr[2] = (float)Ny;


}
