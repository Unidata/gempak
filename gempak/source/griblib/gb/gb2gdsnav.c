#include "gb2def.h"

void gb2_gdsnav( gribfield *gfld, char *cproj,  int *kx,  int *ky, 
                float *gdsarr, float *corners, float *navblk, 
                int *scan_mode, int *iret )
/************************************************************************
 * gb2_gdsnav								*
 *									*
 * This function converts the GRIB grid information from the GDS        *
 * to a Gempak grid navigation block, and passes grid                   *
 * information back to the calling routine.                             *
 *									*
 * gb2_gdsnav ( gfld, cproj, kx, ky, corners, navblk, scan_mode, iret )	*
 *									*
 * Input parameters:							*
 *      *gfld   struct gribfield        Decoded GRIB2 structure         *
 *									*
 * Output parameters:							*
 *      *cproj          char            Grid projection                 *
 *      *kx             int             Number of columns               *
 *      *ky             int             Number of rows                  *
 *      *gdsarr         float           GDS information array           *
 *					   (GRIB projection number,	*
 *					   number of columns of data,	*
 *					   number of rows of data,	*
 *					   lat/lon of corners)		*
 *                     gdsarr[0] = Grid Type                            *
 *                     gdsarr[1] = Num of points in X direction         *
 *                     gdsarr[2] = Num of points in Y direction         *
 *                     gdsarr[3] = Lat of lower left grid point         *
 *                     gdsarr[4] = Lon of lower left grid point         *
 *                     gdsarr[5] = Lat of upper right grid point        *
 *                     gdsarr[6] = Lon of upper right grid point        *
 *                     gdsarr[7] = gds.angle1;                          *
 *                     gdsarr[8] = gds.angle2;                          *
 *                     gdsarr[9] = gds.angle3;                          *
 *      *corners        float           Grid corners                    *
 *      *navblk         float           Grid navigation block           *
 *      *scan_mode      int             GRIB2 scanning mode flag        *
 *	*iret		int		Return code			*
 *					-19 = Unrecognized GDT          *
 *									*
 **									*
 * Log:									*
 * J. Chou/EAI		 7/93						*
 * S. Jacobs/EAI	11/93	Changed GDS info array to FLOAT		*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * L. Sager		 8/95	Fix memory allocation error		*
 * S. Jacobs/NCEP	 1/96	Changed DA_READ to CFL_READ		*
 * D.W.Plummer/NCEP	 3/96	Changed cfl_ call sequence		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * S. Gilbert           11/04   Modified from gb_ggds for use with GRIB2. 
 * S. Gilbert/NCEP      10/06   Added call to GR_VNAV                   *
 ***********************************************************************/
{

        int     angflg, ier, valid;
/*---------------------------------------------------------------------*/
	*iret = 0;


	switch (gfld->igdtnum) {
         case 0:              /* lat-lon   */
         case 1:             
         case 2:             
         case 3:             
            gb2_ltln(gfld, gdsarr, scan_mode, iret );
            strncpy(cproj, "CED", 3);
            break;
         case 10:             /* Mercator   */
            gb2_merc(gfld, gdsarr, scan_mode, iret );
            strncpy(cproj, "MER", 3);
            break;
         case 20:             /* Polar Stereographic */
            gb2_polr(gfld, gdsarr, scan_mode, iret );
            strncpy(cproj, "STR", 3);
            break;
         case 30:             /* Lambert Conformal */
            gb2_lamb(gfld, gdsarr, scan_mode, iret );
            strncpy(cproj, "LCC", 3);
            break;
         case 40:              /* Gaussian   */
         case 41:             
         case 42:             
         case 43:             
            gb2_gaus(gfld, gdsarr, scan_mode, iret );
            strncpy(cproj, "CED", 3);
            break;
         default: 
            printf(" WARNING : Grid Definition Template 3.%d not supported.\n", (int)gfld->igdtnum );
            *iret=-19;
            break;
    }

    /*
     *  Set the number of columns and rows.
     */
    *kx = rint ( gdsarr [1] );
    *ky = rint ( gdsarr [2] );

    /*
     *  Set the lat/lon values of the corners.
     */
    corners [0] = gdsarr [3];
    corners [1] = gdsarr [4];
    corners [2] = gdsarr [5];
    corners [3] = gdsarr [6];

    /*
     *  Set the Navigation block.
     */

    angflg=1;
    gr_vnav  ( cproj, kx, ky, corners, corners+1,
               corners+2, corners+3, gdsarr+7, gdsarr+8,
               gdsarr+9, &angflg, &valid, &ier, strlen(cproj) );

    if ( ier == 0 ) 
        gr_mnav  ( cproj, kx, ky, corners, corners+1,
                   corners+2, corners+3, gdsarr+7, gdsarr+8,
                   gdsarr+9, &angflg, navblk, &ier, strlen(cproj) );
    else 
        *iret = -37;

}
