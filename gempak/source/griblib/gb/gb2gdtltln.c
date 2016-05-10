#include "gb2def.h"

void gb2_gdtltln( float *navblk, int *igdtmpl, int *iret )
/************************************************************************
 * gb2_gdtltln								*
 *                                                                      *
 * This routine converts a CED Gempak grid navigation block to a        *
 * GRIB2 Grid Definition Template 3.0.					*
 *                                                                      *
 * gb2_gdtltln ( navblk, igdtmpl, iret )				*
 *									*
 * Input parameters:                                                    *
 *  *navblk   		float       Decoded GRIB2 structure             *
 *									*
 * Output parameters:                                                   *
 *  *igdtmpl            int        GDT 3.0 values 			*
 *  *iret               int        Return code                          *
 *                                    -36 = Projection not CED		*
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP      08/05	Calculations taken from GDS_CED    	*
 * S. Gilbert/NCEP      03/06	Chngs to remove compiler warnings  	*
 * C. Bailey/HPC        01/07	Chngs to Dx calculation            	*
 * S. Jacobs/NCEP	 6/13	Fixed check for dx calculation to	*
 *				include 0.0 in the range for rlon1	*
 * J. Wu/SGT         03/15	Checked dx calc. for all 9 cases(R6160) *
 ***********************************************************************/
{

        double     rlat1, rlon1, rlat2, rlon2, dx, dy;

        int        ier;
        int        nx, ny;

/*---------------------------------------------------------------------*/
	*iret = 0;

        /*
         *  ensure grid navigation block is CED
         */
        if ( strncmp( (char *)(navblk+1), "CED", 3) != 0 ) {
           *iret=-36;
           er_wmsg("GB", iret, (char *)(navblk+1), &ier, 2, 4 );
        }

        nx = G_NINT(navblk[4]);
        ny = G_NINT(navblk[5]);
        rlat1 = navblk[6];
        rlon1 = navblk[7];
        rlat2 = navblk[8];
        rlon2 = navblk[9];

        /*
         *  Set Grid Definition Template
         */

        igdtmpl[0] = 1;                       /* Earth Assumed Spherical */
        igdtmpl[1] = 0;                       /* Radius scale factor     */
        igdtmpl[2] = G_NINT(RADIUS);          /* Radius of Earth         */
        igdtmpl[3] = 0;                       /* Oblate info     n/a     */
        igdtmpl[4] = 0;                       /* Oblate info     n/a     */
        igdtmpl[5] = 0;                       /* Oblate info     n/a     */
        igdtmpl[6] = 0;                       /* Oblate info     n/a     */
        igdtmpl[7] = nx;                      /* Kx                      */
        igdtmpl[8] = ny;                      /* Kx                      */
        igdtmpl[9] = 0;                       /* Basic Angle             */
        igdtmpl[10] = 0;                      /* Subdivision             */
        igdtmpl[11] = G_NINT(rlat1*1000000.0);
					      /* Lat of 1st grid point   */
   	    /*
   	     * Calculate Dx - there are nine cases here with rlon1 >0, <0, =0.0
   	     *                where rlon2 could be >0, <0, =0.0 as well. All cases
   	     *                should produce a positive dx that is greater than 0
   	     *                but less than or equal to { 360.0/(nx-1) } - R6160.
   	     */
        if ( (( rlon1 > 0.0 || G_DIFF(rlon1,0.0) ) && ( rlon2 < 0.0 || G_DIFF(rlon2,0.0) ))
        	   || ( (rlon1 < 0 && rlon2 < 0) && (rlon1 > rlon2) ) ) {
             dx = ( (rlon2+360) - rlon1) / (nx-1);
        } else {
             dx = (rlon2 - rlon1) / (nx-1);
        }

        if ( rlon1 < 0.0 ) rlon1 += 360.0;
        igdtmpl[12] = G_NINT(rlon1*1000000.0);   
                                              /* Lon of 1st grid point   */
        igdtmpl[13] = 48;                     /* Res and Comp flags      */
        igdtmpl[14] = G_NINT(rlat2*1000000.0);
                                              /* Lat of last grid point  */
        if ( rlon2 < 0.0 ) rlon2 += 360.0;
        igdtmpl[15] = G_NINT(rlon2*1000000.0);   
                                              /* Lon of last grid point  */

        igdtmpl[16] = G_NINT(dx*1000000.0);   /* Dx                   */
        dy = (rlat2 - rlat1) / (ny-1);
        igdtmpl[17] = G_NINT(dy*1000000.0);   /* Dy                   */
        igdtmpl[18] = 64;                     /* Scanning mode           */

}
