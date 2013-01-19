#include "gb2def.h"

void gb2_gdtmerc( float *navblk, int *igdtmpl, int *iret )
/************************************************************************
 * gb2_gdtmerc								*
 *                                                                      *
 * This routine converts a MER Gempak grid navigation block to a        *
 * GRIB2 Grid Definition Template 3.10.					*
 *                                                                      *
 * gb2_gdtmerc ( navblk, igdtmpl, iret )				*
 *									*
 * Input parameters:                                                    *
 *  *navblk   		float       Decoded GRIB2 structure             *
 *									*
 * Output parameters:                                                   *
 *  *igdtmpl            int        GDT 3.10 values 			*
 *  *iret               int        Return code                          *
 *                                    -36 = Projection not MER		*
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP          08/05    Calculations taken from GDS_MER    *
 * S. Gilbert/NCEP          03/06    Chngs to remove compiler warnings  *
 ***********************************************************************/
{

        double     rlat1, rlon1, rlat2, rlon2, dx, dy;
        double     tlat1, tlon1, tlat2, tlon2;
        double     x1, y1, x2, y2, rnx, rny;

        int        ier;
        int        nx, ny;

/*---------------------------------------------------------------------*/
	*iret = 0;

        /*
         *  ensure grid navigation block is MER
         */
        if ( strncmp( (char *)(navblk+1), "MER", 3) != 0 ) {
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
         *  Calculate grid increments
         */
        rnx = navblk[4];
        rny = navblk[5];
        tlat1 = rlat1 * DTR / 2.;
        tlat2 = rlat2 * DTR / 2.;
        if ( rlon2 <  rlon1 ) tlon2 = rlon2 + 360.;
        else tlon2 = rlon2;
        tlon1 = rlon1 * DTR;
        tlon2 = tlon2 * DTR;
        x1 = 0.0;
        y1 = RADIUS * log ( tan ( PI4TH + tlat1 ) );
        x2 = RADIUS * ( tlon2 - tlon1 );
        y2 = RADIUS * log ( tan ( PI4TH + tlat2 ) );
        dx = ( x2 - x1 ) / ( rnx - 1. );
        dy = ( y2 - y1 ) / ( rny - 1. );

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
        igdtmpl[9] = G_NINT(rlat1*1000000.0);
                                              /* Lat of 1st grid point   */
        if ( rlon1 < 0.0 ) rlon1 += 360.0;
        igdtmpl[10] = G_NINT(rlon1*1000000.0);   
                                              /* Lon of 1st grid point   */
        igdtmpl[11] = 48;                     /* Res and Comp flags      */
        igdtmpl[12] = 0;                      /* LaD at equator          */

        igdtmpl[13] = G_NINT(rlat2*1000000.0);
                                              /* Lat of last grid point  */
        if ( rlon2 < 0.0 ) rlon2 += 360.0;
        igdtmpl[14] = G_NINT(rlon2*1000000.0);   
                                              /* Lon of last grid point  */
        igdtmpl[15] = 64;                     /* Scanning mode           */
        igdtmpl[16] = 0;                      /* Orientation of grid     */

        igdtmpl[17] = G_NINT(dx*1000.0);      /* Dx                      */
        igdtmpl[18] = G_NINT(dy*1000.0);      /* Dy                      */

}
