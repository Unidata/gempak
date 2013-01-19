#include "gb2def.h"

void gb2_gdtpstr( float *navblk, int *igdtmpl, int *iret )
/************************************************************************
 * gb2_gdtpstr								*
 *                                                                      *
 * This routine converts a STR Gempak grid navigation block to a        *
 * GRIB2 Grid Definition Template 3.20.					*
 *                                                                      *
 * gb2_gdtpstr ( navblk, igdtmpl, iret )				*
 *									*
 * Input parameters:                                                    *
 *  *navblk   		float       Decoded GRIB2 structure             *
 *									*
 * Output parameters:                                                   *
 *  *igdtmpl            int        GDT 3.20 values 			*
 *  *iret               int        Return code                          *
 *                                    -36 = Projection not STR		*
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP          08/05    Calculations taken from GDS_STR    *
 * S. Gilbert/NCEP          03/06    Chngs to remove compiler warnings  *
 ***********************************************************************/
{

        double     rlat1, rlon1, rlat2, rlon2, dx, dy, polat, rlov;
        double     rnx, rny, sign, clon, re, x1, y1, x2, y2;
        double     tlat1, tlon1;
        double     tan1, tan2, dlon1, dlon2;

        int        ier;
        int        nx, ny;

/*---------------------------------------------------------------------*/
	*iret = 0;

        /*
         *  ensure grid navigation block is STR
         */
        if ( strncmp( (char *)(navblk+1), "STR", 3) != 0 ) {
           *iret=-36;
           er_wmsg("GB", iret, (char *)(navblk+1), &ier, 2, 4 );
        }

        nx = G_NINT(navblk[4]);
        ny = G_NINT(navblk[5]);
        rlat1 = navblk[6];
        rlon1 = navblk[7];
        rlat2 = navblk[8];
        rlon2 = navblk[9];
        polat = navblk[10];
        rlov = navblk[11];

        /*
         *  compute the grid increments
         */
        if ( polat > 0.0 ) sign = -1.0;
        else sign = 1.0;
        rnx = navblk[4];
        rny = navblk[5];
        tlat1 = rlat1 * DTR / 2.;
        rlat2 = rlat2 * DTR / 2.;
        tlon1 = rlon1 * DTR;
        rlon2 = rlon2 * DTR;
        clon = rlov * DTR;
        re = RADIUS;
        tan1 = tan ( PI4TH + sign * tlat1 );
        tan2 = tan ( PI4TH + sign * rlat2 );
        dlon1 = tlon1 - clon;
        dlon2 = rlon2 - clon;
        x1 = re * tan1 * sin ( dlon1 );
        y1 = sign * re * tan1 * cos ( dlon1 );
        x2 = re * tan2 * sin ( dlon2 );
        y2 = sign * re * tan2 * cos ( dlon2 );
        dx = ( x2 - x1 ) * 1.8660254 / ( rnx - 1. );
        dy = ( y2 - y1 ) * 1.8660254 / ( rny - 1. );

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
        igdtmpl[11] = 56;                     /* Res and Comp flags      */
        igdtmpl[12] = 60000000;               /* Lat where Dx,Dy true    */
        if ( rlov < 0.0 ) rlov += 360.0;
        igdtmpl[13] = G_NINT(rlov*1000000.0);   
                                              /* Lon of Orientation      */


        igdtmpl[14] = G_NINT(dx*1000.0);      /* Dx                      */
        igdtmpl[15] = G_NINT(dy*1000.0);      /* Dy                      */
        igdtmpl[16] = 0;
        if ( polat < 0.0 ) igdtmpl[16] = 128; /* Projection center flag  */
        igdtmpl[17] = 64;                     /* Scanning mode           */

}
