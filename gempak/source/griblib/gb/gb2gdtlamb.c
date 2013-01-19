#include "gb2def.h"

void gb2_gdtlamb( float *navblk, int *igdtmpl, int *iret )
/************************************************************************
 * gb2_gdtlamb								*
 *                                                                      *
 * This routine converts a LCC Gempak grid navigation block to a        *
 * GRIB2 Grid Definition Template 3.30.					*
 *                                                                      *
 * gb2_gdtlamb ( navblk, igdtmpl, iret )				*
 *									*
 * Input parameters:                                                    *
 *  *navblk   		float       Decoded GRIB2 structure             *
 *									*
 * Output parameters:                                                   *
 *  *igdtmpl            int        GDT 3.30 values 			*
 *  *iret               int        Return code                          *
 *                                    -36 = Projection not LCC		*
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP          08/05    Calculations taken from GDS_LCC    *
 * S. Gilbert/NCEP          03/06    Chngs to remove compiler warnings  *
 ***********************************************************************/
{
        int        ier;

        double     rlon, rlov, phi1, phi2, trult1, trult2;
        double     sign, rnx, rny, psi1, psi2;
        double     rlat1, rlon1, rlat2, rlon2, clon;
        double     re, cc, tan1, tan2, dlon1, dlon2;
        double     x1, y1, x2, y2, alfa, dx, dy;

/*---------------------------------------------------------------------*/
	*iret = 0;

        /*
         *  ensure grid navigation block is LCC
         */
        if ( strncmp( (char *)(navblk+1), "LCC", 3) != 0 ) {
           *iret=-36;
           er_wmsg("GB", iret, (char *)(navblk+1), &ier, 2, 4 );
        }

        /*
         *  Compute the grid increments.
         */
        rnx=navblk[4];
        rny=navblk[5];
        rlat1 = navblk[6];
        rlon1 = navblk[7];
        rlat2 = navblk[8];
        rlon2 = navblk[9];
        phi1 = navblk[10];
        rlov = navblk[11];
        phi2 = navblk[12];

        if ( fabs(phi1) > fabs(phi2) ) {
           trult1 = phi1;
           trult2 = phi2;
           if ( phi1 > 0.0 ) sign = -1.0;
           else sign = 1.0;
        }
        if ( fabs(phi2) > fabs(phi1) ) {
           trult1 = phi2;
           trult2 = phi1;
           if ( phi2 > 0.0 ) sign = -1.0;
           else sign = 1.0;
        }
        else {
           trult1 = phi1;
           trult2 = phi2;
           if ( phi1 > 0.0 ) sign = -1.0;
           else sign = 1.0;
        }

        rlat1 = rlat1 * DTR / 2.;
        rlat2 = rlat2 * DTR / 2.;
        rlon1 = rlon1 * DTR;
        rlon2 = rlon2 * DTR;
        clon = rlov * DTR;

        /*
         *  Compute the cone constant
         */
        psi1 = HALFPI + sign * phi1 * DTR;
        psi2 = HALFPI + sign * phi2 * DTR;
        if ( G_DIFF (phi1, phi2) ) 
            cc = cos ( psi1 );
        else {
            cc = log ( sin ( psi2 ) / sin ( psi1 ) );
            cc = cc / log ( tan ( psi2 / 2. ) / tan ( psi1 / 2. ) );
        }

        re = RADIUS / cc;
        tan1 = pow ( tan ( PI4TH + sign * rlat1 ) , cc);
        tan2 = pow ( tan ( PI4TH + sign * rlat2 ) , cc);
        dlon1 = ( rlon1 - clon ) * cc;
        dlon2 = ( rlon2 - clon ) * cc;
        x1 = re * tan1 * sin ( dlon1 );
        y1 = sign * re * tan1 * cos ( dlon1 );
        x2 = re * tan2 * sin ( dlon2 );
        y2 = sign * re * tan2 * cos ( dlon2 );
        alfa = sin ( psi1 ) / pow( tan ( psi1 / 2. ) , cc );
        dx = ( x2 - x1 ) * alfa / ( rnx - 1. );
        dy = ( y2 - y1 ) * alfa / ( rny - 1. );

        /*
         *  Set Grid Definition Template
         */

        igdtmpl[0] = 1;                       /* Earth Assumed Spherical */
        igdtmpl[1] = 0;                       /* Radius scale factor     */
        igdtmpl[2] = G_NINT( RADIUS );        /* Radius of Earth         */
        igdtmpl[3] = 0;                       /* Oblate info     n/a     */
        igdtmpl[4] = 0;                       /* Oblate info     n/a     */
        igdtmpl[5] = 0;                       /* Oblate info     n/a     */
        igdtmpl[6] = 0;                       /* Oblate info     n/a     */
        igdtmpl[7] = G_NINT( navblk[4] );     /* Kx                      */
        igdtmpl[8] = G_NINT( navblk[5] );     /* Kx                      */
        igdtmpl[9] = G_NINT(navblk[6]*1000000.0);    
                                              /* Lat of 1st grid point   */
        rlon = navblk[7];   
        if ( rlon < 0.0 ) rlon += 360.0;
        igdtmpl[10] = G_NINT(rlon*1000000.0);   
                                              /* Lon of 1st grid point   */
        igdtmpl[11] = 8;                      /* Res and Comp flags      */

        igdtmpl[12] = G_NINT(trult1*1000000.0);
                                              /* LaD                     */
        rlov = navblk[11];   
        if ( rlov < 0.0 ) rlov += 360.0;
        igdtmpl[13] = G_NINT(rlov*1000000.0);
                                              /* LoV                     */
        igdtmpl[14] = G_NINT( dx*1000.0 );    /* Dx                      */
        igdtmpl[15] = G_NINT( dy*1000.0 );    /* Dy                      */
        if ( sign < 0.0 ) igdtmpl[16] = 0;
        else igdtmpl[16] = 128;               /* Projection center flag  */
        igdtmpl[17] = 64;                     /* Scanning mode           */
        igdtmpl[18] = G_NINT( trult1*1000000.0);
                                              /* Lat intrsect 1          */
        igdtmpl[19] = G_NINT( trult2*1000000.0);
                                              /* Lat intrsect 2          */
        igdtmpl[20] = -90000000;              /* Lat of South Pole       */
        igdtmpl[21] = 0;                      /* Lon of South Pole       */

}
