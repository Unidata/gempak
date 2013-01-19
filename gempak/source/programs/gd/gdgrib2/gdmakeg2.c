#include "gdgrib2.h"
#include "gb2def.h"

#define MAXTMPL 200

void gdmakeg2 ( GDG2_input *input, GDG2_gemgrid *gemgrid, 
                unsigned char **g2msg, int *g2len, int *iret )
/************************************************************************
 * gdmakeg2                                                             *
 *                                                                      *
 * This routine creates a GRIB2 message from a GEMPAK grid.             *
 * GDFILE, GDATTIM, GLEVEL, and GVCORD.                                 *
 *                                                                      *
 * Memory is allocated for the output GRIB2 message by this routine.    *
 * It is the users responsibility to free the space when it is no       *
 * longer needed by calling the system routine, "free".                 *
 *                                                                      *
 *  Usage:                                                              *
 *      gdmakeg2( input, gemgrid, g2msg, g2len, iret );                 *
 *                                                                      *
 *  Input Arguments:                                                    *
 *      *input            GDG2_input       input structure for GDGRIB2  *
 *      *gemgrid          GDG2_gemgrid     output grid and Gempak info  *
 *                                                                      *
 *  Output Arguments:                                                   *
 *      **g2msg            unsigned char    Packed GRIB2 message        *
 *      *g2len             int             Length of packed GRIB2 msg   *
 *      *iret              int              Error return code.          *
 *                                           0 = Normal                 *
 *                                           -27 = Error encoding       *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP           6/2005   Orig                              *
 * S. Gilbert/NCEP           3/2006   Fixed allocation of bmap          *
 ***********************************************************************/
{

    int    ret, discipline, num, j;
    int    kx, ky, msize, ngrdpts;
    int    itmp2[2];
    int    igdtnum;
    int    numcoord=0;
    float  rmiss;
    char   proj[4];
    g2int  ibmap, *bmap, *ideflist=0, idefnum=0, idrtmpl[MAXTMPL], idrtnum, igds[5], 
		igdtmpl[MAXTMPL], ipdtnum, ipdtmpl[MAXTMPL], lsec0[2], lsec1[13]; 
    g2float *coordlist=0;

/*-----------------------------------------------------------------------*/

    *iret = 0;
    *g2msg = 0;

    gr_rnav( gemgrid->navblk, proj, &kx, &ky, &ret, 4 );

    /*
     *  Set Section 1 - Identification Section
     */
    gdsetsect1( input, gemgrid, (int*)lsec1, &ret );

    /*
     *  Set Grid Definition Template for Section 3
     */
    gb2_navgdt( gemgrid->navblk, &igdtnum, (int*)igdtmpl, &ret );
    igds[0] =  0;
    igds[1] = kx * ky;
    igds[2] = 0;
    igds[3] = 0;
    igds[4] = igdtnum;

    /*
     *  Set Product Definition Template for Section 4
     */
    gdsetsect4( input, gemgrid, (int*)lsec1, &discipline, (int*)&ipdtnum,
                (int*)ipdtmpl, &ret);
    
    /*
     *  Set Section 0 - Indicator Section
     */
    cst_ilst( input->g2is, ';', 255, 2, itmp2, &num, &ret);
    if ( itmp2[0] != 255 ) lsec0[0] = itmp2[0];
    else lsec0[0] = discipline;
    lsec0[1] = 2;                      /*  GRIB version   */

    /*
     *  Set Data Representation Template for Sections 5 and 7
     */
    gdsetsect5( input, (int*)&idrtnum, (int*)idrtmpl, &ret );

    /*
     *  Set Bitmap, if appropriate
     */
    bmap = 0;
    ibmap = 255;
    /*
     *  Check for missing data in grid
     */
    ngrdpts = kx * ky;
    for ( j=0; j<ngrdpts; j++ ) {
        if ( ERMISS(gemgrid->grid[j]) ) {
            ibmap = 0;
            break;
        }
    }
    if ( ibmap == 0 ) {
        /*
         *   Missing data exists
         */
        if ( (idrtnum == 2 || idrtnum == 3) && idrtmpl[6] == 1 ) {
            /*
             *  Use missing value management instead of bitmap
             */
            ibmap = 255;
            rmiss = RMISSD;
            mkieee( &rmiss, idrtmpl+7, 1);
        }
        else {
            /*
             *  Set up bitmap.
             */
            bmap = (g2int *)calloc( ngrdpts, sizeof(g2int) );
            for ( j=0; j<ngrdpts; j++ ) {
                if ( !ERMISS(gemgrid->grid[j]) ) bmap[j] = 1;
            }
        }
    }

    /*
     *  Allocate space for new GRIB2 message
     */
    msize = 4 * kx * ky;
    *g2msg = (unsigned char *)malloc(msize);
    if ( *g2msg == 0 ) {
        *iret=-26;
        *g2len=0;
    }

    /*
     *  Encode GRIB2 message
     */
    *g2len = g2_create (*g2msg, lsec0, lsec1 );
    *g2len = g2_addgrid (*g2msg, igds, igdtmpl, ideflist, idefnum);
    *g2len = g2_addfield (*g2msg, ipdtnum, ipdtmpl, coordlist, numcoord, 
                        idrtnum, idrtmpl, gemgrid->grid, igds[1],
                        ibmap, bmap );
    *g2len = g2_gribend ( *g2msg );

    if ( bmap != 0 ) free(bmap);

    if (*g2len <= 0 ) {
        *iret = -27;
        *g2len = 0;
        free ( *g2msg );
        *g2msg = 0;
    }

}
