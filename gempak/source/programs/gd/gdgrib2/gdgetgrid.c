#include "gdgrib2.h"

#define TMPLEN 80

void gdgetgrid ( GDG2_input *input, GDG2_gemgrid *gemgrid, int *iret )
/************************************************************************
 * gdgetgrid                                                            *
 *                                                                      *
 * This routine gets a gempak grid based on user input variables GFUNC, *
 * GDFILE, GDATTIM, GLEVEL, and GVCORD.                                 *
 *                                                                      *
 *  Usage:                                                              *
 *      gdgetgrid( input, gemgrid, iret );                              *
 *                                                                      *
 *  Input Arguments:                                                    *
 *      *input            GDG2_input       input structure for GDGRIB2  *
 *                                                                      *
 *  Output Arguments:                                                   *
 *      *gemgrid          GDG2_gemgrid     output grid and Gempak info  *
 *      *iret              int              Error return code.          *
 *                                           0 = Normal                 *
 *                                          -4 = Could not open GDFILE  *
 *                                         -20 = could not process      *
 *                                               GDATTIM                *
 *                                         -21 = could not get next time*
 *                                         -22 = could not get requested*
 *                                               grid                   *
 *                                         -23 = could not allocate     *
 *                                               space for output grid  *
 *                                         -32 = could not set output   *
 *                                               grid navigation        *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP           6/2005   Orig                              *
 ***********************************************************************/
{

    int    ret, ier, jj;
    int    chngnv, coladd, nxtok;
    int    igx, igy, olevel[2], ovcord, npts;
    char   time[2][20], otime[2][20], dgerrstr[TMPLEN], parm[13];
    char   dattim[TMPLEN], *gdoutf="";
    float  *grid, navblk[LLNNAV];

    *iret=0;
    gemgrid->grid=0;

    /*
     *  Open input GEMPAK file
     */
    dgc_nfil( input->gdfile, gdoutf, &ret );
    if ( ret != 0 ) {
        er_wmsg("DG",&ret," ",&ier,2,1);
        *iret=-4;
        return;
    }

    /*
     *  Create internal list of times to process from GDATTIM
     */
    dgc_ndtm( input->gdattim, &ret );
    if ( ret != 0 ) {
        er_wmsg("DG",&ret," ",&ier,2,1);
        *iret=-20;
        return;
    }

    /*
     *  Get Gempak time of next field.
     */
    chngnv=1;
    coladd=0;
    dgc_ntim( &chngnv, &coladd, time[0], time[1], &nxtok, &ret );
    if ( ret != 0 ) {
        er_wmsg("DG",&ret," ",&ier,2,1);
        *iret=-21;
        return;
    }

    /*
     *  Convert time(2) array to dual grid time stamp ?????
     */
    if ( time[1][0] == '\0' ) {
	strcpy(dattim,time[0]);
    }
    else {
	sprintf(dattim, "%s:%s", time[0], time[1]);
    }
 
    /*
     *  Set output Grid navigation
     */
    gdsetnav ( input, navblk, &ret );
    if ( ret != 0 ) {
        *iret=-32;
        return;
    }
 
    /*
     *  Allocate space for grid
     */
    npts = navblk[4]*navblk[5];
    grid = (float *)calloc( npts, sizeof(float) );
    if ( grid == 0 ) {
        *iret=-23;
        return;
    }
 
    /*
     *  Get Grid
     */
    dgerrstr[0] = '\0';
    dgc_grid( dattim , input->glevel, input->gvcord, input->gfunc,
             dgerrstr, grid, &igx, &igy, otime[0], otime[1],
	     &olevel[0], &olevel[1], &ovcord, parm, &ret );
    if ( ret != 0 ) {
        er_wmsg("DG",&ret,dgerrstr,&ier,2,strlen(dgerrstr));
        *iret=-22;
        if ( grid != 0 ) free(grid);
        return;
    }

    /*
     *  Set returned values.
     */

    gemgrid->kx = igx;
    gemgrid->ky = igy;
    gemgrid->level[0] = olevel[0];
    gemgrid->level[1] = olevel[1];
    gemgrid->vcord = ovcord;
    gemgrid->grid=grid;
    strcpy( gemgrid->param, parm );
    strcpy( gemgrid->ctime[0], otime[0] );
    strcpy( gemgrid->ctime[1], otime[1] );
    for (jj=0 ; jj<LLNNAV ; jj++) gemgrid->navblk[jj] = navblk[jj];
 
}
