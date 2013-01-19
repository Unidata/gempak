#include "na.h"

void na_ggds ( const float *gdsarr, char *cprj, int *kx, int *ky,
               float *grdout, float *rnvblk, int *iret )
/************************************************************************
 * na_ggds								*
 *									*
 * This subroutine takes the GDS information and constructs a 		*
 * navigation block for a GEMPAK grid file.				*
 *									*
 * na_ggds ( gdsarr, cprj, kx, ky, grdout, rnvblk, iret )		*
 *									*
 * Input parameters:							*
 *	*gdsarr		const float	GRIB GDS projection info	*
 *									*
 * Output parameters:							*
 *	*cprj		char		Grid projection			*
 *	*kx		int		Number of points in x dir	*
 *	*ky		int		Number of points in y dir	*
 *	*grdout		float		Grid corners			*
 *	*rnvblk		float		Grid navigation block		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -4 = invalid navigation	*
 **									*
 * Log:									*
 * S. Jacobs/EAI	 7/93						*
 * R. Tian/SAIC		 7/06	Recoded from Fortran			*
 * S. Gilbert/NCEP	10/06	Added call to GR_VNAV                   *
 ************************************************************************/
{
    float angle1, angle2, angle3;
    int angflg, ier, valid;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Set the projection.
     */
    if ( G_NINT ( gdsarr[0] ) == 0 ) {
	strcpy ( cprj, "CED" );
    } else if ( G_NINT ( gdsarr[0] ) == 1 ) {
	strcpy ( cprj, "MER" );
    } else if ( G_NINT ( gdsarr[0] ) == 3 ) {
	strcpy ( cprj, "LCC" );
    } else if ( G_NINT ( gdsarr[0] ) == 5 ) {
	strcpy ( cprj, "STR" );
    } else {
	*iret = -4;
	return;
    }

    /*
     * Set the number of columns and rows.
     */
    *kx = G_NINT ( gdsarr[1] );
    *ky = G_NINT ( gdsarr[2] );

    /*
     * Set the lat/lon values of the corners.
     */
    grdout[0] = gdsarr[3];
    grdout[1] = gdsarr[4];
    grdout[2] = gdsarr[5];
    grdout[3] = gdsarr[6];

    /*
     * Set the projection angles.
     */
    angle1 = gdsarr[7];
    angle2 = gdsarr[8];
    angle3 = gdsarr[9];
    angflg = G_TRUE;

    /*
     * Fill navigation block.
     */
    gr_vnav  ( cprj, kx, ky, &grdout[0], &grdout[1], &grdout[2], &grdout[3],
               &angle1, &angle2, &angle3, &angflg, &valid, &ier, strlen(cprj) );

    if ( ier == 0 )
       grc_mnav  ( cprj, kx, ky, &grdout[0], &grdout[1], &grdout[2], &grdout[3],
                   &angle1, &angle2, &angle3, &angflg, rnvblk, &ier );
    else
       *iret = -2;

    return;
}
