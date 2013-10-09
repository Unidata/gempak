#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_wprt ( FILE *fp, shp_part *oneprt, int *iret )
/************************************************************************
 * shp_wprt 								*
 *									*
 * This function writes one part of data in GEMPAK Standard Sequential	*
 * Format.								*
 *									*
 * shp_wprt ( fp, oneprt, iret )					*
 *									*
 * Input parameters:							*
 *	*fp		FILE		Output file pointer		*
 *	*oneprt		shp_part	One part of data		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = Normal			*
 **									*
 * R. Tian/SAIC		 3/04		Initial coding			*
 ***********************************************************************/
{
    char sp14[15] = "              ";
    float maxlon, minlon, maxlat, minlat;
    float *xpts, *ypts;
    int	npts, ipts;
/*---------------------------------------------------------------------*/
    *iret = 0;

    npts = oneprt->numpts;
    xpts = oneprt->ptx;
    ypts = oneprt->pty;

    /*
     * Compute bounds.
     */
    maxlat = -90.0F;
    minlat = 90.0F;
    maxlon = -180.0F;
    minlon = 180.0F;
    for ( ipts = 0; ipts < npts; ipts++ ) {
        maxlon = xpts[ipts] > maxlon ? xpts[ipts] : maxlon;
        minlon = xpts[ipts] < minlon ? xpts[ipts] : minlon;
        maxlat = ypts[ipts] > maxlat ? ypts[ipts] : maxlat;
        minlat = ypts[ipts] < minlat ? ypts[ipts] : minlat;
    }

    /*
     * Write part.
     */
    fprintf ( fp, "%4d%14.14s", npts*2, sp14 );
    fprintf ( fp, "%9.3f%9.3f%9.3f%9.3f",
		 maxlat, minlat, maxlon, minlon );
    for ( ipts = 0; ipts < npts; ipts++ ) {
        fprintf ( fp, "%9.3f%9.3f", ypts[ipts], xpts[ipts] );
        if ( (ipts % 4) == 0 ) fprintf ( fp, "\n" );
    }
    if ( ((ipts+3) % 4) != 0 ) fprintf ( fp, "\n" );
}
