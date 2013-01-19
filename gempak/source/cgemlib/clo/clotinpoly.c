#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

#ifndef FLT_MAX
#define FLT_MAX 1E+37
#endif

void clo_tinpoly ( char *name, char *sysin, int npoly, float *x, 
						float *y, int *iret )
/************************************************************************
 * clo_tinpoly								*
 *									*
 * This function extracts an array of integers which are		*
 * offsets into a location structure.					*
 *									*
 * clo_tinpoly ( name, sysin, npoly, x, y, iret )			*
 *									*
 * Input parameters:							*
 *	*name 		char	Location name				*
 *	*sysin		char	Polygon coordinate system		*
 *	npoly		int	Number of points in the polygon		*
 *	*x		float	x-coordinates of the polygon		*
 *	*y		float	y-coordinates of the polygon		*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/99	From clo_ancinpoly			*
 * D.W.Plummer/NCEP	 4/99	Remove unnecessary structure references	*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 ***********************************************************************/
{
int		i, nin, nll, ier; 

float		lat1, lon1, lat2, lon2;
int		*inout;
float		*lat, *lon;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Transform input polygon coords to map coords
     */
    inout = (int *) malloc ( MAXHOT * sizeof(int) );
    lat = (float *) malloc ( MAXHOT * sizeof(float) );
    lon = (float *) malloc ( MAXHOT * sizeof(float) );

    gtrans( sysin, sys_M, &npoly, x, y, lat, lon, &ier,
		strlen(sysin), strlen(sys_M) );

    /*
     *  Compute minimum and maximum map bounds
     */
    lat1 =  FLT_MAX;	lon1 =  FLT_MAX;
    lat2 = -FLT_MAX;	lon2 = -FLT_MAX;
    for ( i = 0; i < npoly; i++ )  {
	lat1 = G_MIN( lat1, lat[i] ); lat2 = G_MAX( lat2, lat[i] );
	lon1 = G_MIN( lon1, lon[i] ); lon2 = G_MAX( lon2, lon[i] );
    }
    lat1 = G_MAX( -90.0F, lat1 );
    lat2 = G_MIN(  90.0F, lat2 );

/*
        This section was to more accurately calculate the minimum and
        maximum M coordinate bounds.  However, udmbnd is actually a
        GPLT routine and cannot be used at the APPL level.
        So, until a proper high-level routine can be developed,
        simply add a few degrees to the polygon M coordinate limits.
    np = 1;
    gtrans( sys_M, sys_L, &np, &lat1, &lon1, &xl, &yb, &iret,
		strlen(sys_M), strlen(sys_L) );
    gtrans( sys_M, sys_L, &np, &lat2, &lon2, &xr, &yt, &iret,
		strlen(sys_M), strlen(sys_L) );
    udmbnd( &xl, &yb, &xr, &yt, &lat1, &lon1, &lat2, &lon2, &iret );
*/
    lat1 = lat1 - 3.0F;          lon1 = lon1 - 3.0F;
    lat2 = lat2 + 3.0F;          lon2 = lon2 + 3.0F;

    /*
     *  Gather a hotlist within the lat-lon bounds
     */
    clo_tinltln ( name, lat1, lat2, lon1, lon2, &ier );
	    
    /*
     *  Get latitudes and longitudes of hotlist
     */
    clo_tgltln ( name, MAXHOT, &nll, lat, lon, &ier );

    /*
     *  Determine which ones are actually inside the polygon
     */
    cgr_inpoly( sys_M, &nll, lat, lon, sysin, &npoly, 
		x, y, inout, &ier );

    /*
     *  Eliminate those which are outside the polygon
     */
    nin = 0;
    for ( i = 0; i < nll; i++ )  {
	if ( inout[i] == 1 )  {
	    hotlist[nin] = hotlist[i];
	    nin++;
	}
    }
    nhot= nin;

    free ( lon );
    free ( lat );
    free ( inout );

}
