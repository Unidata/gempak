#include "geminc.h"
#include "gemprm.h"
#include "gpc.h"
#include "vgstruct.h"

static gpc_polygon	_poly_out;

void poly_clipw ( int *np1, float *lat1, float *lon1, int *np2, 
		  float *lat2, float *lon2, int *iret );

void poly_get_contour ( int *nc, int *iret );

void poly_get_vertex (  int *nth, int *np, float *clat, float *clon, 
			int *iret );

/*
 *  Public functions
 */
void poly_clipw ( int *np1, float *lat1, float *lon1, int *np2, 
		  float *lat2, float *lon2, int *iret )
/************************************************************************
 * cgr_clipw								*
 *                                                                      *
 * This is a wrapper function to get the intersection of two polygons.	*
 *                                                                      *
 * poly_clipw ( np1, lat1, lon1, np2, lat2, lon2, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *  *np1    	int     number of points on polygon one (p1)		*
 *  lat1[np1]	float	vertices latitudes on p1			*
 *  lon1[np1]	float	vertices longitudes on p1			*
 *  *np2    	int     number of points on polygon two (p2)		*
 *  lat2[np2]	float	vertices latitudes on p2			*
 *  lon2[np2]	float	vertices longitudes on p2			*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret    	int    	return code					*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/SAIC		05/08	initial coding				*
 ***********************************************************************/
{   
    int                 hole = 0, ier = 0;
    float               *xn1, *yn1, *xn2, *yn2;
    gpc_vertex_list     verts1, verts2;
    gpc_polygon		poly1, poly2;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Convert to normalized coordinate first.
     */
    G_MALLOC ( xn1, float, *np1, "x1 normal" );
    G_MALLOC ( yn1, float, *np1, "y1 normal" );

    gtrans ( sys_M, sys_N, np1, lat1, lon1, xn1, yn1, &ier, 
	     strlen(sys_M), strlen(sys_N) );

    /*
     * Fill 1st polygon structure with points.
     */
    poly1.num_contours = 0;
    poly1.hole         = (int*)NULL;
    poly1.contour      = (gpc_vertex_list*)NULL;
    verts1.vertex       = (gpc_vertex*)NULL;
    verts1.num_vertices = 0;
    gpc_cvlist ( *np1, xn1, yn1, &verts1, &ier );
    gpc_add_contour ( &poly1, &verts1, hole );

    free ( verts1.vertex );
    G_FREE ( xn1, float );
    G_FREE ( yn1, float );
   /*
    * Convert to normalized coordinate.
    * 2nd pologon (bound files).
    */

    G_MALLOC ( xn2, float, *np2, "x2 normal" );
    G_MALLOC ( yn2, float, *np2, "y2 normal" );

    gtrans ( sys_M, sys_N, np2, lat2, lon2, xn2, yn2, &ier, 
	     strlen(sys_M), strlen(sys_N) );

    /*
     * Fill 2nd polygon structure with points.
     */
    poly2.num_contours  = 0;
    poly2.hole          = (int*)NULL;
    poly2.contour       = (gpc_vertex_list*)NULL;
    verts2.vertex       = (gpc_vertex*)NULL;
    verts2.num_vertices = 0;
    gpc_cvlist ( *np2, xn2, yn2, &verts2, &ier );
    gpc_add_contour ( &poly2, &verts2, hole );

    free ( verts2.vertex );
    G_FREE ( xn2, float );
    G_FREE ( yn2, float );

    gpc_polygon_clip ( GPC_INT, &poly1, &poly2, &_poly_out );
}

/*=====================================================================*/

void poly_get_contour ( int *nc, int *iret )
/************************************************************************
 * poly_get_contour							*
 *                                                                      *
 * This function returns number of contours after clipping.		*
 *                                                                      *
 * poly_contour ( *nc, *iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:							*
 *  *nc    	int     number of contours				*
 *  *iret	int	return code					*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		05/08	initial coding				*
 ***********************************************************************/
 {
    *iret = 0;
    *nc = _poly_out.num_contours;
 }

/*=====================================================================*/

void poly_get_vertex ( int *nth, int *np, float *clat, float *clon, 
  		       int *iret )
/************************************************************************
 * poly_get_vertex							*
 *									*
 * This function returns vertices on the nth contour.			*
 *									*
 * poly_get_vertex ( *nth, *np, *clat, *clon, int *iret )		*
 *									*
 * Input parameters:                                                    *
 *  *nth	int	nth contour					*
 *									*
 * Output parameters:							*
 *  *np    	int     number of points on the intersect polygon	*
 *  *clat[np]	float	clipped latitudes 				*
 *  *clon[np]	float	clipped longitudes				*
 *  *iret    	int    	return code					*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		05/08	initial coding				*
 ***********************************************************************/
{   
    int		ii, ier, nc;
    float	*xn, *yn;
/*---------------------------------------------------------------------*/
    *iret = 0;

    nc = *nth - 1;

    if ( _poly_out.num_contours > 0 ) {
        *np = _poly_out.contour[nc].num_vertices;
    	G_MALLOC ( xn, float, *np, "xn normal" );
    	G_MALLOC ( yn, float, *np, "xn normal" );

	for ( ii = 0; ii < *np; ii++ ) {
	    xn [ii] = _poly_out.contour[nc].vertex[ii].x;
	    yn [ii] = _poly_out.contour[nc].vertex[ii].y;
	}
        gtrans ( sys_N, sys_M, np, xn, yn, clat, clon, &ier, 
	         strlen(sys_M), strlen(sys_N) );
    	G_FREE ( xn, float );
    	G_FREE ( yn, float );
    }
    else {
	*np = 0;
    }
}
