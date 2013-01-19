#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"
#include "gpc.h"
#include "proto_gpc.h"

/************************************************************************
 * cloclip.c                                                            *
 *                                                                      *
 * This module contains the subroutines to perform clipping of an       *
 * arbitrary polygon against a specified bounds area.			*
 *                                                                      *
 * CONTENTS:                                                            *
 *   global functions:                                                  *
 *      clo_clip		- perform clipping against a bounds area*
 *      clo_clipget		- get points of a resulting polygon     *
 *      clo_clipdone		- release previously allocated memory   *
 *                                                                      *
 *   private functions:							*
 *	clo_clipbnds 		- get the bounds area polygon		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           9/04   Created                                 *
 ***********************************************************************/

/*
 * Private functions
 */
    static void clo_clipbnds ( char *, char *, gpc_polygon *, int * );

/*
 * Global variables
 */
    static gpc_polygon     	gpc_poly_results; 	/* resulting polygon 		*/
    static char			sysin [ 10 ];		/* incoming coordinate system 	*/


void clo_clip ( int *np, float *xp, float *yp, char *sysp, char *bounds,
		char *name, int *nclip, int *maxpts, int *iret )
/************************************************************************
 * clo_clip	                                                        *
 *                                                                      *
 * This routine performs clipping of a polygon against a specified      *
 * bounds area.                                                         *
 *                                                                      *
 * void clo_clip ( np, xp, yp, sysp, bounds, name, nclip, 		*
 *		   maxpts, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      *np		int             Number of polygon points        *
 *      *xp		float		X array of polygon points	*
 *      *yp		float		Y array of polygon points       *
 *	*sysp		char		Polygon points coordinate system*
 *	*bounds		char		Bounds name			*
 *	*name		char		Clipping area tag 		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *nclip		int             Number of resulting contours    *
 *      *maxpts		int             Maximum number of points for any*
 *					contour in the resulting polygon*
 *      *iret           int             Return code                     *
 *                                      =  0: normal                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           9/04   	Created                         *
 ***********************************************************************/
{
    int     		ier, ii;
    float   		*xnormal, *ynormal;
    gpc_polygon     	gpc_poly, gpc_poly_bnds, gpc_poly_tmp; 
    gpc_vertex_list 	verts;
/*---------------------------------------------------------------------*/

    strcpy ( sysin, sysp ); 

    /*
     * Convert incoming coordinate system to sys_N (normalized).
     */
    G_MALLOC ( xnormal, float, *np, "CLO_CLIP: xnormal" );
    G_MALLOC ( ynormal, float, *np, "CLO_CLIP: ynormal" );

    gtrans ( sysp, sys_N, np, xp, yp, xnormal, ynormal,
             &ier, strlen(sysp), strlen(sys_N) );

    /*
     * Fill GPC polygon structure with incoming points.
     */
    gpc_poly.num_contours = 0;
    gpc_poly.hole         = (int*)NULL;
    gpc_poly.contour      = (gpc_vertex_list*)NULL;

    verts.vertex          = (gpc_vertex*)NULL;
    verts.num_vertices    = 0;

    gpc_cvlist ( *np, xnormal, ynormal, &verts, &ier );
    gpc_add_contour ( &gpc_poly, &verts, G_FALSE );

    /*
     * Create a GPC polygon for the bounds.
     */
    clo_clipbnds ( bounds, name, &gpc_poly_bnds, &ier );

    /*
     * Clip
     */
    gpc_polygon_clip ( GPC_INT, &gpc_poly, &gpc_poly_bnds, &gpc_poly_tmp );

    /*
     * Remove holes. Holes are ignored in this release.
     */
    gpc_poly_results.num_contours = 0;
    gpc_poly_results.hole         = (int*)NULL;
    gpc_poly_results.contour      = (gpc_vertex_list*)NULL;

    for ( ii = 0; ii < gpc_poly_tmp.num_contours; ii++ ) {
	if ( gpc_poly_tmp.hole [ ii ] == G_FALSE ) {
	   gpc_add_contour ( &gpc_poly_results, &gpc_poly_tmp.contour [ ii ], G_FALSE );
	}
    }

    /*
     * Get the number of contours in the resulting polygon
     * and the maximum number of pointers of any contour
     */
    *nclip  = gpc_poly_results.num_contours;
    *maxpts = 0;

    for ( ii = 0; ii < *nclip; ii++ ) {
        *maxpts = MAX ( *maxpts, gpc_poly_results.contour [ ii ].num_vertices );
    }

    /*
     * Release memory
     */
    gpc_free_polygon ( &gpc_poly_bnds );
    gpc_free_polygon ( &gpc_poly_tmp );
    gpc_free_polygon ( &gpc_poly );

    free ( verts.vertex );

    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );

    *iret = 0;

}

void clo_clipget ( int *which, int *npts, float *xo, float *yo, int *iret )
/************************************************************************
 * clo_clipget	                                                        *
 *                                                                      *
 * This routine obtains x and y coordinates of points of a contour.	*
 *                                                                      *
 * void clo_clipget ( which, npts, xo, yo, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *which		int             Which contour to retrieve	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *npts		int             Number of the contour points	*
 *      *xo		float		X array of contour points	*
 *      *yo		float		Y array of contour points       *
 *      *iret           int             Return code                     *
 *                                      =  0: normal                    *
 *                                      = -1: no such contour 	        *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           9/04   	Created                         *
 ***********************************************************************/
{
    int		ii, ier;
    float	*xnormal, *ynormal; 
/*---------------------------------------------------------------------*/

    if ( *which >= gpc_poly_results.num_contours || *which < 0 ) {
       *iret = -1;
       return;
    }

    /*
     * Allocated memory for points in sys_N coordinates.
     */ 
    *npts  = gpc_poly_results.contour [ *which ].num_vertices;

    G_MALLOC ( xnormal, float, *npts, "CLO_CLIPGET: xnormal" );
    G_MALLOC ( ynormal, float, *npts, "CLO_CLIPGET: ynormal" );

    /*
     * Get (x, y) of the contour
     */
    for ( ii = 0; ii < *npts; ii++ ) {
	xnormal [ ii ] = gpc_poly_results.contour [ *which ].vertex [ ii ].x;
	ynormal [ ii ] = gpc_poly_results.contour [ *which ].vertex [ ii ].y;
    }

    /*
     * Convert sys_N to the incoming coordinate system 
     */
    gtrans ( sys_N, sysin, npts, xnormal, ynormal, xo, yo,
             &ier, strlen(sys_N), strlen(sysin) );

    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );

    *iret = 0;
    
}

void clo_clipdone ( int *iret )
/************************************************************************
 * clo_clipdone	                                                        *
 *                                                                      *
 * This routine releases previous allocated memory in this module	*
 *                                                                      *
 * void clo_clipdone ( iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      None								*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                      =  0: normal                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           9/04   	Created                         *
 ***********************************************************************/
{
    gpc_free_polygon ( &gpc_poly_results );

    *iret = 0;
}

static void clo_clipbnds ( char *bounds, char *name, gpc_polygon *polygon, 
		    int *iret )
/************************************************************************
 * clo_clipbnds		                                                *
 *                                                                      *
 * This routine puts all bound parts into a polygon.			*
 *                                                                      *
 * void clo_clipbnds ( bounds, name, polygon, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*bounds		char		Bounds name			*
 *	*name		char		Clipping area tag		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*polygon	gpc_polygon	Resulting polygon		*
 *      *iret           int             Return code                     *
 *                                      =  0: normal                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           9/04   	Created                         *
 ***********************************************************************/
{
    int			minp, maxp, next, npts, ier;
    float		*xlat, *ylon, *xnormal, *ynormal;
    float		filter;
    gpc_vertex_list 	verts;
/*---------------------------------------------------------------------*/

    /*
     * Set bounds type and tag
     */
    clo_bstype ( bounds, &ier );
    clo_bstag  ( name, &ier );

    /*
     * Initialization
     */
    minp   = 0;
    maxp   = LLMXPT;
    filter = 0.0F;
    next   = 0;

    G_MALLOC ( xlat, float, maxp, "CLO_CLIPBNDS: xlat" );
    G_MALLOC ( ylon, float, maxp, "CLO_CLIPBNDS: ylon" );

    polygon->num_contours = 0;
    polygon->hole         = (int*)NULL;
    polygon->contour      = (gpc_vertex_list*)NULL;

    /*
     * Read each bounds part and put it in a gpc polygon
     */  
    clo_bgnext ( &minp, &maxp, &filter, &npts, xlat, ylon, &next );
 
    while ( next == 0 ) {
        /*
         * Convert bounds coordinate system to sys_N
         */
        G_MALLOC ( xnormal, float, npts, "CLO_CLIPBNDS: xnormal" );
        G_MALLOC ( ynormal, float, npts, "CLO_CLIPBNDS: ynormal" );

        gtrans ( sys_M, sys_N, &npts, xlat, ylon, xnormal, ynormal,
                 &ier, strlen(sys_M), strlen(sys_N) );

        /*
         * Create GPC polygon structures for bounds.
         */
        verts.vertex          = (gpc_vertex*)NULL;
        verts.num_vertices    = 0;

        gpc_cvlist ( npts, xnormal, ynormal, &verts, &ier );

	/*
	 * Holes are not considered 
	 */
        gpc_add_contour ( polygon, &verts, G_FALSE );

	/*
	 * Free memory
	 */
        free ( verts.vertex );
        G_FREE ( xnormal, float );
        G_FREE ( ynormal, float );

        clo_bgnext ( &minp, &maxp, &filter, &npts, xlat, ylon, &next );
    }
  
    G_FREE ( xlat, float );
    G_FREE ( ylon, float );

    *iret = 0;

}
