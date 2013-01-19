#include "geminc.h"
#include "gemprm.h"
#include "gpc.h"
#include "proto_gpc.h"

void cgr_polydiff ( int *npin0, float *xin0, float *yin0,
	            int *npin1, float *xin1, float *yin1, int *maxnpo,
		    int *npo, float *xo, float *yo, int *iret )

/************************************************************************
 * cgr_polydiff 							*
 *									*
 * This function computes the difference of two polygons using the      *
 * difference clipping option in the GPC (General Polygon Clipper)      *
 * library.  The area that is in polygon 0 but not in polygon 1 is 	*
 * returned.								*
 *									*
 * No coordinate system is assumed, however normalized coordinates	*
 * (sys_N) are recommended. The first and last points may or may not 	*
 * be equal.								*
 *									*
 * The incoming polygon's points are not assumed to be ordered in any	*
 * particular way.							*
 *									*
 * The incoming polygons are not assumed to be regular, i.e., they may	*
 * cross themselves. However, no interior voids may be specified.	*
 *									*
 * If the maximum number of output points is exceeded, the returned	*
 * number of points 'npo' is set to zero w/ the return code set to -1.	*
 * 									*
 * If there is no area in polygon 0 that is not in polygon 1, then npo  *
 * will be zero with a return code of 1.				*
 *									*
 * cgr_polydiff ( npin0, xin0, yin0, npin1, xin1, yin1, maxnpo,		*
 * 		  npo, xo, yo, iret )					*
 *									*
 * Input parameters:							*
 * *npin0    	    int     Number of points in polygon 0               *
 * *xin0     	    float   X coordinates of polygon 0                  *
 * *yin0     	    float   Y coordinates of polygon 0                  *
 * *npin1    	    int     Number of points in polygon 1               *
 * *xin1     	    float   X coordinates of polygon 1                  *
 * *yin1     	    float   Y coordinates of polygon 1                  *
 * *maxnpo   	    int     Maximum number of output points		*
 *									*
 * Output parameters:							*
 * *npo      	    int     Number of points in output polygon          *
 * *xo       	    float   X coordinates of output polygon             *
 * *yo       	    float   Y coordinates of output polygon             *
 * *iret     	    int     Return code					*
 *			    =  1 - no area in 0 that isn't in 1		*
 * 			    = -1 - insufficient maxnpo			*
 * 			    = -2 - polygons have insufficient # of pts	*
 *									*
 **									*
 * Log:									*
 * E. Safford/SAIC 	12/04	initial coding.				*
 ***********************************************************************/
{
int		ii, hole, ier=0;
gpc_polygon     gpc_poly_0, gpc_poly_1, gpc_diff_poly;
gpc_vertex_list	verts;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    *npo  = 0;
    hole  = 0;

    if ( *npin0 <= 2 || *npin1 <= 2 )  {
	*iret = -2;
	return;
    }

    gpc_poly_0.num_contours = 0;
    gpc_poly_0.hole         = (int*)NULL;
    gpc_poly_0.contour      = (gpc_vertex_list*)NULL;

    gpc_poly_1.num_contours = 0;
    gpc_poly_1.hole         = (int*)NULL;
    gpc_poly_1.contour      = (gpc_vertex_list*)NULL;

    verts.vertex	    = (gpc_vertex*)NULL;
    verts.num_vertices	    = 0;

    gpc_diff_poly.hole      = (int*)NULL;
    gpc_diff_poly.contour   = (gpc_vertex_list*)NULL;

    gpc_cvlist ( *npin0, xin0, yin0, &verts, &ier );
    gpc_poly_0.num_contours = 0;
    gpc_add_contour ( &gpc_poly_0, &verts, hole );
    free ( verts.vertex );

    gpc_cvlist ( *npin1, xin1, yin1, &verts, &ier );
    gpc_poly_1.num_contours = 0;
    gpc_add_contour ( &gpc_poly_1, &verts, hole );
    free ( verts.vertex );

    gpc_polygon_clip ( GPC_DIFF, &gpc_poly_0, &gpc_poly_1, &gpc_diff_poly );


    if ( gpc_diff_poly.num_contours <= 0 ) {
	*iret = 1;
	*npo  = 0;
    }
    else {
        for ( ii = 0; ii < gpc_diff_poly.num_contours; ii++ )  {
	    if ( gpc_diff_poly.hole[ii] == 0 )  {
                if ( gpc_diff_poly.contour->num_vertices <= *maxnpo )  {
                    gpc_gvlist ( gpc_diff_poly.contour, npo, xo, yo, &ier );
		    break;
                }
                else  {
	            *iret = -1;
	            *npo  = 0;
                }
	    }
        }
    }
    

    gpc_free_polygon ( &gpc_poly_0 );
    gpc_free_polygon ( &gpc_poly_1 );
    gpc_free_polygon ( &gpc_diff_poly );

    return;

}
