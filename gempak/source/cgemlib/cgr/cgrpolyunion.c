#include "geminc.h"
#include "gemprm.h"
#include "gpc.h"
#include "proto_gpc.h"

void cgr_polyunion ( int *npin0, float *xin0, float *yin0,
	             int *npin1, float *xin1, float *yin1, int *maxnpo,
		     int *npo, float *xo, float *yo, int *iret )

/************************************************************************
 * cgr_polyunion							*
 *									*
 * This function computes the union of two polygons using the union	*
 * clipping option in the GPC (General Polygon Clipper) library.	*
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
 *									*
 * cgr_polyunion ( npin0, xin0, yin0, npin1, xin1, yin1, maxnpo,	*
 * 		   npo, xo, yo, iret )					*
 *									*
 * Input parameters:							*
 * *npin0    	    int     Number of points in polygon 1               *
 * *xin0     	    float   X coordinates of polygon 1                  *
 * *yin0     	    float   Y coordinates of polygon 1                  *
 * *npin1    	    int     Number of points in polygon 2               *
 * *xin1     	    float   X coordinates of polygon 2                  *
 * *yin1     	    float   Y coordinates of polygon 2                  *
 * *maxnpo   	    int     Maximum number of output points		*
 *									*
 * Output parameters:							*
 * *npo      	    int     Number of points in output polygon          *
 * *xo       	    float   X coordinates of output polygon             *
 * *yo       	    float   Y coordinates of output polygon             *
 * *iret     	    int     Return code					*
 * 			    = -1 - insufficient maxnpo			*
 * 			    = -2 - polygons have insufficient # of pts	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 * D.W.Plummer/NCEP	02/04	Changes to use GPC library		*
 * J. Wu/SAIC		09/06	Added missing subscripts for contours	*
 ***********************************************************************/
{
int		ii, hole, ier=0;
gpc_polygon     gpc_poly_0, gpc_poly_1, gpc_union_poly;
gpc_vertex_list	verts;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    *npo  = 0;
    hole = 0;

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

    gpc_union_poly.hole     = (int*)NULL;
    gpc_union_poly.contour  = (gpc_vertex_list*)NULL;

    gpc_cvlist ( *npin0, xin0, yin0, &verts, &ier );
    gpc_poly_0.num_contours = 0;
    gpc_add_contour ( &gpc_poly_0, &verts, hole );
    free ( verts.vertex );

    gpc_cvlist ( *npin1, xin1, yin1, &verts, &ier );
    gpc_poly_1.num_contours = 0;
    gpc_add_contour ( &gpc_poly_1, &verts, hole );
    free ( verts.vertex );

    gpc_polygon_clip ( GPC_UNION, &gpc_poly_0, &gpc_poly_1, &gpc_union_poly );
    
    for ( ii = 0; ii < gpc_union_poly.num_contours; ii++ )  {
	if ( gpc_union_poly.hole[ii] == 0 )  {
            if ( gpc_union_poly.contour[ii].num_vertices <= *maxnpo )  {
                gpc_gvlist ( &gpc_union_poly.contour[ii], npo, xo, yo, &ier );
		break;
            }
            else  {
	        *iret = -1;
	        *npo = 0;
            }
	}
    }

    gpc_free_polygon ( &gpc_poly_0 );
    gpc_free_polygon ( &gpc_poly_1 );
    gpc_free_polygon ( &gpc_union_poly );

    return;

}
