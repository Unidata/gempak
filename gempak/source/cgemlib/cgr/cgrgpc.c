#include "geminc.h"
#include "gemprm.h"
#include "gpc.h"
#include "proto_gpc.h"

void gpc_cvlist ( int npoly, float *px, float *py,
                        gpc_vertex_list *contour, int *iret )
/************************************************************************
 * gpc_cvlist                                                           *
 *                                                                      *
 * This function generates a GPC vertex list structure given a set of   *
 * polygon points.                                                      *
 *                                                                      *
 * gpc_cvlist ( npoly, px, py, contour, iret )                          *
 *                                                                      *
 * Input parameters:                                                    *
 *      *npoly          int             Number of points in polygon     *
 *      *px             float[]         X array                         *
 *      *py             float[]         Y array                         *
 *                                                                      *
 * Output parameters:                                                   *
 *      *contour        gpc_vertex_list Vertex list structure           *
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      2/04   Created                                 *
 ***********************************************************************/
{
int     ii;
/*---------------------------------------------------------------------*/

    *iret = 0;

    contour->num_vertices = npoly;

    G_MALLOC ( contour->vertex, gpc_vertex, npoly, "gpc_vertex creation" );

    for ( ii = 0; ii < npoly; ii++ )  {
        contour->vertex[ii].x = px[ii];
        contour->vertex[ii].y = py[ii];
    }

    return;

}

/*=======================================================================*/

float gpc_gvarea ( gpc_vertex_list *contour )
/************************************************************************
 * gpc_gvarea                                                           *
 *                                                                      *
 * This function computes the area bounded by a GPC vertex list.        *
 * Note the area may be positive (points counter clockwise) or negative *
 * (clockwise).                                                         *
 *                                                                      *
 * gpc_gvarea ( contour )                                               *
 *                                                                      *
 * Input parameters:                                                    *
 *      *contour        gpc_vertex_list Vertex list structure           *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 *                                                                      *
 * Returned parameters:                                                 *
 *      gpc_gvarea      float           Area                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     04/04   Created                                 *
 ***********************************************************************/
{
int     ii, nv, nbytes, ier;
float   *x, *y, xc, yc, area;
/*---------------------------------------------------------------------*/

    nv = contour->num_vertices;

    nbytes = nv * sizeof(float);
    G_MALLOC ( x, float, nbytes, "x" );
    G_MALLOC ( y, float, nbytes, "y" );

    for ( ii = 0; ii < nv; ii++ )  {
        x[ii] = (float)contour->vertex[ii].x;
        y[ii] = (float)contour->vertex[ii].y;
    }

    cgr_centroid ( x, y, &nv, &xc, &yc, &area, &ier );

    G_FREE ( y, float );
    G_FREE ( x, float );

    return ( area );

}

/*======================================================================*/

void gpc_gvlist ( gpc_vertex_list *contour,
                int *npoly, float *px, float *py, int *iret )
/************************************************************************
 * gpc_gvlist                                                           *
 *                                                                      *
 * This function takes a GPC vertex list structure and returns a set of *
 * polygon points.                                                      *
 *                                                                      *
 * gpc_gvlist ( contour, npoly, px, py, iret )                          *
 *                                                                      *
 * Input parameters:                                                    *
 *      *contour        gpc_vertex_list Vertex list structure           *
 *                                                                      *
 * Output parameters:                                                   *
 *      *npoly          int             Number of points in polygon     *
 *      *px             float[]         X array                         *
 *      *py             float[]         Y array                         *
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      2/04   Created                                 *
 ***********************************************************************/
{
int     ii;
/*---------------------------------------------------------------------*/

    *iret = 0;

    *npoly = contour->num_vertices;

    for ( ii = 0; ii < *npoly; ii++ )  {
        px[ii] = contour->vertex[ii].x;
        py[ii] = contour->vertex[ii].y;
    }

    return;

}

/*======================================================================*/

void gpc_trpoly ( gpc_polygon *poly, int scal, int *iret )
/************************************************************************
 * gpc_trpoly                                                         	*
 *                                                                      *
 * This function takes a GPC polygon and truncates the x and y values	*
 * of all vertices to 'scal' decimal places.				*
 *                                                                      *
 * gpc_trpoly ( poly, scal, iret )                          		*
 *                                                                      *
 * Input and Output parameters:                                         *
 *      *poly        	gpc_polygon 	GPC polygon			*
 *                                                                      *
 * Output parameters:                                                   *
 *      scal            int		Truncation factor		*
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      1/07   Created                                 *
 ***********************************************************************/
{
int     jj, kk;
double	dscal, dpow;
/*---------------------------------------------------------------------*/

    *iret = 0;

    dpow = scal;
    dscal = pow ( 10.0, dpow );

    for ( jj = 0; jj < poly->num_contours; jj++ )  {

	for( kk = 0; kk < poly->contour[jj].num_vertices; kk++ )  {

	    poly->contour[jj].vertex[kk].x = 
		G_NINT(poly->contour[jj].vertex[kk].x * dscal) / dscal;

	    poly->contour[jj].vertex[kk].y = 
		G_NINT(poly->contour[jj].vertex[kk].y * dscal) / dscal;

	}

    }

    return;

}
