#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"
#include "gpc.h"
#include "proto_gpc.h"

#define SMALL_F	.0001

/*
 * Private functions
 */
void 	smpoly_dump ( MPOLYGON poly, int which );

void 	smpoly_original ( int *npin0, float *xin0, float *yin0, int *npin1, 
		          float *xin1, float *yin1, int *nmp, float *xmp0, 
		          float *ymp0, float *xmp1, float *ymp1, int *maxnpo, 
		          int *npo, float *xo, float *yo, int *iret );

void 	smpoly_merge ( int *npin, float *xin, float *yin, 
		       int *npo, float *xo, float *yo );

void 	smpoly_getnextp ( float x1, float y1, int np, float *xall, float *yall, 
	 	          float *x2, float *y2, int *iret );

/*=====================================================================*/

/*
 * Public functions
 */
void cgr_polysmear ( char * smear_type, int *npin0, float *xin0, 
		     float *yin0, int *npin1, float *xin1, float *yin1, 
		     int *nmp, float *xmp0, float *ymp0, float *xmp1, 
		     float *ymp1, int *maxnpo, int *npo, float *xo, 
		     float *yo, int *iret )
/************************************************************************
 * cgr_polysmear							*
 *									*
 * This function takes two polygons and calculates the 'smear' polygon.	*
 * Cartesian coordinates are assumed.					*
 * Polygons are assumed to be ordered in a ccw fashion.			*
 * Closed polygons are assumed with unequal first and last points.	*
 *									*
 * cgr_polysmear ( smear_type, npin0, xin0, yin0, npin1, xin1, yin1,    *
 * 	           npo, xo, yo, iret )					*
 *									*
 * Input parameters:							*
 *	*smear_type	char	Smear type ORIGINAL/RUBBERBAND		*
 *	*npin0		int	Number of points in polygon1		*
 *	*xin0		float	X coordinates of polygon 1		*
 *	*yin0		float	Y coordinates of polygon 1		*
 *	*npin1		int	Number of points in polygon2		*
 *	*xin1		float	X coordinates of polygon 2		*
 *	*yin1		float	Y coordinates of polygon 2		*
 *									*
 * Output parameters:							*
 *	*npo		int	Number of points in output polygon	*
 *	*xo		float	X coordinates of output polygon		*
 *	*yo		float	Y coordinates of output polygon		*
 *	*iret		int	Return code				*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC	 	10/04	Created					*
 ***********************************************************************/
{
    if ( strcasecmp ( smear_type, "ORIGINAL" ) == 0 ) {
       smpoly_original ( npin0, xin0, yin0, npin1, xin1, yin1, nmp,
        	      xmp0, ymp0, xmp1, ymp1, maxnpo, npo, 
		      xo, yo, iret );
    }
    else if ( strcasecmp ( smear_type, "RUBBERBAND" ) == 0 ) {
       smpoly_rubberband ( npin0, xin0, yin0, npin1, xin1, yin1, 
                        npo, xo, yo, iret );
    }
}

/*=====================================================================*/

void smpoly_rubberband ( int *npin0, float *xin0, float *yin0, 
		      int *npin1, float *xin1, float *yin1, 
		      int *npo, float *xo, float *yo, int *iret )
/************************************************************************
 * smpoly_rubberband							*
 *									*
 * This function takes two polygons and calculates the 'smear' polygon 	*
 * using the 'Rubber Band' algorithm.					*
 *									*
 * smpoly_rubberband ( npin0, xin0, yin0, npin1, xin1, yin1, 		*
 * 	            npo, xo, yo, iret )					*
 *									*
 * Input parameters:							*
 *	*npin0		int	Number of points in polygon1		*
 *	*xin0		float	X coordinates of polygon 1		*
 *	*yin0		float	Y coordinates of polygon 1		*
 *	*npin1		int	Number of points in polygon2		*
 *	*xin1		float	X coordinates of polygon 2		*
 *	*yin1		float	Y coordinates of polygon 2		*
 *									*
 * Output parameters:							*
 *	*npo		int	Number of points in output polygon	*
 *	*xo		float	X coordinates of output polygon		*
 *	*yo		float	Y coordinates of output polygon		*
 *	*iret		int	Return code				*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC	 	10/04	Created					*
 ***********************************************************************/
{
int 	ii, nptotal, ier;
float	*xall, *yall;
/*---------------------------------------------------------------------*/

    /*
     * Allocate memory for an array to hold all input points
     */
    xall = NEW ( float, *npin0 + *npin1 );
    yall = NEW ( float, *npin0 + *npin1 );

    /*
     * Put all input points into one single array
     */
    nptotal = 0;
    smpoly_merge ( npin0, xin0, yin0, &nptotal, xall, yall );
    smpoly_merge ( npin1, xin1, yin1, &nptotal, xall, yall );

    /*
     * Find the left most point. This point must be one of the result points.
     */
    *npo = 0;
    xo [ 0 ] = xall [ 0 ];
    yo [ 0 ] = yall [ 0 ];

    for ( ii = 1; ii < nptotal; ii++ ) {
	if ( xall [ ii ] < xo [ 0 ] ) {
	   xo [ 0 ] = xall [ ii ];
	   yo [ 0 ] = yall [ ii ];
	}
	else if ( fabs ( xall [ ii ] - xo [ 0 ] ) < SMALL_F ) {
	   if ( yall [ ii ] < yo [ 0 ] ) {
	      xo [ 0 ] = xall [ ii ];
	      yo [ 0 ] = yall [ ii ];
	   }
	}
    }	

    (*npo)++;
    
    /*
     * Find the second point that makes all remaining points are located
     * to one side of the line defined by the two points until the second
     * point equals the first point in the result point set.
     */
    do { 

       smpoly_getnextp ( xo[ *npo - 1 ], yo[ *npo - 1 ], nptotal, xall, yall, 
		      &xo[ *npo ], &yo[ *npo ], &ier );
       (*npo)++;

    } while ( !( fabs( xo [ *npo - 1 ] - xo [ 0 ] ) < SMALL_F &&
	         fabs( yo [ *npo - 1 ] - yo [ 0 ] ) < SMALL_F ) &&
	      ( ier == 0 ) && ( *npo <= nptotal ) );

    (*npo)--;

    free ( xall );
    free ( yall );

    *iret = 0;
}

/*=====================================================================*/

void smpoly_original ( int *npin0, float *xin0, float *yin0, int *npin1, 
		    float *xin1, float *yin1, int *nmp, float *xmp0, 
		    float *ymp0, float *xmp1, float *ymp1, int *maxnpo, 
		    int *npo, float *xo, float *yo, int *iret )
/************************************************************************
 * smpoly_original								*
 *									*
 * This function takes two polygons and calculates the 'smear' polygon.	*
 * Cartesian coordinates are assumed.					*
 * Polygons are assumed to be ordered in a ccw fashion.			*
 * Closed polygons are assumed with unequal first and last points.	*
 *									*
 * smpoly_original ( npin0, xin0, yin0, npin1, xin1, yin1,        	*
 * 	       npo, xo, yo, iret )					*
 *									*
 * Input parameters:							*
 *	*npin0		int	Number of points in polygon1		*
 *	*xin0		float	X coordinates of polygon 1		*
 *	*yin0		float	Y coordinates of polygon 1		*
 *	*npin1		int	Number of points in polygon2		*
 *	*xin1		float	X coordinates of polygon 2		*
 *	*yin1		float	Y coordinates of polygon 2		*
 *									*
 * Output parameters:							*
 *	*npo		int	Number of points in output polygon	*
 *	*xo		float	X coordinates of output polygon		*
 *	*yo		float	Y coordinates of output polygon		*
 *	*iret		int	Return code				*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	10/03						*
 * D.W.Plummer/NCEP	02/04	Function name change to polyp_getmap	*
 * B. Yin/SAIC		10/04	Changed routine name from cgr_polysmear	*
 * D.W.Plummer/NCEP	01/07	Added calls to cgr_trpoly		*
 * D.W.Plummer/NCEP	02/07	Set GPC epsilon from prefs table	*
 * E. Safford/SAIC	03/07	fix error on max_area assignment	*
 * D.W.Plummer/NCEP	05/07	Rm any remaining original mapping pts	*
 * D.W.Plummer/NCEP	07/07	Init mapping poly 'type' to MPT_ORIGINAL*
 ***********************************************************************/
{
int		nmap, ier;
float		*xmap0, *ymap0, *xmap1, *ymap1;
MPOLYGON	mpoly0, mpoly1;
MPOLYPOINT	*mpp_tmp, *mpp, *mpp0, *mpp_next;
int		four=4, *inout;
float		quad_x[4], quad_y[4];
POLYGON		poly0, poly1;
POLYPOINT	*pp;
char		tag[128];
double		epsilon;

gpc_polygon     gpc_poly, gpc_smear_poly;
gpc_vertex_list verts;

int 		ii, nonholes, which, nbytes;
float 		max_area, area;
/*---------------------------------------------------------------------*/

    ctb_pfstr ( "GFA_SMEAR_EPSILON", tag, &ier );
    if ( ier == 0 ) {
	sscanf ( tag, "%lf", &epsilon );
    }
    else {
	epsilon = DBL_EPSILON;
    }
    gpc_set_epsilon ( epsilon );

    /*
     * Initialize GPC polygon structures.
     */
    gpc_poly.num_contours = 0;
    gpc_poly.hole         = (int*)NULL;
    gpc_poly.contour      = (gpc_vertex_list*)NULL;

    gpc_smear_poly.num_contours = 0;
    gpc_smear_poly.hole         = (int*)NULL;
    gpc_smear_poly.contour      = (gpc_vertex_list*)NULL;

    verts.vertex            = (gpc_vertex*)NULL;
    verts.num_vertices      = 0;

    /*
     * Generate POLYGON structures.
     */
    poly0.first = polyp_create ( npin0, xin0, yin0 );
    poly1.first = polyp_create ( npin1, xin1, yin1 );

    /*
     * Make working copies of the input polygons in linked-list structures. 
     *
     * LET WORKING POLYGON P1 (x1[npin0],y1[npin0]) BE THE POLY WITH 
     * THE FEWER NUMBER OF POINTS.
     */
    nmap = *nmp;
    xmap0  = NEW ( float, G_MAX(nmap,10) );
    ymap0  = NEW ( float, G_MAX(nmap,10) );
    xmap1  = NEW ( float, G_MAX(nmap,10) );
    ymap1  = NEW ( float, G_MAX(nmap,10) );
    inout  = NEW (   int, ((*npin0)*(*npin1)) );
    if ( *npin0 <= *npin1 )  {
	memcpy ( xmap0, xmp0, nmap*sizeof(float) );
	memcpy ( ymap0, ymp0, nmap*sizeof(float) );
	memcpy ( xmap1, xmp1, nmap*sizeof(float) );
	memcpy ( ymap1, ymp1, nmap*sizeof(float) );
    }
    else  {
	memcpy ( xmap0, xmp1, nmap*sizeof(float) );
	memcpy ( ymap0, ymp1, nmap*sizeof(float) );
	memcpy ( xmap1, xmp0, nmap*sizeof(float) );
	memcpy ( ymap1, ymp0, nmap*sizeof(float) );
    }

    /*
     * Create linked-list mapping polygons from the input polygons
     * 'poly0' and 'poly1'.
     */
    pp = poly0.first;
    do {
	if ( pp == poly0.first )  {
	    mpp = NEW(MPOLYPOINT,1);
	    nBytesAlloc += sizeof(MPOLYPOINT);
	    mpoly0.first = mpp;
	    mpp->pt = pp->pt;
	    mpp->map = NULL_MPOLYPOINT;
	    mpp->type = MPT_ORIGINAL;
	}
	else  {
	    mpp_tmp = mpp;
	    mpp = NEW(MPOLYPOINT,1);
	    nBytesAlloc += sizeof(MPOLYPOINT);
	    mpp_tmp->next = mpp;
	    mpp->pt = pp->pt;
	    mpp->prev = mpp_tmp;
	    mpp->map = NULL_MPOLYPOINT;
	    mpp->type = MPT_ORIGINAL;
	}
	pp = pp->next;
    } while ( pp != poly0.first );
    mpp->next = mpoly0.first;
    mpoly0.first->prev = mpp;

    pp = poly1.first;
    do {
	if ( pp == poly1.first )  {
	    mpp = NEW(MPOLYPOINT,1);
	    nBytesAlloc += sizeof(MPOLYPOINT);
	    mpoly1.first = mpp;
	    mpp->pt = pp->pt;
	    mpp->map = NULL_MPOLYPOINT;
	    mpp->type = MPT_ORIGINAL;
	}
	else  {
	    mpp_tmp = mpp;
	    mpp = NEW(MPOLYPOINT,1);
	    nBytesAlloc += sizeof(MPOLYPOINT);
	    mpp_tmp->next = mpp;
	    mpp->pt = pp->pt;
	    mpp->prev = mpp_tmp;
	    mpp->map = NULL_MPOLYPOINT;
	    mpp->type = MPT_ORIGINAL;
	}
	pp = pp->next;
    } while ( pp != poly1.first );
    mpp->next = mpoly1.first;
    mpoly1.first->prev = mpp;

    /*
     * Map each mapping polygon into the other.
     */
    polyp_getmap ( &mpoly0, &mpoly1, nmap, xmap0, ymap0, xmap1, ymap1 );

    /*
     * GPC_UNION the MAPPED polygons as a first guess into 
     * the 'smear' GPC polygon.
     */
    mpp0 = mpoly0.first;
    ii = 0;
    do {
        xo[ii] = mpp0->pt->x; yo[ii] = mpp0->pt->y; ii += 1;
	mpp0 = mpp0->next;
    } while ( mpp0 != mpoly0.first );
    gpc_cvlist ( ii, xo, yo, &verts, &ier );
    gpc_add_contour ( &gpc_poly, &verts, G_FALSE );
    free ( verts.vertex );

    mpp0 = mpoly1.first;
    ii = 0;
    do {
        xo[ii] = mpp0->pt->x; yo[ii] = mpp0->pt->y; ii += 1;
	mpp0 = mpp0->next;
    } while ( mpp0 != mpoly1.first );
    gpc_cvlist ( ii, xo, yo, &verts, &ier );
    gpc_add_contour ( &gpc_smear_poly, &verts, G_FALSE );
    free ( verts.vertex );

    gpc_polygon_clip ( GPC_UNION, 
	    &gpc_poly, &gpc_smear_poly, &gpc_smear_poly );
    gpc_free_polygon ( &gpc_poly );

    /*
     * Loop over the mapping polygon segments.
     * Create a quadrilateral using a segment from each and GPC_UNION it
     * with the 'smear' GPC polygon.
     * The end result after the mapping polygons have been traversed
     * will be the total smear polygon.
     * Be fastidious about cleaning up memory along the way.
     */
    mpp0 = mpoly0.first;
    do {
	quad_x[0] = mpp0->pt->x;       
	   quad_y[0] = mpp0->pt->y;
	quad_x[1] = mpp0->next->pt->x; 
	   quad_y[1] = mpp0->next->pt->y;
	quad_x[2] = mpp0->next->map->pt->x; 
	   quad_y[2] = mpp0->next->map->pt->y;
	quad_x[3] = mpp0->next->map->prev->pt->x; 
	   quad_y[3] = mpp0->next->map->prev->pt->y;
        gpc_cvlist ( four, quad_x, quad_y, &verts, &ier );
        gpc_poly.num_contours = 0;
        gpc_add_contour ( &gpc_poly, &verts, G_FALSE );
        free ( verts.vertex );

	gpc_trpoly ( &gpc_poly, 3, &ier );
	gpc_trpoly ( &gpc_smear_poly, 3, &ier );

        gpc_polygon_clip ( GPC_UNION, 
		&gpc_poly, &gpc_smear_poly, &gpc_smear_poly );
        gpc_free_polygon ( &gpc_poly );
	mpp0 = mpp0->next;
    } while ( mpp0 != mpoly0.first );

    /*
     * By definition, the 'smear' of two singular polygons should be
     * a singular polygon, but sometimes there are small slivers of
     * polygons also returned. Find the non-hole polygon with the 
     * largest area and return that one.
     *
     * Also check for excessive number of vertices.
     */
    nonholes = 0;
    for ( ii = 0; ii < gpc_smear_poly.num_contours; ii++ )  {
	if ( gpc_smear_poly.hole[ii] == 0 )  {
	    nonholes += 1;
	    which = ii;
	}
    }
    if ( nonholes > 0 )  {
	if ( nonholes > 1 )  {
	    max_area = -FLT_MAX;
	    for ( ii = 0; ii < gpc_smear_poly.num_contours; ii++ )  {
		if ( gpc_smear_poly.hole[ii] == 0 )  {
		    area = gpc_gvarea(&(gpc_smear_poly.contour[ii]));
		    if ( G_ABS(area) > max_area )  {
			which = ii;
			max_area = G_ABS( area );
		    }
		}
	    }
	}
	if ( gpc_smear_poly.contour[which].num_vertices <= *maxnpo )  {
	    gpc_gvlist ( &(gpc_smear_poly.contour[which]), npo, xo, yo, &ier );
	}
        else  {
	    *iret = -1;
	    *npo = 0;
        }
    }
    else  {
	*iret = -1;
	*npo = 0;
    }

    /*
     * Remove any non-original remaining mapping points
     */
    mpp0 = mpoly0.first;
    do {
	if ( mpp0->type == MPT_MAPPING )  {
	    for ( ii = 0; ii < *npo; ii++ )  {
		if ( G_DIFFT ( mpp0->pt->x, xo[ii], 0.001 ) &&
		     G_DIFFT ( mpp0->pt->y, yo[ii], 0.001 ) )  {
		    nbytes = sizeof(float) * ( *npo - ii - 1 );
		    memmove ( &(xo[ii]), &(xo[ii+1]), nbytes );
		    memmove ( &(yo[ii]), &(yo[ii+1]), nbytes );
		    *npo -= 1;
		}
	    }
	}
	mpp0 = mpp0->next;
    } while ( mpp0 != mpoly0.first );

    mpp0 = mpoly1.first;
    do {
	if ( mpp0->type == MPT_MAPPING )  {
	    for ( ii = 0; ii < *npo; ii++ )  {
		if ( G_DIFFT ( mpp0->pt->x, xo[ii], 0.001 ) &&
		     G_DIFFT ( mpp0->pt->y, yo[ii], 0.001 ) )  {
		    nbytes = sizeof(float) * ( *npo - ii - 1 );
		    memmove ( &(xo[ii]), &(xo[ii+1]), nbytes );
		    memmove ( &(yo[ii]), &(yo[ii+1]), nbytes );
		    *npo -= 1;
		}
	    }
	}
	mpp0 = mpp0->next;
    } while ( mpp0 != mpoly1.first );

    /*
     * Free the GPC smear polygon...
     */
    gpc_free_polygon ( &gpc_smear_poly );

    /*
     * Free the CGR polygons...
     */
    polyp_destroy ( poly0.first );
    polyp_destroy ( poly1.first );

    mpp = mpoly0.first;
    do {
	mpp_next = mpp->next;
        nBytesFree += sizeof(MPOLYPOINT);
	free ( mpp );
	mpp = mpp_next;
    } while ( mpp != mpoly0.first );

    mpp = mpoly1.first;
    do {
	mpp_next = mpp->next;
        nBytesFree += sizeof(MPOLYPOINT);
	free ( mpp );
	mpp = mpp_next;
    } while ( mpp != mpoly1.first );

    /*
     * And all the rest.
     */
    free ( inout );
    free ( ymap1 );
    free ( xmap1 );
    free ( ymap0 );
    free ( xmap0 );

    return;

}

/*=====================================================================*/

void smpoly_dump ( MPOLYGON poly, int which )
/************************************************************************
 * smpoly_dump                                                          *
 *                                                                      *
 * This function dumps the contents of an MPOLYGON.                     *
 *                                                                      *
 * smpoly_dump ( MPOLYGON poly, int which )				*
 *                                                                      *
 * Input parameters:                                                    *
 * poly  	MPOLYGON 	Mapped polygon				*
 * which  	int 		Which mapping				*
 *                                                                      *
 * Output parameters:                                                   *
 * none                                                                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/

{
int		ii;
/* float		tx, ty; */
MPOLYPOINT      *pp;
/*---------------------------------------------------------------------*/

    printf("POLYGON %d :\n", which );
    pp = poly.first;
    ii = 0;
    do {
	printf("Segment %d  (%4.1f,%4.1f) - (%4.1f,%4.1f) - (%4.1f,%4.1f) MAP TO (%4.1f,%4.1f) (%s)\n",
	    ii,
	    pp->prev->pt->x, pp->prev->pt->y,
	    pp->pt->x, pp->pt->y,
	    pp->next->pt->x, pp->next->pt->y,
	    pp->map==NULL_MPOLYPOINT ? RMISSD : pp->map->pt->x,
	    pp->map==NULL_MPOLYPOINT ? RMISSD : pp->map->pt->y,
	    pp->type == MPT_ORIGINAL ? "O" : "M" );

	ii++;
	pp = pp->next;
    } while ( pp != poly.first );

}

/*=====================================================================*/

void smpoly_merge ( int *npin, float *xin, float *yin, 
		 int *npo, float *xo, float *yo )
/************************************************************************
 * smpoly_merge								*
 *									*
 * This function merges two sets of points.				*
 *									*
 * smpoly_merge ( npin, xin, yin, npo, xo, yo, iret )			*
 *									*
 * Input parameters:							*
 *	*npin		int	Number of input points 			*
 *	*xin		float	X coordinates of input set		*
 *	*yin		float	Y coordinates of input set		*
 *									*
 * Output parameters:							*
 *	*npo		int	Number of points in output set		*
 *	*xo		float	X coordinates of output set		*
 *	*yo		float	Y coordinates of output set		*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC	 	10/04	Created					*
 ***********************************************************************/
{
int ii, jj, add;
/*---------------------------------------------------------------------*/

    for ( ii = 0; ii < *npin; ii++ ) {

	add = G_TRUE;

        for ( jj = 0; jj < *npo; jj++ ) {

	    if ( ( fabs( xin [ ii ] - xo [ jj ] ) < SMALL_F ) &&
		 ( fabs( yin [ ii ] - yo [ jj ] ) < SMALL_F ) ) {

	       add = G_FALSE;
	       break;

            }
	}

	if ( add ) {

	   xo [ *npo ] = xin [ ii ];
	   yo [ *npo ] = yin [ ii ];

	   (*npo)++;

   	}
    }
}

/*=====================================================================*/

void smpoly_getnextp ( float x1, float y1, int np, float *xall, float *yall, 
		    float *x2, float *y2, int *iret )
/************************************************************************
 * smpoly_getnextp								*
 *									*
 * This function gets the second point that makes all remaining points  *
 * are located to one side of the line defined by (x1, y1) and (x2, y2)	*
 *									*
 * smpoly_getnextp ( x1, y1, np, xall, yall, x2, y2, iret )		*
 *									*
 * Input parameters:							*
 *	x1		float	X coordinates of the first point	*
 *	y1		float	Y coordinates of the first point	*
 *	np		int	Number of input point set		*
 *	*xall		float	X coordinates of input point set	*
 *	*yall		float	Y coordinates of input point set	*
 *									*
 * Output parameters:							*
 *	*x2		float	X coordinates of the second point	*
 *	*y2		float	Y coordinates of the second point	*
 *	*iret		int	Return code				*
 *                              =  0: normal    	                *
 *                              = -1: no such point			*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC	 	10/04	Created					*
 ***********************************************************************/
{
int ii, jj, found;
/*---------------------------------------------------------------------*/

    for ( ii = 0; ii < np; ii++ ) {

        if ( ( fabs ( yall[ ii ] - y1 ) < SMALL_F ) &&
	     ( fabs ( xall[ ii ] - x1 ) < SMALL_F ) )
	   continue;

        found = G_TRUE;

        for ( jj = 0; jj < np; jj++ ) {

            if ( (( fabs ( yall[ ii ] - yall[ jj ] ) < SMALL_F ) &&
	          ( fabs ( xall[ ii ] - xall[ jj ] ) < SMALL_F ))||
		 (( fabs ( y1 - yall[ jj ] ) < SMALL_F ) &&
		  ( fabs ( x1 - xall[ jj ] ) < SMALL_F ) ))
	       continue;

	    if ( ( ( yall[ ii ] - y1 )*( xall [ jj ] - x1 )
		  -( yall[ jj ] - y1 )*( xall [ ii ] - x1 )) < 0 ) {

	       found = G_FALSE;
	       break;

            }
	}

	if ( found ) {

	   *x2  = xall [ ii ];
	   *y2  = yall [ ii ];
	   *iret = 0;
	   break;

   	}
    }

    if ( !found ) *iret = -1;
}

