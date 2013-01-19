#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"

/*
 * ALL FUNCTIONS in this module are PRIVATE to the CGR library and are
 * not intended to be called from an application.
 */

/************************************************************************
 * cgr_poly								*
 *									*
 * This module contains CGR POINT and POLYGON processing functions.	*
 *                                                                      *
 * Public functions:							*
 *                                                                      *
 * polyp_create		- create a POLYGON from an array of points	*
 * polyp_destroy	- frees a POLYGON allocated memory		*
 * polyp_dump		- dumps the contents of a POLYGON		*
 * polyp_getpts		- pulls out the points from a POLYGON structure	*
 * polyp_scale		- scales the points of a POLYGON		*
 * polyp_unscale	- unscales the points of a POLYGON		*
 * polyp_newpt		- creates a new POINT				*
 * polyp_freepts	- frees all allocated POINT structures		*
 * polyp_tdist		- computes the distance around a POLYGON	*
 * polyp_rmpts		- removes POLYGON points			*
 * polyp_segint		- SEGMENT intersect checking			*
 * polyp_polyint	- POLYGON intersect checking			*
 * polyp_findpt		- Find POINT in POLYGON				*
 * polyp_dup		- Search and replace duplicate POINT values	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/

/*=====================================================================*/

void	polyp_dup ( POLYGON *poly0, POLYGON *poly1, float tol )
/************************************************************************
 * polyp_dup								*
 *                                                                      *
 * This function compares each POINT of POLYGON poly0 with each POINT	*
 * of POLYGON poly1. If a comparison indicates that the two points are	*
 * sufficiently close to one another, then the POINT pointer from poly0	*
 * is replaced with that from poly1.					*
 *                                                                      *
 * polyp_dup ( POLYGON *poly0, POLYGON *poly1 )				*
 *                                                                      *
 * Input parameters:                                                    *
 * *poly0		POLYGON		POLYGON #0			*
 * *poly1		POLYGON		POLYGON #1			*
 * tol			float		Tolerance value			*
 *                                                                      *
 * Output parameters:                                                   *
 * none									*
 *                                                                      *
 * Returned value:                                                 	*
 * none									*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     12/03                                           *
 ***********************************************************************/
{
POLYPOINT       *pp0, *pp1;
/*---------------------------------------------------------------------*/

    pp0 = poly0->first;
    do {
	pp1 = poly1->first;
	do {
	    if ( ( G_ABS(pp0->pt->x-pp1->pt->x) < tol ) &&
		 ( G_ABS(pp0->pt->y-pp1->pt->y) < tol ) )  {
		pp0->pt = pp1->pt;
		break;
	    }
	    pp1 = pp1->next;
	} while ( pp1 != poly1->first );
	pp0 = pp0->next;
    } while ( pp0 != poly0->first );

}

/*=====================================================================*/

POLYPOINT	*polyp_findpt ( POINT *point, POLYPOINT *first )
/************************************************************************
 * polyp_findpt								*
 *                                                                      *
 * This function finds a POINT in a POLYGON, if possible.		*
 *                                                                      *
 * polyp_findpt ( POINT *point, POLYPOINT *ppt )			*
 *                                                                      *
 * Input parameters:                                                    *
 * *point		POINT		POINT to find			*	
 * *first		POLYPOINT	First POLYGON point		*
 *                                                                      *
 * Output parameters:                                                   *
 * none									*
 *                                                                      *
 * Returned value:                                                 	*
 * *polyp_findpt	POLYPOINT	Found POLYPOINT			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
POLYPOINT       *pp;
/*---------------------------------------------------------------------*/

    pp = first;
    do  {
	if ( pp->pt == point )  return ( pp );
	pp = pp->next;
    } while ( pp != first );

    return ( NULL_POLYPOINT );

}

/*=====================================================================*/

void	polyp_rmpts ( POLYPOINT	*p0, POLYPOINT *p1 )
/************************************************************************
 * polyp_rmpts								*
 *                                                                      *
 * This function removes POLYGON points between point p0 and point p1,	*
 * NOT INCLUSIVE.							*
 *                                                                      *
 * polyp_rmpts ( POLYPOINT *p0, POLYPOINT *p1 )				*
 *                                                                      *
 * Input parameters:                                                    *
 * *p0		POLYPOINT	Starting POLYGON point			*
 * *p1		POLYPOINT	Ending POLYGON point			*
 *                                                                      *
 * Output parameters:                                                   *
 * none									*
 *                                                                      *
 * Returned value:                                                 	*
 * none									*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
POLYPOINT	*pp, *pp_next;
/*---------------------------------------------------------------------*/

    if ( p0->next == p1 )  return;

    pp = p0->next;
    do {
	pp_next = pp->next;
        nBytesFree += sizeof(POLYPOINT);
	free ( pp );
	pp = pp_next;
    }  while ( pp != p1 );

    p0->next = p1;
    p1->prev = p0;

    return;

}

/*=====================================================================*/

double	polyp_tdist ( POLYPOINT	*p0, POLYPOINT *p1 )
/************************************************************************
 * polyp_tdist								*
 *                                                                      *
 * This function calculates the total distance along a series of	*
 * POLYGON points, from point p0 to point p1, INCLUSIVE. Note that if	*
 * p0 and p1 are the same point, then the total perimeter distance	*
 * is returned.								*
 *                                                                      *
 * polyp_tdist ( POLYPOINT *p0, POLYPOINT *p1 )				*
 *                                                                      *
 * Input parameters:                                                    *
 * *p0		POLYPOINT	Starting POLYGON point			*
 * *p1		POLYPOINT	Ending POLYGON point			*
 *                                                                      *
 * Output parameters:                                                   *
 * none									*
 *                                                                      *
 * Returned value:                                                 	*
 * polyp_tdist	double		Distance				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
double		tdist;
POLYPOINT	*pp;
/*---------------------------------------------------------------------*/

    tdist = 0.0F;
    pp = p0;
    do {
	tdist += POLYPOINT_DIST ( pp, pp->next );
	pp = pp->next;
    }  while ( pp != p1 );
    return ( tdist );

}

/*=====================================================================*/

POINT * polyp_newpt ( float x, float y )
/************************************************************************
 * polyp_newpt								*
 *                                                                      *
 * This function allocates a new POINT with coordinate value (x,y) and	*
 * returns the pointer to it.						*
 *                                                                      *
 * polyp_newpt ( float x, float y )					*
 *                                                                      *
 * Input parameters:                                                    *
 * x		float	X-coordinate					*
 * y		float	Y-coordinate					*
 *                                                                      *
 * Output parameters:                                                   *
 * none									*
 *                                                                      *
 * Returned value:                                                 	*
 * polyp_newpt	POINT	Pointer to the new POINT			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
POINT	*pp;
/*---------------------------------------------------------------------*/

    pp = NEW(POINT,1);
    nBytesAlloc += sizeof(POINT);
    pp->x = x;
    pp->y = y;

    if ( ncgrPoints == maxcgrPoints )  {
	maxcgrPoints *= 2;
	cgrPoints = RENEW ( cgrPoints, POINT*, maxcgrPoints );
    }
    cgrPoints[ncgrPoints] = pp;
    ncgrPoints += 1;

    return ( pp );

}

/*=====================================================================*/

void polyp_freepts ( int *iret )
/************************************************************************
 * polyp_freepts							*
 *                                                                      *
 * This function frees the memory for all POINT allocations.		*
 *                                                                      *
 * polyp_freepts ( int *iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 * none									*
 *                                                                      *
 * Output parameters:                                                   *
 * *iret	int	Return code					*
 *                                                                      *
 * Returned value:                                                 	*
 * none									*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    for ( ii = 0; ii < ncgrPoints; ii++ )  {
	free ( cgrPoints[ii] );
    }
    nBytesFree += ncgrPoints * sizeof(POINT);
    ncgrPoints = 0;

    free ( cgrPoints );
    cgrPoints = (POINT **)NULL;

    return;

}

/*=====================================================================*/

POLYPOINT * polyp_create ( int *npin, float *xin, float *yin )
/************************************************************************
 * polyp_create								*
 *                                                                      *
 * This function generates a POLYGON structure from an input set of	*
 * points. The structure is a linked-list of POINT structures.		*
 *                                                                      *
 * polyp_create ( int *npin, float *xin, float *yin )			*
 *                                                                      *
 * Input parameters:                                                    *
 * *npin           int     Number of points in polygon			*
 * *xin            float   X coordinates of polygon			*
 * *yin            float   Y coordinates of polygon			*
 *                                                                      *
 * Output parameters:                                                   *
 * none									*
 *                                                                      *
 * Returned value:                                                 	*
 * *polyp_create   POLYPOINT	Pointer to the first POLYGON point	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
int		ii;
POLYPOINT	*pp, *pp_tmp, *pstart;
/*---------------------------------------------------------------------*/

    /*
     * Create linked-list polygon from incoming point arrays.
     */

    for ( ii = 0; ii < *npin; ii++ )  {
	if ( ii == 0 )  {
	    pp = NEW(POLYPOINT,1);
	    pstart = pp;
	    pp->pt = polyp_newpt ( xin[ii], yin[ii] );
        }
        else  {
	    pp_tmp = pp;
	    pp = NEW(POLYPOINT,1);
	    pp_tmp->next = pp;
	    pp->pt = polyp_newpt ( xin[ii], yin[ii] );
	    pp->next = NULL_POLYPOINT;
	    pp->prev = pp_tmp;
	}
    }
    nBytesAlloc += *npin * sizeof(POLYPOINT);
    pp->next = pstart;
    pstart->prev = pp;

    return ( pstart );

}

/*=====================================================================*/

void polyp_destroy ( POLYPOINT *poly_start )
/************************************************************************
 * polyp_destroy							*
 *                                                                      *
 * This function frees the memory that was allocated by polyp_create.	*
 *                                                                      *
 * polyp_destroy ( POLYPOINT *poly_start )				*
 *                                                                      *
 * Input parameters:                                                    *
 * *poly_start     POLYPOINT     Pointer to start of polygon		*
 *                                                                      *
 * Output parameters:                                                   *
 * none									*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
POLYPOINT	*pp, *pp_next;
/*---------------------------------------------------------------------*/

    /*
     * Free linked-list polygon.
     */

    pp = poly_start;
    do {
	pp_next = pp->next;
	if ( pp != NULL_POLYPOINT )  {
	    nBytesFree += sizeof(POLYPOINT);
	    free ( pp );
	}
	pp = pp_next;
    } while ( pp != poly_start );

    return;

}

/*=====================================================================*/

void polyp_getpts ( POLYPOINT *start, int *npo, float *xo, float *yo )
/************************************************************************
 * polyp_getpts								*
 *                                                                      *
 * This function converts a linked-list polygon into an array of points.*
 *                                                                      *
 * polyp_getpts ( POLYPOINT *start, int *npo, float *xo, float *yo )	*
 *                                                                      *
 * Input parameters:                                                    *
 * *start     	POLYPOINT  	Pointer to start of polygon		*
 *                                                                      *
 * Output parameters:                                                   *
 * *npo		int		Number of points			*
 * *xo		float		x-coordinates				*
 * *yo		float		y-coordinates				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
POLYPOINT	*pp;
/*---------------------------------------------------------------------*/

    /*
     * Traverse polygon to get points.
     */
    *npo = 0;
    pp = start;
    do {
	xo[*npo] = pp->pt->x;
	yo[*npo] = pp->pt->y;
	*npo += 1;
        pp = pp->next;
    } while ( pp != start );

    return;

}

/*=====================================================================*/

void polyp_scale ( POLYPOINT *start )
/************************************************************************
 * polyp_scale								*
 *                                                                      *
 * This function scales a linked-list polygon.				*
 *                                                                      *
 * polyp_scale ( POLYPOINT *start )					*
 *                                                                      *
 * Input parameters:                                                    *
 * *start     	POLYPOINT 	Pointer to start of polygon		*
 *                                                                      *
 * Output parameters:                                                   *
 * none									*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
POLYPOINT	*pp;
/*---------------------------------------------------------------------*/

    pp = start;
    do {
	pp->pt->x = TO_INT ( pp->pt->x );
	pp->pt->y = TO_INT ( pp->pt->y );
	pp = pp->next;
    } while ( pp != start );

    return;

}

/*=====================================================================*/

void polyp_unscale ( POLYPOINT *start )
/************************************************************************
 * polyp_scale								*
 *                                                                      *
 * This function unscales a linked-list polygon.			*
 *                                                                      *
 * polyp_scale ( POLYPOINT *start )					*
 *                                                                      *
 * Input parameters:                                                    *
 * *start     	POLYPOINT     Pointer to start of polygon		*
 *                                                                      *
 * Output parameters:                                                   *
 * none									*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
POLYPOINT	*pp;
/*---------------------------------------------------------------------*/

    pp = start;
    do {
	pp->pt->x = TO_FLT ( pp->pt->x );
	pp->pt->y = TO_FLT ( pp->pt->y );
	pp = pp->next;
    } while ( pp != start );

    return;

}

/*=====================================================================*/

void	polyp_dump ( POLYPOINT *start )
/************************************************************************
 * polyp_dump								*
 *                                                                      *
 * This function dumps the contents of a POLYGON.			*
 *                                                                      *
 * polyp_dump ( POLYPOINT *start )					*
 *                                                                      *
 * Input parameters:                                                    *
 * *start     	POLYPOINT     Pointer to start of polygon		*
 *                                                                      *
 * Output parameters:                                                   *
 * none									*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/
{
int	ii;
POLYPOINT	*pp;
/*---------------------------------------------------------------------*/

    pp = start;
    ii = 0;
    do {
        printf("Segment %d", ii );

        printf(" (%4.1f,%4.1f)",
	   pp->prev->pt != NULL_POINT ? pp->prev->pt->x : -9999.0F, 
	   pp->prev->pt != NULL_POINT ? pp->prev->pt->y : -9999.0F );

        printf(" - (%4.1f,%4.1f)",
	   pp->pt       != NULL_POINT ? pp->pt->x       : -9999.0F, 
	   pp->pt       != NULL_POINT ? pp->pt->y       : -9999.0F );

        printf(" - (%4.1f,%4.1f) %ld\n",
	   pp->next->pt != NULL_POINT ? pp->next->pt->x : -9999.0F, 
	   pp->next->pt != NULL_POINT ? pp->next->pt->y : -9999.0F,
	     (long)(pp->pt) );

	pp = pp->next;
        ii++;
    } while ( pp != start );

}

/*=====================================================================*/

int 	polyp_polyint ( POLYGON *p0, POLYGON *p1 )
/************************************************************************
 * cgr_polyint								*
 *									*
 * This function accepts two POLYGON structure pointers and determines	*
 * if the two polygons intersect.					*
 *									*
 * polyp_polyint ( POLYGON *p0, POLYGON *p1 )				*
 *									*
 * Input parameters:							*
 *	*p0		POLYGON		First POLYGON pointer		*
 *	*p1		POLYGON		Second POLYGON pointer		*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Returned parameters:							*
 *	polyp_polyint	int	G_TRUE or G_FALSE			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03	Created					*
 ***********************************************************************/
{
int		intrsct, ier;
double		xint, yint;
POLYPOINT	*pp0, *pp1;
SEGMENT		s0, s1;
/*---------------------------------------------------------------------*/

    pp0 = p0->first;
    do  {

	s0.from = pp0->pt;
	s0.to   = pp0->next->pt;
        pp1 = p1->first;
        do  {
	    
	    s1.from = pp1->pt;
	    s1.to   = pp1->next->pt;

	    polyp_segint ( &s0, &s1, &intrsct, &xint, &yint, &ier );

	    if ( intrsct == G_TRUE )  return ( G_TRUE );

	    pp1 = pp1->next;
        } while ( pp1 != p1->first );

	pp0 = pp0->next;
    } while ( pp0 != p0->first );

    return ( G_FALSE );
	
}

/*=====================================================================*/

void polyp_segint ( SEGMENT *s0, SEGMENT *s1,
		   int *intrsct, double *xint, double *yint, int *iret )
/************************************************************************
 * cgr_segint								*
 *									*
 * This function accepts two line segments s0 and s1:			*
 *									*
 * 	s0 = [ P1(x,y),P2(x,y) ) 					*
 * 	and 								*
 * 	s1 = [ P3(x,y),P4(x,y) ) 					*
 *									*
 * and determines if they intersect one another.			*
 *									*
 * The intersecting point is returned only if it falls on the segments	*
 * themselves, otherwise the intersecting point is (RMISSD,RMISSD).	*
 *									*
 * Note that the first endpoint of each segment is considered part of	*
 * the segment while the second endpoint is excluded for consideration.	*
 *									*
 * polyp_segint ( s0, s1, intrsct, xint, yint, iret )			*
 *									*
 * Input parameters:							*
 *	*s0	SEGMENT		First line segment		*
 *	*s1	SEGMENT		Second line segment		*
 *									*
 * Output parameters:							*
 *	*intrsct int	Result: 					*
 *			0-FALSE (the segments do not intersect),	*
 *			1-TRUE (the segments intersect, (xint,yint)	*
 *			  are returned)					*
 *	*xint	float	X-coordinate of intersecting point		*
 *	*yint	float	Y-coordinate of intersecting point		*
 *	*iret	 int	Return code					*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03	Created					*
 ***********************************************************************/
{
double	x1, y1, x2, y2, x3, y3, x4, y4;
double	a, b, c, s, t;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    *intrsct = 0;
    *xint = RMISSD;
    *yint = RMISSD;

    x1 = s0->from->x; 	y1 = s0->from->y;
    x2 = s0->to->x;   	y2 = s0->to->y;
    x3 = s1->from->x; 	y3 = s1->from->y;
    x4 = s1->to->x;   	y4 = s1->to->y;

    a = (y2-y1)*(x3-x4) - (x2-x1)*(y3-y4);
    b = (y3-y4)*(x1-x3) - (x3-x4)*(y1-y3);
    c = (x2-x1)*(y1-y3) - (y2-y1)*(x1-x3);

    if ( G_DIFF(a, 0.0F) )  {
	if ( G_DIFF(b, 0.0F) )  {
	    /*
	     * COINCIDE
	     */
	    if ( G_DIFF(x1, x2) )  {
		if ( y2 > y1 )  {
		    if ( y3 >= y1 && y3 < y2 )  *yint = y3;
		}
		if ( y1 > y2 )  {
		    if ( y3 <= y1 && y3 > y2 )  *yint = y3;
	    	}
		if ( y4 > y3 )  {
		    if ( y1 >= y3 && y1 < y4 )  *yint = y1;
		}
		if ( y3 > y4 )  {
		    if ( y1 <= y3 && y1 > y4 )  *yint = y1;
	    	}
		if ( !ERMISS(*yint) )  *xint = x1;
	    }
	    else  {
		if ( x2 > x1 )  {
		    if ( x3 >= x1 && x3 < x2 )  {
			*xint = x3;
			*yint = y3;
		    }
		}
		if ( x1 > x2 )  {
		    if ( x3 <= x1 && x3 > x2 )  {
			*xint = x3;
			*yint = y3;
		    }
		}
		if ( x4 > x3 )  {
		    if ( x1 >= x3 && x1 < x4 )  {
			*xint = x1;
			*yint = y1;
		    }
		}
		if ( x3 > x4 )  {
		    if ( x1 <= x3 && x1 > x4 )  {
			*xint = x1;
			*yint = y1;
		    }
		}
	    }
	}
	/*
	 * PARALLEL
	 */
	if ( !ERMISS(*xint) && !ERMISS(*yint) )  *intrsct = 1;
	return;
    }

    s = b / a;
    t = c / a;

    if ( s < 0.0F || s >= 1.0F )  return;
    if ( t < 0.0F || t >= 1.0F )  return;

    *intrsct = 1;
    *xint = x1 + s * ( x2 - x1 );
    *yint = y1 + s * ( y2 - y1 );

    return;

}

/*=====================================================================*/
