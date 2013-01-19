#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"

/*
 * ALL FUNCTIONS in this module are PRIVATE to the CGR library and are
 * not intended to be called from an application.
 */

/************************************************************************
 * cgr_polyp                                                            *
 *                                                                      *
 * This module contains CGR UNION and LINK processing functions, as 	*
 * well as the POLYGON mapping functions used by smear and interpolate.	*
 *                                                                      *
 * Private Functions:                                                   *
 *                                                                      *
 * polyp_union		- compute UNION POLYGON of two POLYGONs		*
 * upolyp_create	- (local) create UPOLYPOINT structure		*
 * upolyp_destroy	- (local) destroy UPOLYPOINT structure		*
 * upolyp_dump		- (local) dump UPOLYPOINT structure		*
 * is_pp_intr		- (local) intersection check			*	
 * upolyp_sortii	- (local) sort intersections			*
 *									*
 * polyp_link 		- compute LINK  POLYGON of two POLYGONs		*
 * lpolyp_leftofseg	- (local) left of line segment computation	*
 * 									*
 * polyp_getmap		- compute mapping between two POLYGON		*
 * mpolyp_tdist		- compute total distance along MPOLYGON		*
 * mpolyp_numunmapped	- compute number of unmapped points		*
 * mpolyp_mapsection 	- map a section of an MPOLYGON			*
 * mpolyp_mapshare   	- share mapping information			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 * D.W.Plummer/NCEP     02/04	Additions for POLYGON interpolation	*
 ***********************************************************************/

struct upolypoint {
    POINT		*pt;		/* POINT        	 	*/
    struct upolypoint	*next[2];	/* ptrs to next pts in poly 1&2	*/
    char		nxtio[2];	/* in/out of next segment	*/
    struct upolypoint	*prev[2];	/* ptrs to prev pts in poly 1&2	*/
    char		prvio[2];	/* in/out of previous segment	*/
};
#define	UPOLYPOINT	struct upolypoint

struct upolygon {
    UPOLYPOINT	*first;			/* ptr to first polygon point	*/
};
#define	UPOLYGON		struct upolygon

struct ints {
    double	x;			/* intersection x coord		*/
    double	y;			/* intersection y coord		*/
    double	d1;			/* distance from poly0 seg from	*/
    double	d2;			/* distance from poly1 seg from	*/
    UPOLYPOINT	*from_1;		/* poly0 segment starting point	*/
    UPOLYPOINT	*to_1;			/* poly0 segment ending point	*/
    UPOLYPOINT	*from_2;		/* poly1 segment starting point	*/
    UPOLYPOINT	*to_2;			/* poly1 segment ending point	*/
};
#define	INTPOINT		struct ints

#define	NULL_UPOLYPOINT	( (UPOLYPOINT *)NULL )

#define	EPS		0.01F
#define	ZERO		0.00F


/*
 * Local Private Functions
 */

double  mpolyp_tdist ( MPOLYPOINT *p1, MPOLYPOINT *p2 );

int     mpolyp_numunmapped ( MPOLYPOINT *p0 );

void    mpolyp_mapsection ( MPOLYPOINT *pp0 );

void    mpolyp_mapshare      ( MPOLYGON *mpoly0, MPOLYGON *mpoly1, int which );

int 	lpolyp_leftofseg ( POLYGON *p0, POLYGON *p1, SEGMENT *seg );

int	upolyp_sortii ( INTPOINT *i1, INTPOINT *i2 );

void	upolyp_dump ( UPOLYPOINT *start, int which );

int	is_pp_intr ( UPOLYPOINT *pp );

UPOLYPOINT * upolyp_create ( POLYGON *poly, int wpoly );

void upolyp_destroy ( UPOLYPOINT *upoly0, UPOLYPOINT *upoly1 );

MPOLYPOINT *findpoint( MPOLYPOINT *pp, float x, float y );


/*=====================================================================*/


void polyp_union ( int *process_type, POLYGON *poly0, POLYGON *poly1,
	         POLYGON *poly_out, int *iret )

/************************************************************************
 * polyp_union								*
 *									*
 * This function computes either the union of two polygons		*
 * ( process_type == 0 (local parameter UNION) ) or the intersection	*
 * of two polygons ( process_type == 1 (local parameter INTERSECT) ).	*
 *									*
 * Cartesian (Normalized) coordinates are assumed with the first and	*
 * last points not being equal.						*
 *									*
 * The incoming polygon's points are assumed to be ordered consistent	*
 * with each other, i.e., both are ordered either clockwise or ccw.	*
 *									*
 * The incoming polygons are individually assumed to be regular, i.e.,	*
 * they do not cross themselves. For UNION, it is assumed that no	*
 * interior voids are present. For INTERSECT, it is assumed that	*
 * only one possible result exists.					*
 *									*
 * If one or fewer intersections occur between the polygons, no		*
 * computation is performed and the variable 'npo' is set to zero.	*
 *									*
 * polyp_union ( process_type, poly0, poly1, poly_out, iret )		*
 *									*
 * Input parameters:							*
 * *process_type    int     	Process type, =0-UNION, =1-INTERSECT	*
 * *poly0    	    POLYGON     Polygon #0				*
 * *poly1    	    POLYGON     Polygon #1				*
 *									*
 * Output parameters:							*
 * *poly_out   	    POLYGON     Output polygon				*
 * *iret     	    int     	Return code				*
 * 			    	= +1 - input polygons do not properly	*
 * 			    	       intersect (0 or 1 int pts)	*
 * 			    	= -3 - processing failed		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 ***********************************************************************/
{
int		ii, np, npin0, npin1, npinx, npo, which, intrsct, ier;
int		maxint, numint, total_int, *inout, found;
char		ptype;
float		*xpts, *ypts, *xin0, *yin0, *xin1, *yin1, *xo, *yo;
double		dist1, dist2, xint, yint;
UPOLYGON	upoly0, upoly1;
UPOLYPOINT	*pp, *pp_tmp, *t_start, *p0p, *p1p;
POLYPOINT	*polyp, *pop, *pop_tmp;
SEGMENT		s0, s1;
INTPOINT	*intr;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    ptype = (char)(*process_type);

    /*
     * 1) Move polygon points into working polygon structures.
     * 2) Find all intersections between the two polygons; for each, 
     *    insert a new point into each working polygon.
     * 3) Traverse point by point, changing polygons at the intersection
     *    points until the beginning is reached. Save points to xo and yo.
     * 4) Poly po is the computed polygon. Insert into polygon
     *    structure and return.
     */

    xin0 = NEW(float,LLMXPT);
    yin0 = NEW(float,LLMXPT);
    npin0 = 0;
    polyp = poly0->first;
    do {
	xin0[npin0] = polyp->pt->x;
	yin0[npin0] = polyp->pt->y;
	npin0 += 1;
	polyp = polyp->next;
    } while ( polyp != poly0->first );
    xin1 = NEW(float,LLMXPT);
    yin1 = NEW(float,LLMXPT);
    npin1 = 0;
    polyp = poly1->first;
    do {
	xin1[npin1] = polyp->pt->x;
	yin1[npin1] = polyp->pt->y;
	npin1 += 1;
	polyp = polyp->next;
    } while ( polyp != poly1->first );

    npinx = 2 * (npin0+npin1);

    xo = NEW(float,npinx);
    yo = NEW(float,npinx);

    xpts = NEW(float,npinx);
    ypts = NEW(float,npinx);

    inout = NEW(int,npinx);

    upoly0.first = upolyp_create ( poly0, POLY0 );
    upoly1.first = upolyp_create ( poly1, POLY1 );

    /*
     * Allocate space for theoretical maximum number of intersections.
     */
    maxint = LLMXPT;
    intr = NEW ( INTPOINT, maxint );
    numint = 0; 
    total_int = 0; 

    /*
     * For each poly0 segment, calculate intersection(s) w/ all
     * possible poly1 segments; insert intersection point into
     * each respective poly segment.
     */
    p0p = upoly0.first;
    s0.from = p0p->pt;
    s0.to   = p0p->next[POLY0]->pt;
    do {
	    p1p = upoly1.first;
	    s1.from = p1p->pt;
	    s1.to   = p1p->next[POLY1]->pt;
	    numint = 0;
	    do {
	        polyp_segint ( &s0, &s1, &intrsct, &xint, &yint, &ier );
	        if ( intrsct == 1 )  {
		    dist1 = (xint-s0.from->x)*(xint-s0.from->x) +
		            (yint-s0.from->y)*(yint-s0.from->y);
		    dist2 = (xint-s1.from->x)*(xint-s1.from->x) +
		            (yint-s1.from->y)*(yint-s1.from->y);
		    if ( dist1 <= EPS )  dist1 = ZERO;
		    if ( dist2 <= EPS )  dist2 = ZERO;
		    intr[numint].x = xint; intr[numint].y = yint;
		    intr[numint].d1= dist1; intr[numint].d2= dist2;
		    intr[numint].from_1 = p0p;
		    intr[numint].to_1   = p0p->next[POLY0];
		    intr[numint].from_2 = p1p;
		    intr[numint].to_2   = p1p->next[POLY1];
		    numint += 1;
		}
	        p1p = p1p->next[POLY1];
	        s1.from = p1p->pt;
	        s1.to   = p1p->next[POLY1]->pt;
	    } while ( p1p != upoly1.first );

	    total_int += numint;

	    p0p = p0p->next[POLY0];
	    s0.from = p0p->pt;
	    s0.to   = p0p->next[POLY0]->pt;

	    /*
	     * Sort from closest to farthest from seg0 starting point.
	     */
	    qsort ( intr, numint, sizeof(INTPOINT), 
		  (int(*)(const void*, const void*))upolyp_sortii );

	    /*
	     * Now insert each intersection point, usually must
	     * create a new point.
	     */
	    for ( ii = 0; ii < numint; ii++ )  {
		if ( ii == 0 )  {
		    if ( G_DIFF(intr[ii].d1, ZERO) )  {
		        pp = intr[ii].from_1;
			if ( !G_DIFF(intr[ii].d2, ZERO) )  {
		          pp->next[POLY1] = intr[ii].to_2;
		          pp->prev[POLY1] = intr[ii].from_2;
			  pp->next[POLY1]->prev[POLY1] = pp;
			  pp->prev[POLY1]->next[POLY1] = pp;
			}
			else  {
		          pp->next[POLY1] = intr[ii].to_2;
		          pp->prev[POLY1] = intr[ii].from_2->prev[POLY1];
			  pp->next[POLY1]->prev[POLY1] = pp;
			  pp->prev[POLY1]->next[POLY1] = pp;
			  if ( upoly1.first == intr[ii].from_2 )  
			      upoly1.first = pp;
			  free ( intr[ii].from_2 );
    			  nBytesFree += sizeof(UPOLYPOINT);
			}
		    }
		    else if ( G_DIFF(intr[ii].d2, ZERO) )  {
		        pp = intr[ii].from_2;
		        pp->next[POLY0] = intr[ii].to_1;
		        pp->prev[POLY0] = intr[ii].from_1;
			pp->next[POLY0]->prev[POLY0] = pp;
			pp->prev[POLY0]->next[POLY0] = pp;
		    }
		    else  {
		        pp = NEW(UPOLYPOINT,1);
    			nBytesAlloc += sizeof(UPOLYPOINT);
			pp->pt = polyp_newpt ( intr[ii].x, intr[ii].y );
		        intr[ii].from_1->next[POLY0] = pp;
		        intr[ii].from_2->next[POLY1] = pp;
		        pp->next[POLY0] = intr[ii].to_1;
		        pp->prev[POLY0] = intr[ii].from_1;
		        pp->next[POLY1] = intr[ii].to_2;
		        pp->prev[POLY1] = intr[ii].from_2;
		        pp->next[POLY1]->prev[POLY1] = pp;
		        pp->prev[POLY1]->next[POLY1] = pp;
		    }
		}
		else  {
		    pp_tmp = pp;
		    pp = NEW(UPOLYPOINT,1);
    		    nBytesAlloc += sizeof(UPOLYPOINT);
		    pp->pt = polyp_newpt ( intr[ii].x, intr[ii].y );
		    pp_tmp->next[POLY0] = pp;
		    pp->next[POLY0] = intr[ii].to_1;
		    pp->prev[POLY0] = pp_tmp;
		    pp->next[POLY1] = intr[ii].to_2;
		    pp->prev[POLY1] = intr[ii].from_2;
		    pp->next[POLY1]->prev[POLY1] = pp;
		    pp->prev[POLY1]->next[POLY1] = pp;
		}
	    }
    } while ( p0p != upoly0.first );

    free ( intr );

    if ( total_int <= 1 )  {
	*iret = +1;
    }
    else  {

	/*
	 * Compute intermediate points of poly 0 and send them to 
	 * cgr_inpoly against poly 1 for testing. Do the same for poly 1.
	 */
	np = 0;
	pp = upoly0.first;
	do {
	    xpts[np] = ( pp->pt->x + pp->next[POLY0]->pt->x ) / 2.0F;
	    ypts[np] = ( pp->pt->y + pp->next[POLY0]->pt->y ) / 2.0F;
	    np += 1;
	    pp = pp->next[POLY0];
	} while ( pp != upoly0.first );
        cgr_inpoly ( sys_D, &np, xpts, ypts,
		     sys_D, &npin1, xin1, yin1, inout, &ier );
	pp = upoly0.first;
	for ( ii = 0; ii < np; ii++ )  {
	    pp->nxtio[POLY0] = inout[ii];
	    pp->prvio[POLY0] = inout[(ii-1+np)%np];
	    pp = pp->next[POLY0];
	}

	np = 0;
	pp = upoly1.first;
	do {
	        xpts[np] = ( pp->pt->x + pp->next[POLY1]->pt->x ) / 2.0F;
	        ypts[np] = ( pp->pt->y + pp->next[POLY1]->pt->y ) / 2.0F;
	        np += 1;
	        pp = pp->next[POLY1];
	} while ( pp != upoly1.first );
        cgr_inpoly ( sys_D, &np, xpts, ypts,
		         sys_D, &npin0, xin0, yin0, inout, &ier );
	pp = upoly1.first;
	for ( ii = 0; ii < np; ii++ )  {
	        pp->nxtio[POLY1] = inout[ii];
	        pp->prvio[POLY1] = inout[(ii-1+np)%np];
	        pp = pp->next[POLY1];
	}

	/*
	 * Find an intersection point which will be a 
	 * traversal starting point.
	 */
	found = G_FALSE;
	t_start = upoly0.first;
	do {
		if ( is_pp_intr(t_start) )  {
		    if ( !is_pp_intr(t_start->next[POLY0]) && 
			  is_pp_intr(t_start->next[POLY1]) )  {
			if ( t_start->nxtio[POLY0] == OUT )  found = G_TRUE;
		    }
		    else if ( !is_pp_intr(t_start->next[POLY0]) && 
			      !is_pp_intr(t_start->next[POLY1]) )  {
			found = G_TRUE;
		    }
		}
	        if ( found == G_FALSE )  t_start = t_start->next[POLY0];
	} while ( found == G_FALSE && t_start != upoly0.first );

	if ( found == G_FALSE )  {

	        t_start = upoly1.first;
	        do {
		    if ( is_pp_intr(t_start) )  {
		        if ( !is_pp_intr(t_start->next[POLY1]) && 
			      is_pp_intr(t_start->next[POLY0]) )  {
			    if ( t_start->nxtio[POLY1] == OUT )  found = G_TRUE;
		        }
		    }
	            if ( found == G_FALSE )  t_start = t_start->next[POLY0];
	        } while ( found == G_FALSE && t_start != upoly1.first );

	}

	if ( found == G_FALSE )  {
	    /*
	     * Cannot find starting point.
	     */
	    *iret = -3;
	}
	else  {

	    /*
	     * Generate UNION or INTERSECT polygon.
	     *
	     * The first point is t_start which is an intersection point.
	     * The variable 'which' is set to traverse either inside or outside
	     * depending on the process_type.
	     * Traverse polygon saving each point until an intersection
	     * point with the other polygon is found.
	     * Continue traversal with the other polygon points until an 
	     * intersection point with the original is found.
	     * Switch back and forth, etc., until the starting point is reached.
	     */
            pp = t_start;
	    which = POLY0;
	    npo = 0;
            do {
	        /*  Start at an intersection point. */
	        if ( is_pp_intr ( pp->next[POLY0] )  ||
		     is_pp_intr ( pp->next[POLY1] ) )  {
		    if ( is_pp_intr ( pp->next[POLY0] )  &&
		         is_pp_intr ( pp->next[POLY1] ) )  {
		        if ( pp->next[POLY0] == pp->next[POLY1] )  {
		            /* Do nothing; save pt and go to next int */
		        }
		        else  {
		            if ( pp->nxtio[POLY0] == ptype )  which = POLY0;
		            if ( pp->nxtio[POLY1] == ptype )  which = POLY1;
		        }
		    }
		    else  {
		        if ( is_pp_intr ( pp->next[POLY0] ) )  which = POLY1;
		        if ( is_pp_intr ( pp->next[POLY1] ) )  which = POLY0;
		    }
	        }
	        else  {
		    if ( pp->nxtio[POLY0] == OUT && pp->nxtio[POLY1] == OUT )  {
		        which = ( which==POLY0 ) ? POLY1 : POLY0;
		    }
		    else  {
		        if ( pp->nxtio[POLY0] == ptype )  which = POLY0;
		        if ( pp->nxtio[POLY1] == ptype )  which = POLY1;
		    }
	        }
		/*
		 * Assign points to 'poly_out' here to make sure that all
		 * 'poly_out' points (pointers) equal those in either
		 * 'poly0' or 'poly1'.
		 */
		if ( npo == 0 )  {
		    pop = NEW(POLYPOINT,1);
    		    nBytesAlloc += sizeof(POLYPOINT);
		    pop->pt = pp->pt;
		    poly_out->first = pop;
		}
		else  {
		    pop_tmp = pop;
		    pop = NEW(POLYPOINT,1);
    		    nBytesAlloc += sizeof(POLYPOINT);
		    pop_tmp->next = pop;
		    pop->pt = pp->pt;
		    pop->prev = pop_tmp;
		}
	        xo[npo] = pp->pt->x; yo[npo] = pp->pt->y; (npo)++;
	        pp = pp->next[which];
	        while ( ! is_pp_intr ( pp )  )  {
		    pop_tmp = pop;
		    pop = NEW(POLYPOINT,1);
    		    nBytesAlloc += sizeof(POLYPOINT);
		    pop_tmp->next = pop;
		    pop->pt = pp->pt;
		    pop->prev = pop_tmp;
		    xo[npo] = pp->pt->x; yo[npo] = pp->pt->y; (npo)++;
	            pp = pp->next[which];
	        }
            } while ( pp != t_start );

	    poly_out->first->prev = pop;
	    pop->next = poly_out->first;

	}

    }

    /*
     * Free all temporary memory.
     */

    upolyp_destroy ( upoly0.first, upoly1.first );

    free ( inout );

    free ( ypts );
    free ( xpts );

    free ( yo );
    free ( xo );

    free ( yin1 );
    free ( xin1 );
    free ( yin0 );
    free ( xin0 );

    return;

}

int	upolyp_sortii ( INTPOINT *i1, INTPOINT *i2 )
/************************************************************************
 *									*
 * upolyp_sortii								*
 *									*
 * This function sorts intersections by distance.			*
 *									*
 * upolyp_sortii ( INTPOINT *i1, INTPOINT *i2 )				*
 *									*
 * Input parameters:							*
 * *i1  	INTPOINT	Intersection #1				*
 * *i2  	INTPOINT	Intersection #2				*
 *									*
 * Output parameters:							*
 * none									*
 *									*
 * Returned parameters:							*
 * upolyp_sortii	int		Logical return				*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 ***********************************************************************/
{
	if ( i1->d1 > i2->d1 )  
	    return (  1 );
	else
	    return ( -1 );

}


void	upolyp_dump ( UPOLYPOINT *start, int which )
/************************************************************************
 *									*
 * upolyp_dump								*
 *									*
 * This function dumps the contents of a working U polygon.		*
 *									*
 * upolyp_dump ( UPOLYPOINT *start, int which )				*
 *									*
 * Input parameters:							*
 * *start  	UPOLYPOINT	U polygon point				*
 * which  	int		Which U polygon to dump			*
 *									*
 * Output parameters:							*
 * none									*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 ***********************************************************************/
{
int	ii;
UPOLYPOINT	*pp;
/*---------------------------------------------------------------------*/

    printf("UPOLYGON %d :\n", which );
    pp = start;
    ii = 0;
    do {
        printf("Segment %d  (%4.1f,%4.1f) - %s - (%4.1f,%4.1f) - %s - (%4.1f,%4.1f) %s\n", 
           ii, 
	   pp->prev[which]->pt->x, pp->prev[which]->pt->y,
	   pp->prvio[which] == IN?"I":pp->prvio[which] == OUT?"O":"?",
	   pp->pt->x, pp->pt->y, 
	   pp->nxtio[which] == IN?"I":pp->nxtio[which] == OUT?"O":"?",
	   pp->next[which]->pt->x, pp->next[which]->pt->y,
           pp->next[which == POLY0?POLY1:POLY0] == (UPOLYPOINT *)NULL ? " " : "*" );
	pp = pp->next[which];
        ii++;
    } while ( pp != start );

}

int	is_pp_intr ( UPOLYPOINT *pp )
/************************************************************************
 *									*
 * is_pp_intr								*
 *									*
 * This function determines if a UPOLYPOINT point is an intersection.	*
 *									*
 * is_pp_intr ( UPOLYPOINT *pp )					*
 *									*
 * Input parameters:							*
 * *pp  	UPOLYPOINT	U polygon point				*
 *									*
 * Output parameters:							*
 * none									*
 *									*
 * Returned parameters:							*
 * is_pp_intr	int		G_TRUE or G_FALSE			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( pp->next[POLY0] == NULL_UPOLYPOINT )  return ( G_FALSE );
    if ( pp->next[POLY1] == NULL_UPOLYPOINT )  return ( G_FALSE );

    return ( G_TRUE );

}

UPOLYPOINT * upolyp_create ( POLYGON *poly, int wpoly )
/************************************************************************
 *									*
 * upolyp_create							*
 *									*
 * This function creates a working U polygon from a regular POLYGON.	*
 *									*
 * upolyp_create ( POLYGON *poly, int wpoly )				*
 *									*
 * Input parameters:							*
 * *poly  	POLYGON		POLYGON					*
 * wpoly   	int		Which U polygon to create, 0 or 1	*
 *									*
 * Output parameters:							*
 * *upolyp_create	UPOLYPOINT	Pointer to U polygon		*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 ***********************************************************************/
{
int		which, other;
UPOLYPOINT	*upp, *upp_tmp, *pstart;
POLYPOINT	*pp;
/*---------------------------------------------------------------------*/
	/*
	 * Create linked-list UPOLYGON from incoming regular POLYGON.
	 */

	which = wpoly==POLY0 ? POLY0 : POLY1;
	other = wpoly==POLY0 ? POLY1 : POLY0;

	pp = poly->first;
	do {
	  if ( pp == poly->first )  {
	    upp = NEW(UPOLYPOINT,1);
    	    nBytesAlloc += sizeof(UPOLYPOINT);
	    pstart = upp;
	    upp->pt = pp->pt;
	  }
	  else  {
	    upp_tmp = upp;
	    upp->prev[other] = NULL_UPOLYPOINT;
	    upp->next[other] = NULL_UPOLYPOINT;
	    upp = NEW(UPOLYPOINT,1);
    	    nBytesAlloc += sizeof(UPOLYPOINT);
	    upp_tmp->next[which] = upp;
	    upp->pt = pp->pt;
	    upp->next[other] = NULL_UPOLYPOINT;
	    upp->prev[which] = upp_tmp;
	    upp->prev[other] = NULL_UPOLYPOINT;
	  }
	  pp = pp->next;
	} while ( pp != poly->first );
	upp->next[which] = pstart;
	upp->next[other] = NULL_UPOLYPOINT;
	pstart->prev[which] = upp;
	pstart->prev[other] = NULL_UPOLYPOINT;

	return ( pstart );

}

/*=====================================================================*/

void upolyp_destroy ( UPOLYPOINT *upoly0, UPOLYPOINT *upoly1 )
/************************************************************************
 *									*
 * upolyp_destroy							*
 *									*
 * This function frees the memory used by the UNION polygons.		*
 *									*
 * upolyp_destroy ( UPOLYPOINT *upoly0, UPOLYPOINT *upoly1 )		*
 *									*
 * Input parameters:							*
 * *upoly0   	    UPOLYPOINT	Polygon #0 used to create UNION POLYGON	*
 * *upoly1   	    UPOLYPOINT	Polygon #1 used to create UNION POLYGON	*
 *									*
 * Output parameters:							*
 * none									*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 ***********************************************************************/
{
int		found;
UPOLYPOINT	*upp0, *upp1, *upp_next;
/*---------------------------------------------------------------------*/
	/*
	 * Free linked-list UPOLYGONs. 
	 */
	upp0 = upoly0;
	do {
	    upp_next = upp0->next[POLY0];
	    found = G_FALSE;
	    upp1 = upoly1;
	    do {
		if ( upp1->pt == upp0->pt )  {
		    found = G_TRUE;
		    break;
		}
		upp1 = upp1->next[POLY1];
	    } while ( upp1 != upoly1 );
	    if ( !found )  {
	        nBytesFree += sizeof(UPOLYPOINT);
		free ( upp0 );
	    }
	    upp0 = upp_next;
	} while ( upp0 != upoly0 );

	upp1 = upoly1;
	do {
	    upp_next = upp1->next[POLY1];
	    nBytesFree += sizeof(UPOLYPOINT);
	    free ( upp1 );
	    upp1 = upp_next;
	} while ( upp1 != upoly1 );

	return;

}

/*=====================================================================*/


void polyp_link  ( POLYGON *poly0, POLYGON *poly1, 
		   POLYGON *poly_link, int *iret )
/************************************************************************
 * polyp_link								*
 *									*
 * This function takes two polygons and 'links' them together.		*
 *									*
 * This function is designed for non-embedded polygons.			*
 *									*
 * The incoming polygons' points are assumed to be ordered in a 	*
 * counter-clockwise fashion (cgr_reorder).				*
 *									*
 * polyp_link  ( poly0, poly1, poly_link, iret )			*
 *									*
 * Input parameters:							*
 * *poly0	POLYGON		POLYGON #0				*
 * *poly1	POLYGON		POLYGON #1				*
 *									*
 * Output parameters:							*
 * *poly_link	POLYGON		'Linked' POLYGON			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 ***********************************************************************/
{
int		ier_0to1, ier_1to0;
POLYPOINT	*pp, *pp_tmp, *pp0, *pp1, *pp_start, *pp_end;
SEGMENT		seg_0to1, seg_1to0;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Call 'lpolyp_leftofseg' from p0 to p1 and p1 to p0 to determine the 
     * line segment such that all polygon points of both polygons lie on, 
     * or to the left of, the segment. 
     * 'Link' the two polygons by starting with the 'from' POINT of the 
     * segment from p0 to p1, then jump to p1 via the 'to' POINT. 
     * Traverse p1 from that 'to' point until the 'from' POINT of the 
     * segment from p1 to p0 is encountered. Jump back to p0 and traverse
     * until the original 'from' POINT is reached. Done.
     */
    
    ier_0to1 = lpolyp_leftofseg ( poly0, poly1, &seg_0to1 );
    ier_1to0 = lpolyp_leftofseg ( poly1, poly0, &seg_1to0 );

    if ( ier_0to1 != G_NORMAL || ier_1to0 != G_NORMAL )  {
	*iret = -1;
	return;
    }

    pp = NEW(POLYPOINT,1);
    poly_link->first = pp;
    nBytesAlloc += sizeof(POLYPOINT);
    pp->pt = seg_0to1.from;

    /*
     * Traverse poly1
     */
    pp_start = polyp_findpt ( seg_0to1.to  , poly1->first );
    pp_end   = polyp_findpt ( seg_1to0.from, poly1->first );

    pp1 = pp_start;
    do {
	pp_tmp = pp;
	pp = NEW(POLYPOINT,1);
        nBytesAlloc += sizeof(POLYPOINT);
	pp_tmp->next = pp;
	pp->prev = pp_tmp;
	pp->pt = pp1->pt;
	pp1 = pp1->next;
    } while ( pp1 != pp_end );
    pp_tmp = pp;
    pp = NEW(POLYPOINT,1);
    nBytesAlloc += sizeof(POLYPOINT);
    pp_tmp->next = pp;
    pp->prev = pp_tmp;
    pp->pt = pp_end->pt;

    /*
     * Traverse poly0
     */
    pp_start = polyp_findpt ( seg_1to0.to  , poly0->first );
    pp_end   = polyp_findpt ( seg_0to1.from, poly0->first );

    pp0 = pp_start;
    do {
	pp_tmp = pp;
	pp = NEW(POLYPOINT,1);
        nBytesAlloc += sizeof(POLYPOINT);
	pp_tmp->next = pp;
	pp->prev = pp_tmp;
	pp->pt = pp0->pt;
	pp0 = pp0->next;
    } while ( pp0 != pp_end );

    poly_link->first->prev = pp;
    pp->next = poly_link->first;

    return;

}

/*=====================================================================*/

int lpolyp_leftofseg ( POLYGON *p0, POLYGON *p1, SEGMENT *seg )
/************************************************************************
 * lpolyp_leftofseg							*
 *									*
 * This function takes two POLYGON and returns the SEGMENT such that	*
 * all POINT from both POLYGON are to the left of the segment.		*
 *									*
 * In the case where such a segment cannot be determined (embedded	*
 * POLYGON) and error is returned.					*
 *									*
 * lpolyp_leftofseg ( POLYGON *p0, POLYGON *p1, SEGMENT *seg )		*
 *									*
 * Input parameters:							*
 * *p0		POLYGON		POLYGON #0				*
 * *p1		POLYGON		POLYGON #1				*
 *									*
 * Output parameters:							*
 * *seg		SEGMENT		SEGMENT 				*
 *									*
 * Returned parameters:							*
 * lpolyp_lolse	int		Return code				*
 *				= -1 - cannot find solution		*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 ***********************************************************************/
{
int		done;
float		pdx, pdy, qdx, qdy, rol;
POLYPOINT	*pp0, *pp1, *pp0_k, *pp1_k;
/*---------------------------------------------------------------------*/

    done = G_FALSE;
    pp0 = p0->first;
    do {
        pp1 = p1->first;
        do {
	    pdx = pp1->pt->x - pp0->pt->x;
	    pdy = pp1->pt->y - pp0->pt->y;
	    pp0_k = p0->first;
	    do {
		qdx = pp0_k->pt->x - pp0->pt->x;
		qdy = pp0_k->pt->y - pp0->pt->y;
		rol = pdx*qdy - pdy*qdx;

		if ( rol < 0.0F )  break;

		pp0_k = pp0_k->next;
	    } while ( pp0_k != p0->first );
	    if (  rol >= 0.0F )  {
	        pp1_k = p1->first;
	        do {
		    qdx = pp1_k->pt->x - pp1->pt->x;
		    qdy = pp1_k->pt->y - pp1->pt->y;
		    rol = pdx*qdy - pdy*qdx;

		    if ( rol < 0.0F )  break;

		    pp1_k = pp1_k->next;
	        } while ( pp1_k != p1->first );
		if ( rol >= 0.0F )  {
		    done = G_TRUE;
		    seg->from = pp0->pt;
		    seg->to   = pp1->pt;
		    return ( G_NORMAL );
		}
	    }
	    pp1 = pp1->next;
        } while ( pp1 != p1->first && !done );

	pp0 = pp0->next;
    } while ( pp0 != p0->first && !done );

    return ( -1 );

}

/*=====================================================================*/

void polyp_getmap ( MPOLYGON *mpoly0, MPOLYGON *mpoly1, int nmap, 
		     float *xmap0, float *ymap0, 
		     float *xmap1, float *ymap1 )
/************************************************************************
 * polyp_getmap								*
 *									*
 * This function populates mapping polygons 'mpoly0' and 'mpoly1'	*
 * given a set of mapping points ('nmap' may be zero).			*
 *									*
 * If nmap, the number of input mapping points, is zero, then a set of	*
 * one or more default mapping points are generated as follows:		*
 * 1) Apply a convex hull to the two polygons. Traverse the convex hull	*
 *    and determine which points originated from which polygon. 	*
 *    Consecutive points from different polygons are mapped to one	*
 *    another.								*
 * 2) If no mapping points are found from 1), then one polygon lies 	*
 *    entirely within the other. In this case, use the old method of	*
 *    mapping the points with the greatest X-value as the default.	*
 *									*
 * polyp_getmap ( mpoly0, mpoly1, nmap,	xmap0, ymap0, xmap1, ymap1 )	*
 *									*
 * Input parameters:							*
 *	nmap	int	Number of pairs of points			*
 *	*xmap0	float	X coordinates of starting point			*
 *	*ymap0	float	Y coordinates of starting point 		*
 *	*xmap1	float	X coordinates of ending point			*
 *	*ymap1	float	Y coordinates of ending point 			*
 *									*
 * Input and Output parameters:						*
 *	*mpoly0	MPOLYGON	Mapping polygon #0			*
 *	*mpoly1	MPOLYGON	Mapping polygon #1			*
 *									*
 * Output parameters:							*
 * 	none								*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	10/03						*
 * D.W.Plummer/NCEP	 2/04	Taken from cgr_polysmear		*
 * D.W.Plummer/NCEP	 5/07	Improve default mapping algorithm	*
 ***********************************************************************/
{
int		nvalid, found, first, ii, ier;
MPOLYPOINT      *pp0, *pp1, *ptr0, *ptr1;
MPOLYPOINT      *start, *nextms;
float		dist, mindist;
int		npo, npin0, npin1, npin;
float		*xin0, *yin0, *xin1, *yin1, *xo, *yo;
float		maxX = FLT_MAX;
MPOLYPOINT      *p0maxX, *p1maxX;
/*---------------------------------------------------------------------*/

    if ( nmap == 0 )  {

	/*
	 * First, map any common POLYGON segments to each other.
	 */

	found = G_FALSE;
	pp0 = mpoly0->first;
	do {

	    pp1 = mpoly1->first;
	    do {

		if ( ( pp0->pt == pp1->pt )  &&
		     ( pp0->next->pt == pp1->next->pt ) )  {
		    pp0->map = pp1; pp1->map = pp0;
		    pp0->next->map = pp1->next; pp1->next->map = pp0->next;
		    found = G_TRUE;
		}

	        pp1 = pp1->next;
	    } while ( pp1 != mpoly1->first );

	    pp0 = pp0->next;
	} while ( pp0 != mpoly0->first );

	/*
	 * If common segments were found, then use a convex hull mapping 
	 * strategy to find appropriate mapping point pairs.
	 */
	if ( found == G_FALSE )  {

	    /*
	     * Gather up all the points from polygon 0
	     */
	    npin0 = 0;
            pp0 = mpoly0->first;
            do {
		npin0 += 1;
	        pp0 = pp0->next;
            } while ( pp0 != mpoly0->first );
	    G_MALLOC ( xin0, float, npin0, "Error allocating xin0" );
	    G_MALLOC ( yin0, float, npin0, "Error allocating yin0" );
	    npin = 0;
            pp0 = mpoly0->first;
            do {
		xin0[npin] = pp0->pt->x;
		yin0[npin] = pp0->pt->y;
		npin += 1;
	        pp0 = pp0->next;
            } while ( pp0 != mpoly0->first );
	    
	    /*
	     * Gather up all the points from polygon 1
	     */
	    npin1 = 0;
            pp1 = mpoly1->first;
            do {
		npin1 += 1;
	        pp1 = pp1->next;
            } while ( pp1 != mpoly1->first );
	    G_MALLOC ( xin1, float, npin1, "Error allocating xin1" );
	    G_MALLOC ( yin1, float, npin1, "Error allocating yin1" );
	    npin = 0;
            pp1 = mpoly1->first;
            do {
		xin1[npin] = pp1->pt->x;
		yin1[npin] = pp1->pt->y;
		npin += 1;
	        pp1 = pp1->next;
            } while ( pp1 != mpoly1->first );

	    G_MALLOC ( xo, float, npin1*npin0, "Error allocating xo" );
	    G_MALLOC ( yo, float, npin1*npin0, "Error allocating yo" );
	    
	    /*
	     * Generate a convex hull around all the points.
	     */
	    smpoly_rubberband ( &npin0, xin0, yin0, &npin1, xin1, yin1,
		    &npo, xo, yo, &ier );

	    /*
	     * Now, traverse the convex hull and check the origination of each
	     * point whether it comes from poly 0 or poly 1. If consecutive points
	     * originate from different polygons, then they define a mapping pair.
	     * Identify them as such.
	     */
	    first = G_TRUE;
	    for ( ii = 0; ii < npo; ii++ )  {
		pp0 = (MPOLYPOINT *)findpoint( mpoly0->first, xo[ii],         yo[ii] );
		pp1 = (MPOLYPOINT *)findpoint( mpoly1->first, xo[(ii+1)%npo], yo[(ii+1)%npo] );
		if ( ( pp0 != NULL ) && ( pp1 != NULL ) )  {
		    if ( pp0->map == NULL && pp1->map == NULL )  {
		    pp0->map = pp1;
		    pp1->map = pp0;
		    if ( first == G_TRUE )  {
			mpoly0->first = pp0;
			mpoly1->first = pp1;
			first = G_FALSE;
		    }
		    }
		}
		else  {
		    pp0 = (MPOLYPOINT *)findpoint( mpoly1->first, xo[ii],         yo[ii] );
		    pp1 = (MPOLYPOINT *)findpoint( mpoly0->first, xo[(ii+1)%npo], yo[(ii+1)%npo] );
		    if ( ( pp0 != NULL ) && ( pp1 != NULL ) )  {
		        if ( pp0->map == NULL && pp1->map == NULL )  {
		        pp0->map = pp1;
		        pp1->map = pp0;
		        if ( first == G_TRUE )  {
		            mpoly0->first = pp0;
		    	    mpoly1->first = pp1;
			    first = G_FALSE;
			}
			}
		    }
		}
	    }

	    /*
	     * In case the convex hull finds no mapping point pairs, revert to 
	     * using the maximum-X mapping as a default. This would occur if one
	     * polygon lies entirely within the other polygon.
	     */

	    if ( first == G_TRUE )  {

	        /*
	         * Find the vertex on mpoly0 and mpoly1 
	         * that have the greatest x-value and map those two points together.
	         */
            	maxX = -FLT_MAX;
            	pp0 = mpoly0->first;
            	do {
	            if ( pp0->pt->x > maxX )  {
		    	maxX = pp0->pt->x;
		    	p0maxX = pp0;
	            }
	            pp0 = pp0->next;
            	} while ( pp0 != mpoly0->first );
            	maxX = -FLT_MAX;
            	pp1 = mpoly1->first;
            	do {
	            if ( pp1->pt->x > maxX )  {
		    	maxX = pp1->pt->x;
		    	p1maxX = pp1;
	            }
	            pp1 = pp1->next;
	    	} while ( pp1 != mpoly1->first );
	    	mpoly0->first = p0maxX;
	    	mpoly1->first = p1maxX;

            	p0maxX->map = p1maxX;
            	p1maxX->map = p0maxX;

	    }

	    G_FREE ( yo, float );
	    G_FREE ( xo, float );
	    G_FREE ( yin1, float );
	    G_FREE ( xin1, float );
	    G_FREE ( yin0, float );
	    G_FREE ( xin0, float );

	}

    }
    else  {

	nvalid = 0;
	for ( ii = 0; ii < nmap; ii++ )  {

	    mindist = FLT_MAX;
            pp0 = mpoly0->first;
            do {
		dist = G_DIST ( pp0->pt->x, pp0->pt->y, xmap0[ii], ymap0[ii]);
		if ( dist < mindist )  {
		    ptr0 = pp0;
		    mindist = dist;
		}
		if ( G_DIFF(mindist, 0.0F) )  break;
		pp0 = pp0->next;
	    } while ( pp0 != mpoly0->first );
	
	    mindist = FLT_MAX;
            pp1 = mpoly1->first;
            do {
		dist = G_DIST ( pp1->pt->x, pp1->pt->y, xmap1[ii], ymap1[ii]);
		if ( dist < mindist )  {
		    ptr1 = pp1;
		    mindist = dist;
		}
		if ( G_DIFF(mindist, 0.0F) )  break;
		pp1 = pp1->next;
	    } while ( pp1 != mpoly1->first );
	    if ( ptr0->map == NULL_MPOLYPOINT && 
		 ptr1->map == NULL_MPOLYPOINT )  {
		if ( nvalid == 0 )  {
		    mpoly0->first = ptr0;
		    mpoly1->first = ptr1;
		    nvalid += 1;
		}
	        ptr0->map = ptr1;
	        ptr1->map = ptr0;
	    }
	    else  {
		/*
		 * Multiple maps to/from the same point.
		 * This mapping will be ignored.
		 */
	    }

	}

    }

    pp0 = mpoly0->first;
    while ( pp0->map == NULL_MPOLYPOINT )  pp0 = pp0->next;
    start = pp0;
    do {
	nextms = pp0->next;
	while ( nextms->map == NULL_MPOLYPOINT )  nextms = nextms->next;
	mpolyp_mapsection ( pp0 );
	pp0 = nextms;
    } while ( pp0 != start );

    /*
     * Map any remaining mpoly1 points into mpoly0.
     */
    mpolyp_mapshare ( mpoly0, mpoly1, POLY0 );

    /*
     * Map any remaining mpoly0 points into mpoly1.
     */
    mpolyp_mapshare ( mpoly1, mpoly0, POLY1 );

}

MPOLYPOINT *findpoint( MPOLYPOINT *pp, float x, float y )
/************************************************************************
 * findpoint								*
 *									*
 * This function finds a point (x,y) in the MPOLYPOINT structure pp.	*
 *									*
 * findpoint ( pp )							*
 *									*
 * Input parameters:							*
 *	*pp	MPOLYPOINT	Start of polypoint structure		*
 *	x	float		x coordinate				*
 *	y	float		y coordinate				*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Returned parameters:							*
 *	*findpoint	MPOLYPOINT	location of point (x,y) in pp	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 05/07						*
 ***********************************************************************/
{
MPOLYPOINT *pp0;
/*---------------------------------------------------------------------*/

    pp0 = pp;
    do {
	if ( G_DIFF(x, pp0->pt->x) && G_DIFF(y, pp0->pt->y) )  {
	    return ( pp0 );
	}
	pp0 = pp0->next;
    } while ( pp0 != pp );

    return ( (MPOLYPOINT *)NULL );

}

/*=====================================================================*/

void mpolyp_mapsection ( MPOLYPOINT *pp )
/************************************************************************
 * mpolyp_mapsection							*
 *									*
 * This function examines a mapping section and attempts to map as many	*
 * points as possible.							*
 *									*
 *									*
 * mpolyp_mapsection ( pp )						*
 *									*
 * Input parameters:							*
 *	*pp	MPOLYPOINT	Start of mapping section		*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	12/03						*
 * D.W.Plummer/NCEP	 2/04	Taken from cgr_polysmear		*
 ***********************************************************************/
{
int		nm0, nm1;
double		tdist0, tdist1, min_pct_diff, pct0, pct1, dpct;
MPOLYPOINT	*m0s, *m0e, *m1s, *m1e, *pp0, *pp1, *pp0x, *pp1x;
/*---------------------------------------------------------------------*/
    nm0 = mpolyp_numunmapped ( pp      );
    nm1 = mpolyp_numunmapped ( pp->map );

    if ( nm0 == 0 || nm1 == 0 )  return;

    m0s = pp;
    m0e = m0s->next;
    while ( m0e->map == NULL_MPOLYPOINT )  m0e = m0e->next;
    m1s = pp->map;
    m1e = m1s->next;
    while ( m1e->map == NULL_MPOLYPOINT )  m1e = m1e->next;

    if ( nm0 == nm1 )  {
	/*
	 * Number of points to be mapped in both sections is equal;
	 * map them one-to-one and return.
	 */ 
        pp0 = m0s->next; pp1 = m1s->next;
	do  {
	    pp0->map = pp1; pp1->map = pp0;
	    pp0 = pp0->next; pp1 = pp1->next;
	} while ( pp0 != m0e );
    }
    else  {
	/*
	 * Number of points to be mapped in both sections is unequal
	 * and non-zero.
	 * Find the pair of points that are closest to one another 
	 * in terms of percent distance traversed along section and
	 * map them together. This mapping divides the original section
	 * into two more sections; call this function again recursively
	 * for the two new sections.
	 */ 
	tdist0 = mpolyp_tdist ( m0s, m0e );
	tdist1 = mpolyp_tdist ( m1s, m1e );
	min_pct_diff = FLT_MAX;
	pp0 = m0s->next;
	while ( pp0 != m0e )  {
	    pp1 = m1s->next;
	    while ( pp1 != m1e )  {
		pct0 = mpolyp_tdist ( m0s, pp0 ) / tdist0;
		pct1 = mpolyp_tdist ( m1s, pp1 ) / tdist1;
		dpct = G_ABS ( pct0 - pct1 );
		if ( dpct < min_pct_diff )  {
		    min_pct_diff = dpct;
		    pp0x = pp0; 
		    pp1x = pp1;
		}
	        pp1 = pp1->next;
	    }
	    pp0 = pp0->next;
	}

	pp0x->map = pp1x; pp1x->map = pp0x;

	mpolyp_mapsection ( pp );

	mpolyp_mapsection ( pp0x );

    }
    
}


/*=====================================================================*/

void mpolyp_mapshare ( MPOLYGON *mpoly0, MPOLYGON *mpoly1, int which )
/************************************************************************
 * mpolyp_mapshare							*
 *									*
 * This function maps points from mpoly1 onto mpoly0.			*
 *									*
 *									*
 * mpolyp_mapshare ( mpoly0, mpoly1, which )				*
 *									*
 * Input parameters:							*
 *	*mpoly0	MPOLYGON	Polygon #1				*
 *	*mpoly1	MPOLYGON	Polygon #2				*
 *	int	which		Which MPOLYGON				*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 * D.W.Plummer/NCEP	 2/04	Taken from cgr_polysmear		*
 ***********************************************************************/
{
MPOLYPOINT       *pp, *pp0, *pp1, *ppnew, *start;
MPOLYPOINT       *m0s, *m0e, *m1s, *m1e;
double		dprev, dnext, x1, y1, x2, y2, dpp, dppnext;
double		tdist0, tdist1, pct1;
float		new_x, new_y;
/*---------------------------------------------------------------------*/

    /*
     * Traverse mpoly1 looking for any points that have not been 
     * mapped to mpoly0. For each, compute percent distance from previous
     * mapping point to next mapping point. Create a new point on mpoly0
     * that is the same percent distance from it's mapping start.
     * Don't create a new point if an unmapped point exists within 
     * a given tolerance.
     */

    /*
     * Find a mapping polygon point (MPOLYPOINT) that HAS NOT yet been 
     * mapped AND whose previous point HAS been mapped.
     * Assign this point to the variable 'start'.
     */
    pp1 = mpoly1->first;
    do {
	pp1 = pp1->next;
    } while ( pp1 != mpoly1->first &&
	    ( pp1->prev->map == NULL_MPOLYPOINT || 
	      pp1->map       != NULL_MPOLYPOINT ) );
    start = pp1;
    do {
	if ( pp1->map == NULL_MPOLYPOINT )  {
	    pp = pp1;
	    dprev = MPOLYPOINT_DIST ( pp1, pp->prev );
	    dnext = 0.0F;
	    while ( pp->map == NULL_MPOLYPOINT )  {
	        dnext += MPOLYPOINT_DIST ( pp, pp->next );
		pp = pp->next;
	    }
	    m1s = pp1->prev; m1e = pp;
	    tdist1 = mpolyp_tdist ( m1s, m1e );
	    if ( which == POLY1 )  {
	        m0s = m1s->map; m0e = m1e->map;
	    }
	    else if ( which == POLY0 )  {
	        m0s = m1s->map; m0e = m1e->map;
	    }
	    tdist0 = mpolyp_tdist ( m0s, m0e );
	    pct1 = dprev / tdist1;
	    pp0 = m0s;
	    dpp = 0.0F;
	    do {
		dppnext = mpolyp_tdist ( m0s, pp0->next );
		if ( dpp/tdist0 <= pct1 && dppnext/tdist0 > pct1 )  {
		  if ( G_ABS(dpp/tdist0-pct1) < 0.05 )  {
		      if ( pp0->map == NULL_MPOLYPOINT )  {
		          pp0->map = pp1;
		          pp1->map = pp0;
		      }
		  }
		  else if ( G_ABS(dppnext/tdist0-pct1) < 0.05 )  {
		      if ( pp0->next->map == NULL_MPOLYPOINT )  {
		          pp0->next->map = pp1;
		          pp1->map = pp0->next;
		      }
		  }
		  if ( pp1->map == NULL_MPOLYPOINT )  {
		    ppnew = NEW(MPOLYPOINT,1);
		    nBytesAlloc += sizeof(MPOLYPOINT);
		    x1 = pp0->pt->x; y1 = pp0->pt->y;
		    x2 = pp0->next->pt->x; y2 = pp0->next->pt->y;
		    new_x = x1 + 
			(x2-x1) * (((pct1*tdist0)-dpp)/(dppnext-dpp));
		    new_y = y1 + 
			(y2-y1) * (((pct1*tdist0)-dpp)/(dppnext-dpp));
		    ppnew->pt = polyp_newpt ( new_x, new_y );
		    ppnew->next = pp0->next;
		    ppnew->prev = pp0;
		    pp0->next->prev = ppnew;
		    pp0->next = ppnew;
		    ppnew->map = pp1;
		    pp1->map = ppnew;
		    ppnew->type = MPT_MAPPING;
		  }
		  break;
		}
		dpp = dppnext;
		pp0 = pp0->next;
	    } while ( pp0 != m0e );
	}
	pp1 = pp1->next;
    } while ( pp1 != start );

}

/*=====================================================================*/

double mpolyp_tdist ( MPOLYPOINT *p0, MPOLYPOINT *p1 )
/************************************************************************
 * mpolyp_tdist                                                         *
 *                                                                      *
 * This function calculates the total distance along a series of        *
 * MPOLYPOINT points, from point p0 to point p1, INCLUSIVE.             *
 *                                                                      *
 * mpolyp_tdist ( p0, p1 )                        			*
 *                                                                      *
 * Input parameters:                                                    *
 * *p0          MPOLYPOINT       Starting MPOLYGON point                *
 * *p1          MPOLYPOINT       Ending MPOLYGON point                  *
 *                                                                      *
 * Output parameters:                                                   *
 * none                                                                 *
 *                                                                      *
 * Returned value:                                                      *
 * mpolyp_tdist  double          Distance                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 * D.W.Plummer/NCEP	 2/04	Taken from cgr_polysmear		*
 ***********************************************************************/

{
    MPOLYPOINT	*pp;
    double	d;
/*---------------------------------------------------------------------*/
    d = 0.0F;
    pp = p0;
    do {
	d += MPOLYPOINT_DIST ( pp, pp->next );
	pp = pp->next;
    } while ( pp != p1 );

    return ( d );

}

/*=====================================================================*/

int mpolyp_numunmapped ( MPOLYPOINT *p0 )
/************************************************************************
 * mpolyp_numunmapped                                                	*
 *                                                                      *
 * This function calculates the number of unmapped points between	*
 * mapping points.							*
 *                                                                      *
 * mpolyp_tdist ( p0 )                     				*
 *                                                                      *
 * Input parameters:                                                    *
 * *p0          MPOLYPOINT       Starting MPOLYGON point                *
 *                                                                      *
 * Output parameters:                                                   *
 * none                                                                 *
 *                                                                      *
 * Returned value:                                                      *
 * mpolyp_numunmapped  int       Number of points			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 * D.W.Plummer/NCEP	 2/04	Taken from cgr_polysmear		*
 ***********************************************************************/

{
    MPOLYPOINT	*pp;
    int	n;
/*---------------------------------------------------------------------*/
    n = 0;
    pp = p0;
    while ( pp->next->map == NULL_MPOLYPOINT )  {
	n += 1;
	pp = pp->next;
    }

    return ( n );

}
