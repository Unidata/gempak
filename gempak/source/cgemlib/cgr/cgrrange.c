#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"

/*
 * Local structure definitions.
 */
typedef struct rangepolypoint {
    POINT			pt;	/* point 			*/
    struct rangepolypoint	*next;	/* ptr to next pt            	*/
    struct rangepolypoint	*prev;	/* ptr to prev pt            	*/
    int				roll;	/* roll (wrap) value		*/
    float			cross;	/* cross flag			*/
} RANGEPOLYPOINT;

#define	NULL_RANGEPOLYPOINT	( (RANGEPOLYPOINT 	*)NULL )

struct rangepolygon {
    RANGEPOLYPOINT		*first;	/* ptr to first polygon point	*/
};
#define	RANGEPOLYGON		struct rangepolygon

#define	EXTRAforPOLY		8

/*
 * Private function.
 */
void range_pole ( int *npts, float *lx, float *ly, int *r, float *c, 
		  int *nout, int *minroll, int *maxroll );

void cgr_range ( char *syspts, int *npts, float *x, float *y, int *qpoly,
	char *sysout, int *rollout, int *nout, float *xout, float *yout,
	float *xll, float *yll, float *xur, float *yur, int *iret )
/************************************************************************
 * cgr_range								*
 *									*
 * This function accepts a line (set of points) and converts the line	*
 * coordinates to an output coordinate system and computes the range	*
 * of the points. For most coordinate system transformations, the range	*
 * is simply the min and max of the converted line points.		*
 * The output set of line points may be the same as the input set.	*
 * However, for cylindrical coordinate displays the sequence of points	*
 * may "wrap around" the earth producing inaccurate or misleading range	*
 * values. In these cases, the line figure is "rolled out" onto a 	*
 * continuous coordinate system and a flag is set with the number of	*
 * revolutions expanded.						*
 * For instance:							*
 * For non-cylindrical projections rollout=0.				*
 * For 'PROJ=ced/0;0;0' and 'GAREA=-45;-90;45;90' and a line/polygon	*
 * crossing the ID (longitude=180) from +170 to -170, rollout would	*
 * equal 1 with the longitude range +170 to +190.			*
 *									*
 * This function requires that GPLT coordinate tranform functions are	*
 * available, and that a map projection and device have been specified.	*
 * If the requested output coordinate system (sysout) is sys_I (linear	*
 * wrt grid) or sys_G (grid), then the grid projection must be set as 	*
 * well.								*
 *									*
 * Note that if the input coordinate system (syspts) is S, D, N or L	*
 * and the output coordinate system is I or G (or vice versa), then	*
 * this function will not produce correct results since the final	*
 * transformation via gtrans will pass through sys_M which will always	*
 * scale longitudes to inside the bounds (-180,180).			* 
 *									*
 * The flag 'qpoly' is set to G_TRUE if the set of points represents a	*
 * polygon which must be checked for pole circumscription (north or	*
 * south). 								*
 *									*
 * The number of returned points 'nout' will differ from the 		*
 * number of input points if all of the following criteria are met:	*
 * 1) the working projection is cylindrical,				*
 * 2) qpoly is set to G_TRUE,						*
 * 3) the set of points (polygon) circumscribes one or both poles.	*
 *									*
 * cgr_range ( syspts, npts, x, y, qpoly, sysout,			*
 *             rollout, nout, xout, yout, xll, yll, xur, yur, iret)	*
 *									*
 * Input parameters:							*
 *	*syspts		char	Line coordinate system			*
 *	*npts		int	Number of points in line		*
 *	*x		float[]	X array of line points			*
 *	*y		float[]	Y array of line points			*
 *	*qpoly		int	Flag, G_TRUE if polygon, else G_FALSE	*
 *	*sysout		char	Output coordinate system		*
 *									*
 * Output parameters:							*
 *	*rollout	int	Range flag, (always 0 for non-cyl proj)	*
 *	*nout		int	Number of output points			*
 *	*xout		float[]	X array of line points			*
 *	*yout		float[]	Y array of line points			*
 *	*xll		float	X-coordinate range lower left		*
 *	*yll		float	Y-coordinate range lower left		*
 *	*xur		float	X-coordinate range upper right		*
 *	*yur		float	Y-coordinate range upper right		*
 *	*iret		int	Return code				*
 *				= 0 - NORMAL				*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 8/03	Created					*
 * D.W.Plummer/NCEP	 9/03	Added sys_WORK dependent on output sys	*
 * D.W.Plummer/NCEP	 1/04	Chgs for polygon pole circumscription	*
 * D.W.Plummer/NCEP	 4/04	Added 2nd parameter in call to G_FREE	*
 * S. Gilbert/NCEP	 8/06	Changed working coordinate system to "U"*
 ***********************************************************************/
{
int 	ii, roll, minroll, maxroll, *r, kx, ky, ier, nbytes; 
char	proj[9], sys_WORK[2];
float	angl1, angl2, angl3,  dlatll, dlonll, dlatur, dlonur;
float	minrollTWOPI, TWOPIDEG=TWOPI*RTD;
float	*lx, *ly, *c;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    *xll =  FLT_MAX; *yll =  FLT_MAX;
    *xur = -FLT_MAX; *yur = -FLT_MAX;
    *rollout = 0;

/*
 * Determine working coordinate system... 
 * If user requested sys_I or sys_G as the output coordinate 
 * system (sysout), then use sys_I as the working coordinate system. 
 * Otherwise use sys_U as the working coordinate system.
 * The working coordinate system is sys_WORK.
 */
    if ( strcmp(sysout,sys_I) == 0 || strcmp(sysout,sys_G) == 0 )  {
	strcpy ( sys_WORK, sys_I );
        gqgprj ( proj, &angl1, &angl2, &angl3, &kx, &ky, 
	         &dlatll, &dlonll, &dlatur, &dlonur, &ier,
	         sizeof(proj) );
    }
    else  {
	strcpy ( sys_WORK, sys_U );
        gqmprj ( proj, &angl1, &angl2, &angl3,
	         &dlatll, &dlonll, &dlatur, &dlonur, &ier,
	         sizeof(proj) );
    }

    if ( strncmp(proj,"MER",3) == 0  ||
	 strncmp(proj,"CED",3) == 0  ||
	 strncmp(proj,"MCD",3) == 0 )  {

/*
 * Allocate space for sys_WORK coordinates and roll values.
 */
	nbytes = *npts + EXTRAforPOLY;
	G_MALLOC ( lx, float, nbytes, "CGR_RANGE - lx" );
	G_MALLOC ( ly, float, nbytes, "CGR_RANGE - ly" );
	G_MALLOC (  r,   int, nbytes, "CGR_RANGE - r" );
	G_MALLOC (  c, float, nbytes, "CGR_RANGE - c" );

/*
 * Convert points to sys_WORK coordinates.
 */
	gtrans ( syspts, sys_WORK, npts, x, y, lx, ly, &ier,
		 strlen(syspts), strlen(sys_WORK) );

/*
 * Determine roll value for each point.
 * Roll = 0 means point is in base area.
 * Roll > 0 means point is in area # of rolls to the right.
 * Roll < 0 means point is in area # of rolls to the left.
 */
	minroll =  INT_MAX;
	maxroll = -INT_MAX;
	roll = (int)( lx[0] / TWOPI );
        for ( ii = 0; ii < (*npts)-1; ii++ )  {
	    r[ii] = roll;
	    c[ii] = RMISSD;
	    if ( lx[ii] >   HALFPI  && lx[ii+1] < (-HALFPI) )  {
		c[ii] = roll*TWOPI + PI;
		roll = roll + 1;
	    }
	    if ( lx[ii] < (-HALFPI) && lx[ii+1] >   HALFPI  )  {
		c[ii] = (roll-1)*TWOPI + PI;
		roll = roll - 1;
	    }
	    minroll = G_MIN ( minroll, r[ii] );
	    maxroll = G_MAX ( maxroll, r[ii] );
	}
	r[(*npts-1)] = roll;
	c[(*npts-1)] = RMISSD;
	if ( lx[(*npts-1)] >   HALFPI  && lx[0] < (-HALFPI) )
		c[(*npts-1)] = roll*TWOPI + PI;
	if ( lx[(*npts-1)] < (-HALFPI) && lx[0] >   HALFPI  )
		c[(*npts-1)] = roll*TWOPI + PI;
	minroll = G_MIN ( minroll, r[(*npts-1)] );
	maxroll = G_MAX ( maxroll, r[(*npts-1)] );

/*
 * Now adjust sys_WORK coordinates using roll value.
 */
	for ( ii = 0; ii < *npts; ii++ )  {
	    lx[ii] += ( r[ii] * TWOPI );
	}

/*
 * Call pole processing if necessary.
 */
	if ( *qpoly == G_TRUE && ( minroll != maxroll ) )  {
	    range_pole ( npts, lx, ly, r, c, nout, &minroll, &maxroll );
	}
	else  {
	    *nout = *npts;
	}

	*rollout = maxroll - minroll;

/*
 * Final adjustment to ensure all points are in positive rolls.
 */
	if ( minroll != 0 )  {
	  minrollTWOPI = minroll * TWOPI;
	  for ( ii = 0; ii < *nout; ii++ )  {
	    lx[ii] -= minrollTWOPI ;
	  }
	}

/*
 * Transform to output coordinate system and 
 * calculate min and max.
 */
	gtrans ( sys_WORK, sysout, nout, lx, ly, xout, yout, &ier,
		 strlen(sys_WORK), strlen(sysout) );
	for ( ii = 0; ii < *nout; ii++ )  {
	    *xll = G_MIN ( *xll, xout[ii] );
	    *yll = G_MIN ( *yll, yout[ii] );
	    *xur = G_MAX ( *xur, xout[ii] );
	    *yur = G_MAX ( *yur, yout[ii] );
	}

/*
 * If sysout==sys_M, gtrans will automatically scale all
 * longitudes back into the range (-180,180), so extend the
 * right longitude out 'rollout' number of 360 degree intervals.
 */
	if ( strcmp(sysout,sys_M) == 0  &&  *rollout != 0 )  {
	    *yur += (*rollout) * TWOPIDEG;
	    for ( ii = 0; ii < *nout; ii++ )  
		yout[ii] += ( (r[ii]-minroll) * TWOPIDEG );
	}

/*
 * Remember to free temporary memory.
 */
	G_FREE (  c, float );
	G_FREE (  r,   int );
	G_FREE ( ly, float );
	G_FREE ( lx, float );

    }
    else  {

	gtrans ( syspts, sysout, npts, x, y, xout, yout, &ier,
		 strlen(syspts), strlen(sysout) );
/*
 * Simple - find min and max.
 */
	for ( ii = 0; ii < *npts; ii++ )  {
	    *xll = G_MIN ( *xll, xout[ii] );
	    *yll = G_MIN ( *yll, yout[ii] );
	    *xur = G_MAX ( *xur, xout[ii] );
	    *yur = G_MAX ( *yur, yout[ii] );
	}
	*nout = *npts;

    }

}

/*=====================================================================*/

void range_pole ( int *npts, float *lx, float *ly, int *r, float *c, 
		  int *nout, int *minroll, int *maxroll )
/************************************************************************
 * range_pole								*
 *									*
 * This function links a circumpolar polygon with the pole for fill and	*
 * range purposes.							*
 *									*
 * range_pole ( int *npts, float *lx, float *ly, int *r, float *c, 	*
 *              int *nout, int *minroll, int *maxroll )			*
 *									*
 * Input parameters:							*
 *	*npts		int	Number of incoming polygon points	*
 *	*c		float	Polygon cross values			*
 *									*
 * Input and Output parameters:						*
 *	*lx		float	'L' coordinate x values			*
 *	*ly		float	'L' coordinate y values			*
 *	*r		int	Polygon roll values			*
 * 									*
 * Output parameters:							*
 *	*nout		int	Number of returned polygon points	*
 *	*minroll	int	Minimum roll value			*
 *	*maxroll	int	Maximum roll value			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	10/03	Created					*
 ***********************************************************************/
{
int		ii, tmp_dir, nxt_dir, prv_dir, np=1;
float		ymin, ymax, mid_y, pct;
RANGEPOLYGON	poly;
RANGEPOLYPOINT	*pp, *pp1, *pp2, *pp3, *pp4;
RANGEPOLYPOINT	*pp_tmp, *pp_nextX, *pp_prevX;
/*---------------------------------------------------------------------*/

/*
 * Note, polygons circling the earth will be filled to the 
 * pole closest to the nearest breakpoint.
 */

/*
 * Put polygon and roll and cross values in linked list
 * for easier processing.
 */
    for ( ii = 0; ii < *npts; ii++ )  {
	if ( ii == 0 )  {
	    G_MALLOC ( pp, RANGEPOLYPOINT, np, "CGR_RANGE - pp" );
	    poly.first = pp;
	    pp->pt.x = lx[ii]; pp->pt.y = ly[ii];
	    pp->roll = r[ii];
	    pp->cross = c[ii];
	}
	else  {
	    pp_tmp = pp;
	    G_MALLOC ( pp->next, RANGEPOLYPOINT, np, "CGR_RANGE - pp->next" );
	    pp = pp->next;
    	    pp->prev = pp_tmp;
	    pp->pt.x = lx[ii]; pp->pt.y = ly[ii];
	    pp->roll = r[ii];
	    pp->cross = c[ii];
	}
    }
    pp->next = poly.first;
    poly.first->prev = pp;

/*
 * Process northern hemisphere.
 *
 * Search for crossing with greatest 'y' value and examine
 * previous and next crossings' directions. If either is the
 * same as the greatest 'y' value crossing diection, then this
 * polygon circumscribes the north pole and must be processed.
 */
    pp = poly.first;
    ymax = -FLT_MAX;
    do {
	if ( ( pp->pt.y > ymax || pp->next->pt.y > ymax ) && 
		!ERMISS(pp->cross) )  {
	    ymax = G_MAX ( pp->pt.y, pp->next->pt.y );
	    pp_tmp = pp;
	}
	pp = pp->next;
    } while ( pp != poly.first );
    pp_nextX = pp_tmp;
    do {
	pp_nextX = pp_nextX->next;
    } while ( pp_nextX != pp_tmp && ERMISS(pp_nextX->cross) );
    pp_prevX = pp_tmp;
    do {
	pp_prevX = pp_prevX->prev;
    } while ( pp_prevX != pp_tmp && ERMISS(pp_prevX->cross) );

    tmp_dir = pp_tmp->next->roll - pp_tmp->roll;
    nxt_dir = pp_nextX->next->roll - pp_nextX->roll;
    prv_dir = pp_prevX->next->roll - pp_prevX->roll;

/*
 * Examine previous and next crossings' directions. If either is the
 * same as the greatest 'y' value crossing diection, then this
 * polygon circumscribes the north pole and must be processed.
 */
    if ( ( tmp_dir == nxt_dir || tmp_dir == prv_dir ) && ymax > 0.0F )  {

	pct = (pp_tmp->cross      - pp_tmp->pt.x) /
	      (pp_tmp->next->pt.x - pp_tmp->pt.x);
	mid_y = pp_tmp->pt.y + (pct)*(pp_tmp->next->pt.y-pp_tmp->pt.y);

/*
 * Create 4 new points: crossing value on polygon, pole at crossing, 
 * new crossing value (+TWOPI or -TWOPI depending on direction),
 * new crossing value on polygon.
 */
	G_MALLOC ( pp1, RANGEPOLYPOINT, np, "CGR_RANGE - pp1" );
	G_MALLOC ( pp2, RANGEPOLYPOINT, np, "CGR_RANGE - pp2" );
	G_MALLOC ( pp3, RANGEPOLYPOINT, np, "CGR_RANGE - pp3" );
	G_MALLOC ( pp4, RANGEPOLYPOINT, np, "CGR_RANGE - pp4" );
	pp1->next = pp2; pp2->prev = pp1;
	pp2->next = pp3; pp3->prev = pp2;
	pp3->next = pp4; pp4->prev = pp3;
	pp4->next = pp_tmp->next; pp_tmp->next->prev = pp4;
	pp_tmp->next = pp1; pp1->prev = pp_tmp;

	if ( tmp_dir < 0 )  {
	    pp1->pt.x = pp_tmp->cross;         pp1->pt.y = mid_y;
	    pp1->roll = pp_tmp->roll;
	    pp2->pt.x = pp_tmp->cross;         pp2->pt.y = HALFPI;
	    pp2->roll = pp_tmp->roll;
	    pp3->pt.x = pp_tmp->cross + TWOPI; pp3->pt.y = HALFPI;
	    pp3->roll = pp_tmp->roll;
	    pp4->pt.x = pp_tmp->cross + TWOPI; pp4->pt.y = mid_y;
	    pp4->roll = pp3->roll;
	    pp = pp4->next;
	    do {
	        pp->pt.x += TWOPI; pp->roll += 1; pp = pp->next;
	    } while ( pp != poly.first );
	}
	else  {
	    pp1->pt.x = pp_tmp->cross;         pp1->pt.y = mid_y;
	    pp1->roll = pp_tmp->roll;
	    pp2->pt.x = pp_tmp->cross;         pp2->pt.y = HALFPI;
	    pp2->roll = pp_tmp->roll;
	    pp3->pt.x = pp_tmp->cross - TWOPI; pp3->pt.y = HALFPI;
	    pp3->roll = pp_tmp->roll;
	    pp4->pt.x = pp_tmp->cross - TWOPI; pp4->pt.y = mid_y;
	    pp4->roll = pp3->roll;
	    pp = pp4->next;
	    do {
	        pp->pt.x -= TWOPI; pp->roll -= 1; pp = pp->next;
	    } while ( pp != poly.first );
	}

    }

    pp = poly.first;
    ii = 0;
    *minroll =  INT_MAX;
    *maxroll = -INT_MAX;
    do {
	lx[ii] = pp->pt.x; ly[ii] = pp->pt.y; r[ii] = pp->roll;
	*minroll = G_MIN ( *minroll, r[ii] );
	*maxroll = G_MAX ( *maxroll, r[ii] );
	ii += 1;
	pp = pp->next;
    } while ( pp != poly.first );
    *nout = ii;

/*
* Process southern hemisphere.
*
* Search for crossing with least 'y' value and examine
* previous and next crossings' directions. If either is the
* same as the least 'y' value crossing diection, then this
* polygon circumscribes the south pole and must be processed.
*/
    pp = poly.first;
    ymin = FLT_MAX;
    do {
	if ( ( pp->pt.y < ymin || pp->next->pt.y < ymin ) && 
		!ERMISS(pp->cross) )  {
	    ymin = G_MIN ( pp->pt.y, pp->next->pt.y );
	    pp_tmp = pp;
	}
	pp = pp->next;
    } while ( pp != poly.first );
    pp_nextX = pp_tmp;
    do {
	pp_nextX = pp_nextX->next;
    } while ( pp_nextX != pp_tmp && ERMISS(pp_nextX->cross) );
    pp_prevX = pp_tmp;
    do {
	pp_prevX = pp_prevX->prev;
    } while ( pp_prevX != pp_tmp && ERMISS(pp_prevX->cross) );

    tmp_dir = pp_tmp->next->roll - pp_tmp->roll;
    nxt_dir = pp_nextX->next->roll - pp_nextX->roll;
    prv_dir = pp_prevX->next->roll - pp_prevX->roll;

/*
 * Examine previous and next crossings' directions. If either is the
 * same as the greatest 'y' value crossing diection, then this
 * polygon circumscribes the north pole and must be processed.
 */
    if ( ( tmp_dir == nxt_dir || tmp_dir == prv_dir ) && ymin < 0.0F )  {

	pct = (pp_tmp->cross      - pp_tmp->pt.x) /
	      (pp_tmp->next->pt.x - pp_tmp->pt.x);
	mid_y = pp_tmp->pt.y + (pct)*(pp_tmp->next->pt.y-pp_tmp->pt.y);

/*
 * Create 4 new points: crossing value on polygon, pole at crossing, 
 * new crossing value (+TWOPI or -TWOPI depending on direction),
 * new crossing value on polygon.
 */
	G_MALLOC(pp1,RANGEPOLYPOINT, np,"CGR_RANGE - pp1");
	G_MALLOC(pp2,RANGEPOLYPOINT, np,"CGR_RANGE - pp2");
	G_MALLOC(pp3,RANGEPOLYPOINT, np,"CGR_RANGE - pp3");
	G_MALLOC(pp4,RANGEPOLYPOINT, np,"CGR_RANGE - pp4");
	pp1->next = pp2; pp2->prev = pp1;
	pp2->next = pp3; pp3->prev = pp2;
	pp3->next = pp4; pp4->prev = pp3;
	pp4->next = pp_tmp->next; pp_tmp->next->prev = pp4;
	pp_tmp->next = pp1; pp1->prev = pp_tmp;

	if ( tmp_dir < 0 )  {
	    pp1->pt.x = pp_tmp->cross;         pp1->pt.y = mid_y;
	    pp1->roll = pp_tmp->roll;
	    pp2->pt.x = pp_tmp->cross;         pp2->pt.y = -HALFPI;
	    pp2->roll = pp_tmp->roll;
	    pp3->pt.x = pp_tmp->cross + TWOPI; pp3->pt.y = -HALFPI;
	    pp3->roll = pp_tmp->roll;
	    pp4->pt.x = pp_tmp->cross + TWOPI; pp4->pt.y = pp_tmp->pt.y;
	    pp4->pt.x = pp_tmp->cross + TWOPI; pp4->pt.y = mid_y;
	    pp4->roll = pp3->roll;
	    pp = pp4->next;
	    do {
	        pp->pt.x += TWOPI; pp->roll += 1; pp = pp->next;
	    } while ( pp != poly.first );
	}
	else  {
	    pp1->pt.x = pp_tmp->cross;         pp1->pt.y = mid_y;
	    pp1->roll = pp_tmp->roll;
	    pp2->pt.x = pp_tmp->cross;         pp2->pt.y = -HALFPI;
	    pp2->roll = pp_tmp->roll;
	    pp3->pt.x = pp_tmp->cross - TWOPI; pp3->pt.y = -HALFPI;
	    pp3->roll = pp_tmp->roll;
	    pp4->pt.x = pp_tmp->cross - TWOPI; pp4->pt.y = pp_tmp->pt.y;
	    pp4->pt.x = pp_tmp->cross - TWOPI; pp4->pt.y = mid_y;
	    pp4->roll = pp3->roll;
	    pp = pp4->next;
	    do {
	        pp->pt.x -= TWOPI; pp->roll -= 1; pp = pp->next;
	    } while ( pp != poly.first );
	}

    }

/*
 * Save polygon to returned 'L' coordinate arrays.
 */
    pp = poly.first;
    ii = 0;
    *minroll =  INT_MAX;
    *maxroll = -INT_MAX;
    do {
	lx[ii] = pp->pt.x; ly[ii] = pp->pt.y; r[ii] = pp->roll;
	*minroll = G_MIN ( *minroll, r[ii] );
	*maxroll = G_MAX ( *maxroll, r[ii] );
	ii += 1;
	pp = pp->next;
    } while ( pp != poly.first );
    *nout = ii;

/*
 * Free temporary polygon memory.
 */
    pp = poly.first->next;
    do { 
	pp = pp->next;
	free ( pp->prev );
	} while ( pp != poly.first );
	free(pp);
}
