/************************************************************************
 * cgrcmn.h								*
 * Contains structures and definitions for cartesian coordinate 	*
 * geometry figures within the cgr library.				*
 *									*
 **									*
 * D.W.Plummer/NCEP	11/03						*
 * D.W.Plummer/NCEP	 2/04	Changes for polygon interpolation	*
 * D.W.Plummer/NCEP	 5/07	Add MPT_ORIGINAL, MPT_MAPPING and 'type'*
 * 				in mpolypoint structure			*
 ***********************************************************************/
#ifndef _cgrcmn_include
#define _cgrcmn_include


#define NEW(X,N)        ( (X*) malloc  ( (N) * sizeof(X) ) )
#define RENEW(A,X,N)    ( (X*) realloc ( (A), (N) * sizeof(X) ) )

/*
 * Mapping point type
 */
#define	MPT_ORIGINAL	0
#define	MPT_MAPPING	1

struct point {
    double               x;              /* point x coord                */
    double               y;              /* point y coord                */
};
#define POINT   	struct point
#define NULL_POINT   	(POINT *)NULL


struct segment {
    POINT       *from;                  /* segment 'from' POINT         */
    POINT       *to;                    /* segment 'to' POINT           */
};
#define SEGMENT         struct segment


struct polypoint {
    POINT               *pt;            /* POINT                        */
    struct polypoint    *next;          /* ptrs to next pts in poly 1&2 */
    struct polypoint    *prev;          /* ptrs to next pts in poly 1&2 */
};
#define POLYPOINT       struct polypoint


struct polygon {
    POLYPOINT  	*first;                 /* ptr to first POLYGON point   */
};
#define POLYGON         struct polygon
#define NULL_POLYPOINT  (POLYPOINT *)NULL

struct mpolypoint {
        POINT               *pt;            /* ptr to point coordinates     */
	int		    type;	    /* type of point		    */
	struct mpolypoint   *next;          /* ptr to next pt               */
	struct mpolypoint   *prev;          /* ptr to prev pt               */
	struct mpolypoint   *map;           /* ptr to mapping points        */
};
#define MPOLYPOINT      struct mpolypoint
#define NULL_MPOLYPOINT ( (MPOLYPOINT   *)NULL )

struct mappolygon {
        MPOLYPOINT          *first;         /* ptr to first polygon point   */
};
#define MPOLYGON                struct mappolygon

struct mappingpt {
        struct mappingpt    *next;          /* ptr to next pt               */
	struct mappingpt    *prev;          /* ptr to prev pt               */
	POINT               *pt;            /* ptr to point                 */
};
#define MAPPINGPT       struct mappingpt
#define NULL_MAPPINGPT  ( (MAPPINGPT    *)NULL )



extern	int	ncgrPoints;		/* Number of CGR points		*/
extern	int	maxcgrPoints;		/* Max number of CGR points	*/
extern	POINT	**cgrPoints;		/* Array of CGR points		*/


extern	int	nBytesAlloc, nBytesFree;


/************************************************************************/


#define IN              ( (char) 1 )
#define OUT             ( (char) 0 )

#define UNION           OUT
#define INTERSECT       IN

#define	NFAC		5		/* discrete measure		*/

#define	SCALE		(1 << NFAC)	/* scale factor			*/

#define	TO_INT(X)	( (int  )((X) *SCALE))	/* scale		*/
#define	TO_FLT(X)	(((float) (X))/SCALE)	/* unscale		*/

#define POLYPOINT_DIST(P0,P1)  ( G_DIST ( (P0->pt->x), (P0->pt->y), (P1->pt->x), (P1->pt->y)) )				/* polypoint distance		*/

#define MPOLYPOINT_DIST(P1,P2)  ( G_DIST ( (P1->pt->x), (P1->pt->y), (P2->pt->x), (P2->pt->y)) )			/* mapping polypoint distance	*/

#define	POLY0		0		/* polygon 0			*/
#define	POLY1		1		/* polygon 1			*/

#define	CGR_EPS		0.05F		/* epsilon			*/

#define	MAX_CGR_PTS	1000		/* Maximum number of CGR points	*/


#include "proto_cgr.h"


#endif
