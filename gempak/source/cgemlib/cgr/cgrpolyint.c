#include "geminc.h"
#include "gemprm.h"

#ifndef FLT_MAX
#define FLT_MAX 1E+37
#endif

void cgr_polyint ( char *sysp1, int *np1, float *xp1, float *yp1, char *sysp2, 
		 int *np2, float *xp2, float *yp2, int *intrsct, int *iret )
/************************************************************************
 * cgr_polyint								*
 *									*
 * This function accepts two CLOSED polygons as a sequence of points    *
 * and tests to see if they intersect. The test simply checks to see if	*
 * any points from polygon #1 are inside polygon #2.  If true, return;	*
 * if false, check to see if any points from polygon #2 are inside	*
 * polygon #1.  If true, return; if still false, perform a more		*
 * rigorous check of segment by segment intersection.			*
 *									*
 * cgr_polyint ( sysp1, np1, xp1, yp1, sysp2, np2, xp2, yp2, 		*
 *		 intrsct, iret )					*
 *									*
 * Input parameters:							*
 *	*sysp1	char	Coordinate system for polygon #1		*
 *	*np1	int	Number of vertices in polygon #1		*
 *	*xp1	float	X-Coordinates for polygon #1			*
 *	*yp1	float	Y-Coordinates for polygon #1			*
 *	*sysp2	char	Coordinate system for polygon #2		*
 *	*np2	int	Number of vertices in polygon #2		*
 *	*xp2	float	X-Coordinates for polygon #2			*
 *	*yp2	float	Y-Coordinates for polygon #2
 *									*
 * Output parameters:							*
 * 	*intrsct int	Result: 0-FALSE, 1-TRUE				*
 *	*iret	 int	Return code					*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/97	Created					*
 * F.J.Yen/NCEP		 8/97	Correct indexing problem in call to	*
 *				cgr_segint.				*
 * D.W.Plummer/NCEP	11/97	Added coordinate systems		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * T. Piper/GSC		10/98	Corrected prolog (added ** prior to Log)*
 * D.W.Plummer/NCEP	 9/99	Changed cgr_segint calling sequence	*
 ***********************************************************************/
{
int 	i, j, np;
int	*inout;
float	xmn1, xmx1, ymn1, ymx1;
float	xmn2, xmx2, ymn2, ymx2;
float	xint, yint;
/*---------------------------------------------------------------------*/

    *iret = 0;
    *intrsct = 0;

    /*
     *  First compute extrema of polygons and check for trivial rejection
     */

    xmn1 =  FLT_MAX;
    xmx1 = -FLT_MAX;
    ymn1 =  FLT_MAX;
    ymx1 = -FLT_MAX;
    for ( i = 0; i < *np1; i++ ) {
	xmn1 = G_MIN ( xmn1, xp1[i] );
	xmx1 = G_MAX ( xmx1, xp1[i] );
	ymn1 = G_MIN ( ymn1, yp1[i] );
	ymx1 = G_MAX ( ymx1, yp1[i] );
    }
    xmn2 =  FLT_MAX;
    xmx2 = -FLT_MAX;
    ymn2 =  FLT_MAX;
    ymx2 = -FLT_MAX;
    for ( j = 0; j < *np2; j++ ) {
	xmn2 = G_MIN ( xmn2, xp2[j] );
	xmx2 = G_MAX ( xmx2, xp2[j] );
	ymn2 = G_MIN ( ymn2, yp2[j] );
	ymx2 = G_MAX ( ymx2, yp2[j] );
    }

    if ( xmn1 > xmx2 || xmx1 < xmn2 )  return;
    if ( ymn1 > ymx2 || ymx1 < ymn2 )  return;

    /*
     *  Check the first polygon's points against the second.
     */

    np = G_MAX( *np1, *np2);
    inout = (int *)malloc( np * sizeof(int) );

    cgr_inpoly( sysp1, np1, xp1, yp1, sysp2, np2, xp2, yp2,
		inout, iret );

    i = 0;
    while ( i < *np1 && *intrsct != 1 ) {
	if ( inout[i] == 1 )  *intrsct = 1;
	i++;
    }

    /*
     *  Now check the second polygon's points against the first.
     */

    if ( *intrsct != 1 )
    	cgr_inpoly( sysp2, np2, xp2, yp2, sysp1, np1, xp1, yp1,
		    inout, iret );

    i = 0;
    while ( i < *np2 && *intrsct != 1 ) {
	if ( inout[i] == 1 )  *intrsct = 1;
	i++;
    }

    free ( inout );

    if ( *intrsct == 1 )  return;

    /*
     *  Finally check for individual segment intersection.
     */

    for ( i = 0; i < (*np1)-1; i++ ) {
    	for ( j = 0; j < (*np2)-1; j++ ) {
	    cgr_segint( sysp1, &(xp1[i]), &(yp1[i]), 
			sysp2, &(xp2[j]), &(yp2[j]), sysp1,
			&xint, &yint, intrsct, iret );
	    if ( *intrsct == 1 )  return;
    	}
    }

}
