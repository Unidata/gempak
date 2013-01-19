#include "geminc.h"
#include "gemprm.h"


void cpcg_newpoly ( int *np1, float xp1[], float yp1[], int *np2, 
			float xp2[], float yp2[], int *np3, 
			float xp3[], float yp3[], int *iret )
/************************************************************************
 * cpcg_newpoly								*
 *									*
 * This function accepts two polygons(one open, one close) as a 	*
 * sequence of points, and builds a new close polygon. The two input   	*
 * polygons intersect with two intersected points, and are in the same	*
 * coordinate system.							* 
 *									*
 * cpcg_newpoly(np1, xp1, yp1, np2, xp2, yp2, np3, xp3, yp3, iret)	* 
 *									*
 * Input parameters:							*
 *	*np1	int	Number of vertices in polygon #1 (open)		*
 *	xp1[]	float	X-Coordinates for polygon #1			*
 *	yp1[]	float	Y-Coordinates for polygon #1			*
 *	*np2	int	Number of vertices in polygon #2 (close)	*
 *	xp2[]	float	X-Coordinates for polygon #2			*
 *	yp2[]	float	Y-Coordinates for polygon #2			*
 *									*
 * Output parameters:							*
 *	*np3    int     Number of vertices in new polygon               *
 *      xp3[]   float   X-Coordinates for new polygon                   *
 *      yp3[]   float   Y-Coordinates for new polygon			*
 *	*iret	int	Return code					*
 *									*
 **									*
 * Log:									*
 * M. Li/SAIC		08/01	Created					*
 * M. Li/SAIC		09/01	Increased intrsct from 2 to LLMXPT	*
 * m.gamazaychikov/SAIC  9/02	change calling sequence to clo_reorder	*
  *				add calculation of nx2, ny2 arrays	*
 ***********************************************************************/
{
int  	ii, na, nc, np, intrsct, nv, ier; 
int	bpnt1[LLMXPT], apnt1[LLMXPT], bpnt2[LLMXPT], apnt2[LLMXPT]; 
float	xs1[LLMXPT], ys1[LLMXPT], nx2[LLMXPT], ny2[LLMXPT];
int     indx[LLMXPT];
int	npp2;
/*--------------------------------------------------------------------*/

    *iret = 0;
    *np3 = 0;

/*
 * Get the intersection points
 */
    intrsct = LLMXPT;

    npp2 = *np2;
    clo_reorder(npp2, xp2, yp2, indx, &ier);

        for (ii = 0; ii < npp2; ii++) {
            nx2[ii] = xp2[indx[ii]];
            ny2[ii] = yp2[indx[ii]];
        }

    cgr_intersect ( sys_M, np1, xp1, yp1, sys_M, np2, nx2, ny2,
                        &intrsct, sys_M, &nv, xs1, ys1, bpnt1, apnt1, 
                        bpnt2, apnt2, &ier );

    if (nv < 2) {
	*iret = -5;
	return;
    }
/*
 * if ( bpnt1[0] == bpnt1[1] && apnt1[0] == apnt1[1] ), re-compute the intersections
 */
    if ( bpnt1[0] == bpnt1[1] && apnt1[0] == apnt1[1] ) {
	*np1 = *np1 + 1;
	xp1[bpnt1[0]+1] = (xs1[0] + xs1[1]) / 2;
        yp1[bpnt1[0]+1] = (ys1[0] + ys1[1]) / 2;
	for (ii = *np1-1; ii > bpnt1[0]+1; ii--) {
		xp1[ii] = xp1[ii-1];
                yp1[ii] = yp1[ii-1];
	}

	cgr_intersect ( sys_M, np1, xp1, yp1, sys_M, np2, nx2, ny2,
                        &intrsct, sys_M, &nv, xs1, ys1, bpnt1, apnt1,
                        bpnt2, apnt2, &ier );
    }
/*
 * if more then two intersections, get the first and the last 
 */
    if ( nv > 2 ) {
	xs1[1] = xs1[nv-1];
	ys1[1] = ys1[nv-1];
	bpnt1[1] = bpnt1[nv-1];
	apnt1[1] = apnt1[nv-1];
	bpnt2[1] = bpnt2[nv-1];
	apnt2[1] = apnt2[nv-1];
    }
/*
 * Get points between intersection points in polygon 1
 */
    xp3[0] = xs1[0];
    yp3[0] = ys1[0];
    np = 0;

    if ( G_DIFF(xs1[0], xp1[apnt1[0]]) && G_DIFF(ys1[0], yp1[apnt1[0]]) ) {
	na = apnt1[0] + 1;
    }
    else {
	na = apnt1[0];
    }
     
    for (ii = na; ii <= bpnt1[1]; ii++) {
	np++;
	xp3[np] = xp1[ii];
    	yp3[np] = yp1[ii];
    }
 
    if ( !G_DIFF(xs1[1], xp1[bpnt1[1]]) || !G_DIFF(ys1[1], yp1[bpnt1[1]]) ) {
	np++;
        xp3[np] = xs1[1]; 
        yp3[np] = ys1[1];
    }
/*
 *  Get points from polygon2 
 */
    if ( G_DIFF(xs1[1], nx2[apnt2[1]]) && G_DIFF(ys1[1], ny2[apnt2[1]]) ) {
    	nc = apnt2[1] + 1;
    }
    else {
    	nc = apnt2[1];
    }

    if (apnt2[1] < bpnt2[0]) { 
    	for (ii = nc; ii <= bpnt2[0]; ii++) {
    	    np++;
    	    xp3[np] = nx2[ii];
    	    yp3[np] = ny2[ii];
    	}
    }
    else {
    	for (ii = nc; ii < *np2; ii++) {
    	    np++;
    	    xp3[np] = nx2[ii];
    	    yp3[np] = ny2[ii];
    	}

    	for (ii = 0; ii <= bpnt2[0]; ii++) {
    	    np++;
    	    xp3[np] = nx2[ii];
    	    yp3[np] = ny2[ii];
    	}
    }

    if ( G_DIFF(xp3[np], xp3[np-1]) && G_DIFF(yp3[np], yp3[np-1]) ) {
    	    np--;
    }    
 

    if (np > 0) {
	*np3 = np + 1;
    }
    else {
	*np3 = 0;
	*iret = -5;
    }
} 
