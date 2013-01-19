#include "geminc.h"
#include "gemprm.h"

#define MAXOUT		50000


void cgr_insert ( float *px, float *py, int np, 
                  float *qx, float *qy, int nq, 
		  float dens, float crvscl,
                  float *tx, float *ty, int *nt, 
                  int *widx, int *iret )
/************************************************************************
 * cgr_insert                                                           *
 *                                                                      *
 * This function inserts a sequence of points (qx, qy) into another     *
 * sequence of points (px, py). To do this, it performs parametric	*
 * curve fit to (px, py), and finds on the curve the nearest point	*
 * for each point in (qx, qy) as the inserted point. For overlaped	*
 * points, they will be duplicated in the output array.			*
 *									*
 * cgr_insert ( px, py, np, qx, qy, nq, tx, ty, nt, widx, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *px           	float	x coordinates of points			*
 *      *py           	float	y coordinates of points			*
 *	np		int	number of points			*
 *      *qx           	float	x coordinates of points			*
 *      *qy           	float	y coordinates of points			*
 *	nq		int	number of points			*
 *	dens		float	Density of intermediate points		*
 * 	crvscl		float	Device curve scaling factor		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *tx           	float	x coordinates of points			*
 *      *ty           	float	y coordinates of points			*
 *	*nt		int	number of points			*
 *	*widx		int	index of (qx, qy) in (tx, ty)		*
 *      *iret           int     Return code                    	 	*
 *				   0 =  Normal				*
 *				  -1 = Error				*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 5/03						*
 * R. Tian/SAIC		 6/03	Duplicate the overlaped points		*
 * R. Tian/SAIC		 6/03	Correctly order the duplicated points	*
 ***********************************************************************/
{
    float xcv[MAXOUT], ycv[MAXOUT];
    char marker[MAXOUT];
    int istrt, iend, maxpts, nout;
    float distance;
    int nj, nw, nearest;
    int i, ier;

/*---------------------------------------------------------------------*/
    *iret = 0;

    if(*nt < np + nq) {
	*iret = -1;
	return;
    }

    /*
     * Perform parametric curve fit.
     */
    istrt = 0;
    iend = MAXOUT;
    maxpts = MAXOUT;
    cv_prmt ( &np, px, py, &dens, &maxpts, &crvscl, &istrt, &iend, &nout,
              xcv, ycv, &ier );
    if ( ier != 0 ) {
	*iret = -1;
	return;
    }

    /*
     * Find the nearest point in (xcv, ycv) for each point in sequence 
     * (px, py) and each point in sequence (qx, qy). 
     */
    for ( i = 0; i < nout; i++ ) {
	marker[i] = '\0';
    }
    for ( i = 0; i < np; i++ ) {
	cgr_dist ( nout, xcv, ycv, px[i], py[i], &distance,
                   &nearest, &ier );
	marker[nearest] = 'J';
    }
    for ( i = 0; i < nq; i++ ) {
	cgr_dist ( nout, xcv, ycv, qx[i], qy[i], &distance,
                   &nearest, &ier );
	if ( marker[nearest] == 'J' ) {
	    marker[nearest] = 'D';
	}
	else {
	    marker[nearest] = 'W';
	}
	widx[i] = nearest;
    }

    /*
     * Write the marked points in output array.
     */
    for ( i = nj = 0; i < nout; i++ ) {
	if ( marker[i] == 'J' ) {
	    /*
	     * Jets points.
	     */
	    tx[nj] = xcv[i];
	    ty[nj] = ycv[i];
	    nj++;
	}
	else if ( marker[i] == 'W' ) {
	    /*
	     * Wind barbs and hash marks points.
	     */
	    tx[nj] = xcv[i];
	    ty[nj] = ycv[i];
	    for ( nw = 0; nw < nq; nw++ ) {
		if ( widx[nw] == i ) {
		    widx[nw] = nj;
		}
	    }
	    nj++;
	}
	else if ( marker[i] == 'D' ) {
	    /*
	     * Overlaped points (both jets and wind barbs points)
	     */
	    if ( i == 0 ) {
	    	tx[nj] = xcv[i];
	    	ty[nj] = ycv[i];
	    	nj++;

	    	tx[nj] = xcv[i];
	    	ty[nj] = ycv[i];
	    	for ( nw = 0; nw < nq; nw++ ) {
		    if ( widx[nw] == i ) {
		    	widx[nw] = nj;
		    }
	    	}
	    	nj++;
	    }
	    else if ( i ==  nout - 1 ) {
	    	tx[nj] = xcv[i];
	    	ty[nj] = ycv[i];
	    	for ( nw = 0; nw < nq; nw++ ) {
		    if ( widx[nw] == i ) {
		    	widx[nw] = nj;
		    }
	    	}
	    	nj++;

	    	tx[nj] = xcv[i];
	    	ty[nj] = ycv[i];
	    	nj++;
	    }
	    else {
	    	for ( nw = 0; nw < nq; nw++ ) {
		    if ( widx[nw] == i ) {
			if ( qx[nw] < xcv[i] || qy[nw] < ycv[i] ) {
	    		    tx[nj] = xcv[i];
	    		    ty[nj] = ycv[i];
		    	    widx[nw] = nj;
	    		    nj++;

	    		    tx[nj] = xcv[i];
	    		    ty[nj] = ycv[i];
	    		    nj++;
			}
			else {
	    		    tx[nj] = xcv[i];
	    		    ty[nj] = ycv[i];
	    		    nj++;

	    		    tx[nj] = xcv[i];
	    		    ty[nj] = ycv[i];
		    	    widx[nw] = nj;
	    		    nj++;
			}
		    }
	    	}
	    }
	}
    }
    *nt = nj;

    return;
}
