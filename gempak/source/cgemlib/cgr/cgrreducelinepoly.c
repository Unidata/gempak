#include "geminc.h"
#include "gemprm.h"

/*
 *  Private functions
 */
static Boolean _checkTolerance ( int lastPoint, int curPoint, float *xin, 
		float *yin, float tolerance, int *iret );

/*
 *  Public functions
 */
void cgr_reduceLinePoly ( int nin, float *xin, 
		float *yin, float tolerance, int *nout, float *xout, 
		float *yout, int *iret )
/************************************************************************
 * cgr_reduceLinePoly							*
 *                                                                      *
 * This function takes as input the points to a line or polygon in     	*
 * device coordinates and a tolerance factor.  A KevLinDev reduction    *
 * algorithm is applied to the line/polygon to potentially reduce the   *
 * number of points.  There is no explicit number of points to be       *
 * reduced to -- rather this algoithm identifies interim points that    *
 * are within the tolerance value of a straight line and excludes them  *
 * from the xout/yout arrays.                				*
 *                                                                      *
 * The algorithm can be briefly described as follows:                   *
 *									*
 * define lastPoint to be the first point in the input point array  	*
 *   loop through the remaining list of points, assigning each to       *
 *           currentPoint to form a line from lastPoint to currentPoint	*
 *     test all points between, but not including lastPoint             *
 *                                                and currentPoint      *
 *       if the test point's distance from the line is more than the    *
 *							tolerance then  *
 *         add the point before currentPoint to the result array	*
 *         set lastPoint equal to the added point			*
 *         proceed to the next point in the top-most loop		*
 *       end if								*
 *     end test							 	*
 *   end loop								*
 * add the last point in the input point array to the output array	*
 * 									*
 * A more complete discussion of the algorithm can be found at:		*
 * http://www.kevlindev.com/tutorials/geometry/simplify_polyline/page2.htm *
 *									*
 * The algorithm is taken from the above url, which is copyright 2000-3 *
 * Kevin Lindsey.  This implementation of the algorithm is orginal work *
 * based on Mr. Lindsey's algorithm.					*
 * 									*
 * 									*
 * Note: it is the caller's responsibilty to make sure the size of the	*
 *       output array is no less than the size of the input array.	*
 *                                                                      *
 *                                                                      *
 * cgr_reduceLinePoly ( nin, xin, yin, tolerance, nout, xout, yout,     *
 * 		   iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *   nin    	int     number of input points				*
 *  *xin  	float	X-coordinates of input points			*
 *  *yin  	float	Y-coordinates of input points			*
 *  tolerance 	float   tolerance value, must be > 0.0F               	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *nout    	int     number of output points				*
 *  *xout  	float	X-coordinates of output points			*
 *  *yout  	float	Y-coordinates of output points			*
 *  *iret    	int    	return code					*
 *                      	 1 - nin >= 2, no reduction possible    *
 *                      	-3 - bad tolerance value (<0.0F)	*
 *				-4 - bad nin value (<1)			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	04/06	initial coding				*
 ***********************************************************************/
{
    int    	ii, lastPoint, curPoint, ier;
    Boolean	lineOk;
/*---------------------------------------------------------------------*/

    *iret = 0;
    *nout = 0;

    /*
     *  Verify inputs.
     */
    if ( tolerance < 0.0F ) {			/* bad tolerance value */
        *iret = -3;
    }
    else if( nin < 1 ) {			/* bad nin value */
	*iret = -4;
    }
    else if( nin <= 2 ) {			/* nin <= 2 */
	for( ii=0; ii< nin; ii++ ) {
	    xout[ii] = xin[ii];
	    yout[ii] = yin[ii];
	}
	*nout = nin;
	*iret = 1;
    }

    if( *iret != 0 ) return;


    /*
     *  Add first point to output.
     */
    xout[ 0 ] = xin[ 0 ];
    yout[ 0 ] = yin[ 0 ];
    *nout    = 1;

    lastPoint = 0;
    curPoint  = 2;

    while( curPoint < nin ) {

        lineOk = _checkTolerance( lastPoint, curPoint, xin, yin,  
				tolerance, &ier );
        if( !lineOk ) {
            xout[ *nout ] = xin[ curPoint - 1 ];
	    yout[ *nout ] = yin[ curPoint - 1 ];
	    *nout = *nout + 1;
	    lastPoint = curPoint - 1;
	}

	curPoint++;
    }


    /*
     *  Add last point to output.
     */     
    xout[ *nout ] = xin[ nin-1 ];
    yout[ *nout ] = yin[ nin-1 ];
    *nout = *nout + 1;
}

/*=====================================================================*/

static Boolean _checkTolerance ( int lastPoint, int curPoint, float *xin, 
		float *yin, float tolerance, int *iret )
/************************************************************************
 * _checkTolerance							*
 *                                                                      *
 *                                                                      *
 * _checkTolerance( lastPoint, curPoint, xin, yin, tolerance, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *  lastPoint	int                                             	*
 *  curPoint	int                            				*
 *  *xin  	float	X-coordinates of input points			*
 *  *yin  	float	Y-coordinates of input points			*
 *  tolerance   float							*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret    	int    	return code					*
 *                                                                      *
 * Return parameters:                                                   *
 *              Boolean		True if all points are within tolerance *
 *				 of a line between the end points	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	04/06	initial coding				*
 ***********************************************************************/
{
    float	x1, y1, x2, y2, fx, fy;
    float	px, py, dist;
    int		ii, ier;
    Boolean	withinTolerance = True;
/*---------------------------------------------------------------------*/

    *iret = 0;

    x1 = xin[ lastPoint ];
    y1 = yin[ lastPoint ];
    x2 = xin[ curPoint ];
    y2 = yin[ curPoint ];

    for( ii=lastPoint; ii<curPoint && withinTolerance; ii++ ) {
	fx = xin[ ii ];
	fy = yin[ ii ];

        cgr_lindist( x1, y1, x2, y2, fx, fy, &px, &py, &dist, &ier );

	if( dist > tolerance ) {
	    withinTolerance = False;
	}
    }

    return( withinTolerance ); 
}

/*=====================================================================*/
