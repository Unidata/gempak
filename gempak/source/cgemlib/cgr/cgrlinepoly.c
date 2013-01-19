#include "geminc.h"
#include "gemprm.h"

#define	SMALLF		.0001

void cgr_linepoly ( int nptsLine, float *latLine, float *lonLine, 
		    int nptsPoly, float *latPoly,  float *lonPoly, 
		    int *nout, float **xout, float **yout, Boolean **inout, 
		    int *iret )
/************************************************************************
 * cgr_linepoly								*
 *									*
 * This routine clips a line against a single polygon and returns the 	*
 * number of points, point coordinates and an array which indicates 	*
 * whether those point are in or out of the polygon. The polygon must 	*
 * be closed. 								*
 *									*
 * All input coordates must be in map coordinates (sys_M).  Output 	* 
 * points will also be in map coordinates.				*
 *									*
 * Note: the three output arrays are allocated in this routine, 	*
 *       the caller need free the memory.				*
 *                                                                      *
 * cgr_linepoly ( nptsLine, xLine, yLine, nptsPoly,  			*
 *		  xPoly, yPoly, nout, xout, yout, inout, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *  nptsLine	int	number of points in the line			*
 *  *latLine	float	array of lat coords in the line			*
 *  *lonLine	float	array of lon coords in the line			*
 *  nptsPoly	int	number of points in the polygon			*
 *  *latPoly	float	array of lat coords in the polygon     		*
 *  *lonPoly	float	array of lon coords in the polygon     		*
 *                                                                      *
 * Output parameters:                                                   *
 *  *nout	int	number of returned points			*
 *  **xout	float	array of returned x points			*
 *  **yout	float	array of returned y points			*
 *  **inout	Boolean	array of indicators as to in or out		*
 *  *iret	int	Return code 	0: normal			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		1/06	Modified from clip_line in clipvgf.c	*
 * E. Safford/SAIC	09/06	rm coord system inputs, assume sys_M, 	*
 *				  use cgr_inpolywn not cgr_inpoly	*
 * E. Safford/SAIC	10/06	fix initialization of intesect pts issue*
 ***********************************************************************/
{
    int		ii, jj, kk, ll, ier;
    int		nIntersect = 0, intersectFlag, *pointInout, maxOutPts;
    int		checkAll = 1;

    float	xIntersect, yIntersect, temp;
    float	*xPtsIntersect, *yPtsIntersect, *dist = NULL;
    float	*latPtsIntersect = NULL, *lonPtsIntersect = NULL;

    Boolean	repeatPt, vertex;
/*---------------------------------------------------------------------*/

    *iret = 0;


    G_MALLOC ( pointInout, int, nptsLine, "CGR_LINEPOLY pointInout" );

    /*
     *  Check whether points of the line are in the polygon.
     */
    cgr_inpolywn ( nptsLine, latLine, lonLine, nptsPoly, latPoly, lonPoly, 
		   checkAll, pointInout, &ier );

    *nout = 0;

    /*
     * The maximum points one line may intersect a polygon is (nptsPoly -1).
     * There are (nptsLine -1) lines. So the maximum number of output points 
     * is 2 * (nptsLine -1) * (nptsPoly -1)
     */
    maxOutPts = 2 * (nptsLine -1) * (nptsPoly -1);
    G_MALLOC ( *inout, Boolean,  maxOutPts, "CGR_LINEPOLY inout" );
    G_MALLOC ( *xout, float, maxOutPts, "CGR_LINEPOLY xout" );
    G_MALLOC ( *yout, float, maxOutPts, "CGR_LINEPOLY yout" );

    G_MALLOC ( xPtsIntersect, float, nptsPoly -1, "CGR_LINEPOLY xPtsIntersect" );
    G_MALLOC ( yPtsIntersect, float, nptsPoly -1, "CGR_LINEPOLY yPtsIntersect" );
    G_MALLOC ( dist, float, nptsPoly -1, "CGR_LINEPOLY dist" );

    /*
     * Loop over each segments of the line to check the intersection points.
     */
    for ( ii = 0; ii < nptsLine - 1; ii++ )  {

	(*xout)[ *nout ]  = latLine[ ii ];
	(*yout)[ *nout ]  = lonLine[ ii ];
	(*inout)[ *nout ] = ( pointInout[ ii ] == 1 )? True : False;

	(*nout)++;

	nIntersect = 0;

	/*
	 * Loop over each segments of the polygon.
	 */
	for ( jj = 0; jj < nptsPoly - 1; jj++ )  {

	    /*
	     * Check if the line segment intersects the polygon segment.
	     */
  	    cgr_segintwn ( &( latLine[ ii ] ), &( lonLine [ ii ] ), 
			 &( latPoly[ jj ] ), &( lonPoly [ jj ] ),
			 &xIntersect, &yIntersect, &intersectFlag, &ier );
            
	    /*
	     * If a line cross a vertex of the polygon, the vertex will
	     * appear twice as an intersection point. The repeated 
	     * points need to be removed.
	     */
	    if (  intersectFlag == G_TRUE )  {
		vertex = ( ( ( G_ABS ( xIntersect - latLine[ ii ] ) < SMALLF ) &&
			     ( G_ABS ( yIntersect - lonLine[ ii ] ) < SMALLF ) ) ||
			   ( ( G_ABS ( xIntersect - latLine[ ii + 1 ] ) < SMALLF ) &&  
			     ( G_ABS ( yIntersect - lonLine[ ii + 1 ] ) < SMALLF ) ) ) &&
			 ( ( ( G_ABS ( xIntersect - latPoly[ ii ] ) < SMALLF ) &&
			     ( G_ABS ( yIntersect - lonPoly[ ii ] ) < SMALLF ) ) ||
			   ( ( G_ABS ( xIntersect - latPoly[ ii + 1 ] ) < SMALLF ) &&  
			     ( G_ABS ( yIntersect - lonPoly[ ii + 1 ] ) < SMALLF ) ) );

		repeatPt = False;

		for ( kk = 0; kk < nIntersect; kk++ ) {

		    if ( G_ABS( xIntersect - xPtsIntersect[ kk ] ) < SMALLF &&
		         G_ABS( yIntersect - yPtsIntersect[ kk ] ) < SMALLF ) {

			 repeatPt = True;
			 break;

		    }
		}

		if ( !vertex && !repeatPt ) {
		   xPtsIntersect[ nIntersect ] = xIntersect;
		   yPtsIntersect[ nIntersect ] = yIntersect;
		   nIntersect++;
                   
		}

	    }

	}

	if ( nIntersect > 0 )  {

    	    G_MALLOC ( latPtsIntersect, float, nIntersect, "CGR_LINEPOLY latPtsIntersect" );
    	    G_MALLOC ( lonPtsIntersect, float, nIntersect, "CGR_LINEPOLY lonPtsIntersect" );

	    for( kk=0; kk<nIntersect; kk++ ) {
		latPtsIntersect[kk] = xPtsIntersect[ kk ];
		lonPtsIntersect[kk] = yPtsIntersect[ kk ];
            }

	    clo_dist ( &( latLine[ ii ] ), &( lonLine[ ii ] ), &nIntersect, 
	    		latPtsIntersect, lonPtsIntersect, dist, &ier );

	    G_FREE ( latPtsIntersect, float );
	    G_FREE ( lonPtsIntersect, float );

	    /*
	     *  Sort the distance
	     */
	    for ( kk = 0; kk < nIntersect - 1; kk++ )  {
		for ( ll = 0; ll < nIntersect - kk - 1; ll++ )  {

		    if ( dist[ ll ] > dist[ ll + 1 ] )  {

			temp = xPtsIntersect[ ll ];
			xPtsIntersect[ ll ] = xPtsIntersect[ ll + 1 ];
			xPtsIntersect[ ll + 1 ] = temp;

			temp = yPtsIntersect[ ll ];
			yPtsIntersect[ ll ] = yPtsIntersect[ ll + 1 ];
			yPtsIntersect[ ll + 1 ] = temp;

			temp = dist[ ll ];
			dist[ ll ] = dist[ ll + 1 ];
			dist[ ll + 1 ] = temp;

		    }
		}
	    }

	    /*
	     * Add the intersection points into the output arrays.
	     */
	    for ( kk = 0; kk < nIntersect; kk++ )  {

		(*xout)[ *nout ]  = xPtsIntersect[ kk ];
		(*yout)[ *nout ]  = yPtsIntersect[ kk ];
		(*inout)[ *nout ] = (*inout)[ *nout - 1 ];

		(*nout)++;

		(*xout)[ *nout ]  = xPtsIntersect[ kk ];
		(*yout)[ *nout ]  = yPtsIntersect[ kk ];
		(*inout)[ *nout ] = !( (*inout)[ *nout - 1 ] );

		(*nout)++;
	    }
	}
    }

    /* 
     * Add the last point of the line.
     */
    (*xout)[ *nout ]  = latLine[ nptsLine - 1 ];
    (*yout)[ *nout ]  = lonLine[ nptsLine - 1 ];
    (*inout)[ *nout ] = ( pointInout[ nptsLine - 1 ] == 1 ) ? True : False;

    (*nout)++;

    /*
     *  Reduce the size of output arrays.
     */
    G_REALLOC ( *inout, Boolean, *nout, "CGR_LINEPOLY realloc inout" );
    G_REALLOC ( *xout, float, *nout, "CGR_LINEPOLY realloc xout" );
    G_REALLOC ( *yout, float, *nout, "CGR_LINEPOLY realloc yout" );

    /*
     *  Free temporary memory
     */
    G_FREE ( pointInout, int );
    G_FREE ( xPtsIntersect, float );
    G_FREE ( yPtsIntersect, float );
    G_FREE ( dist, float );

}
