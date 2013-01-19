#include "geminc.h"
#include "gemprm.h"

#define COORD_SYS_LEN 	8

void cgr_linepolyint( const char *sysLine, const int nptsLine, 
		      const float *xLine, const float *yLine, 
		      const char *sysPoly, const int nptsPoly,
		      const float *xPoly, const float *yPoly, 
		      Boolean *intersct, int *iret )
/************************************************************************
 * cgr_linepolyint                                                      *
 *                                                                      *
 * This function determines if a line(one or more segments) intersects 	*
 * a polygon. The polygon must be closed, which means the array size of *
 * xPoly and yPoly is ntpsPoly+1, and (xPoly[0], yPoly[0]) and		* 
 * (xPoly[nptsPoly], yPoly[nptsPoly]) are the same point.		*
 *                                                                      *
 * cgr_linepolyint ( sysLine, nptsLine, xLine, yLine, sysPoly, 		*
 *		     nptsPoly, xPoly, yPoly, intersct, iret  		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*sysLine	char		Coordinate system for the line	*
 *      *nptsLine       int             Number of points in the line    *
 *      *xLine          float           X coordinates of the line       *
 *      *yLine          float           Y coordinates of the line       *
 *	*sysPoly	char		Coordinate system for polygon	*
 *      *nptsPoly       int             Number of points in polygon     *
 *      *xPoly          float           X coordinates of the polygon    *
 *      *yPoly          float           Y coordinates of the polygon    *
 *                                                                      *
 * Output parameters:                                                   *
 *	*intersct	Boolean		Whether line intersects poloygon*
 *      *iret   	int    	 	Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          12/05   Created                                 *
 * B. Yin/SAIC          10/08   Fix a bug in polygon's last segment     *
 ***********************************************************************/
{
    int 	ii, jj, intrsct, ier;

    float	xInt, yInt;
    /*
     *  Those local vaariables are used because cgr_segint() needs
     *  non-constant variables.
     */
    float	xLinePt[2], yLinePt[2], xPolyPt[2], yPolyPt[2];
    char	sLine[ COORD_SYS_LEN ], sPoly[ COORD_SYS_LEN ];
/*---------------------------------------------------------------------*/

    *iret 	= 0;
    *intersct 	= False;
    strcpy( sLine, sysLine );
    strcpy( sPoly, sysPoly );

    for ( ii = 0; ii < nptsPoly - 1 && *intersct == False; ii++ ) {

        xPolyPt[ 0 ] = xPoly[ ii ];
        yPolyPt[ 0 ] = yPoly[ ii ];
        xPolyPt[ 1 ] = xPoly[ ii + 1 ];
        yPolyPt[ 1 ] = yPoly[ ii + 1 ];

	for ( jj = 0; jj < nptsLine - 1; jj++ ) {

	    xLinePt[ 0 ] = xLine[ jj ];
	    yLinePt[ 0 ] = yLine[ jj ];
	    xLinePt[ 1 ] = xLine[ jj + 1 ];
	    yLinePt[ 1 ] = yLine[ jj + 1 ];

	    cgr_segint ( sLine, xLinePt, yLinePt, sPoly, 
	 	         xPolyPt, yPolyPt, sys_M, &xInt, &yInt,   
	                 &intrsct, &ier ); 

 	    if ( intrsct == 1 ) {

	       *intersct = True;
	       break;

	    }
	}
    }
}

