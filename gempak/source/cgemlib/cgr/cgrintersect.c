#include "geminc.h"
#include "gemprm.h"

void cgr_intersect ( 
char    *sysp1,
int     *np1, 
float   *xp1, 
float 	*yp1,
char 	*sysp2, 
int 	*np2, 
float 	*xp2, 
float 	*yp2, 
int 	*intrsct1,
char 	*sys3, 
int 	*intrsct2, 
float 	*xout, 
float 	*yout,
int 	*bpnt1, 
int 	*apnt1, 
int 	*bpnt2, 
int 	*apnt2, 
int 	*iret )
/************************************************************************
 * cgr_intersect                                                        *
 *                                                                      *
 * This function accepts two polygons as a sequence of points    	*
 * and return the intersection points.  				*
 *                                                                      *
 * cgr_intersect (sysp1, np1, xp1, yp1, sysp2, np2, xp2, yp2,           *
 *               intrsct1, sys3, intrsct2, xout, yout, bpnt1, apnt1, 	*
 *					bpnt2, apnt2, iret)		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *sysp1  char    Coordinate system for polygon #1                *
 *      *np1    int     Number of vertices in polygon #1                *
 *      *xp1    float   X-Coordinates for polygon #1                    *
 *      *yp1    float   Y-Coordinates for polygon #1                    *
 *      *sysp2  char    Coordinate system for polygon #2                *
 *      *np2    int     Number of vertices in polygon #2                *
 *      *xp2    float   X-Coordinates for polygon #2                    *
 *      *yp2    float   Y-Coordinates for polygon #2			*
 *	*intrsct1 int   expected intersection points			*
 *	*sys3	char	Coordinate system for the intersection points	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *intrsct2 int   returned intersection points			* 
 *	*xout	float	X-Coordinates for the intersection points	*
 *	*yout	float	Y-Coordinates for the intersection points	*	
 * 	*bpnt1	int	the point before the intersection in pologon1	*
 *	*apnt1  int     the point after the intersection in pologon1  	*	
 *      *bpnt2  int     the point before the intersection in pologon2   *       
 *      *apnt2  int     the point after the intersection in pologon2    * 
 *      *iret   int    	Return code                                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		07/01	Created                                 *
 * M. Li/SAIC		09/01	Set max. intersection points		*
 ***********************************************************************/
{
int     ii, jj, ier;
int     inout;
float   xint, yint;
float   x1[2], y1[2], x2[2], y2[2];
/*---------------------------------------------------------------------*/
    *iret = 0;
    *intrsct2 = 0;
    for (ii = 0; ii < *intrsct1; ii++ ) {
	xout[ii] = RMISSD;
	yout[ii] = RMISSD;
	bpnt1[ii] = -1;
	apnt1[ii] = -1;
	bpnt2[ii] = -1;
	apnt2[ii] = -1;
    }

    xint = RMISSD;
    yint = RMISSD;

    for (ii = 0; ii < *np1-1; ii++) {
    	x1[0] = xp1[ii];
        y1[0] = yp1[ii];
	x1[1] = xp1[ii+1];
        y1[1] = yp1[ii+1];

	for (jj = 0; jj < *np2-1; jj++) {
	    x2[0] = xp2[jj];
	    y2[0] = yp2[jj];
	    x2[1] = xp2[jj+1];
	    y2[1] = yp2[jj+1];
 
	    cgr_segint(sysp1, x1, y1, sysp2, x2, y2, sys3, &xint, &yint, &inout, &ier);

	    if ( inout == 1 ) {
		if ( *intrsct2 == 0 || !G_DIFF (xint, xout[*intrsct2-1]) || !G_DIFF(yint, yout[*intrsct2-1] ) ) {
		    xout[*intrsct2] = xint;
		    yout[*intrsct2] = yint;

		    bpnt1[*intrsct2] = ii;
		    apnt1[*intrsct2] = ii + 1;
		    bpnt2[*intrsct2] = jj;
                    apnt2[*intrsct2] = jj + 1;

		    *intrsct2 = *intrsct2 + 1; 

		    if (*intrsct2 >= *intrsct1) return; 
		
		}	
	    }
	}
    }

    if (*intrsct2 == 0) *iret = -1;

} 
