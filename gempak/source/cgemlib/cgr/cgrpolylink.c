#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"

/*
 * Public Functions
 */

void cgr_polylink  ( int *npin0, float *xin0, float *yin0,
	             int *npin1, float *xin1, float *yin1, int *maxnpo,
		     int *npo, float *xo, float *yo, int *iret )

/************************************************************************
 * cgr_polylink								*
 *									*
 * This function 'links' two polygons together.				*
 *									*
 * Cartesian (Device, sys_D) coordinates are assumed with the first and	*
 * last points not being equal.						*
 *									*
 * Both incoming polygon's points are assumed to be ordered counter-	*
 * clockwise 								*
 *									*
 * If the link cannot be found, as in the case of embedded polygons,	*
 * then the encompassing polygon is returned.				*
 *									*
 * cgr_polylink ( npin0, xin0, yin0, npin1, xin1, yin1, maxnpo,		*
 * 		   npo, xo, yo, iret )					*
 *									*
 * Input parameters:							*
 * *npin0    	    int     Number of points in polygon1                *
 * *xin0     	    float   X coordinates of polygon 1                  *
 * *yin0     	    float   Y coordinates of polygon 1                  *
 * *npin1    	    int     Number of points in polygon2                *
 * *xin1     	    float   X coordinates of polygon 2                  *
 * *yin1     	    float   Y coordinates of polygon 2                  *
 * *maxnpo   	    int     Maximum number of output points		*
 *									*
 * Output parameters:							*
 * *npo      	    int     Number of points in output polygon          *
 * *xo       	    float   X coordinates of output polygon             *
 * *yo       	    float   Y coordinates of output polygon             *
 * *iret     	    int     Return code					*
 * 			    = G_NORMAL - normal				*
 * 			    = +1 - embedded polygon, returned outermost	*
 * 			    = -2 - one or both polygons <= 2 points	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/03						*
 ***********************************************************************/
{
int		ii, found, *inout, ier;
POLYGON		poly_0, poly_1, poly_link;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    if ( *npin0 <= 2 || *npin1 <= 2 )  {
	*iret = -2;
	return;
    }

    /*
     * Create the two POLYGON structures.
     */
    poly_0.first = polyp_create ( npin0, xin0, yin0 );
    poly_1.first = polyp_create ( npin1, xin1, yin1 );

    /*
     * Process them into a 'linked' POLYGON.
     */
    poly_link.first = NULL_POLYPOINT;
    polyp_link ( &poly_0, &poly_1, &poly_link, &ier );

    /*
     * Free the memory from the original POLYGON structures.
     */
    polyp_destroy ( poly_1.first );
    polyp_destroy ( poly_0.first );

    if ( ier == 0 )  {

        /*
	 * Retrieve the points from the 'linked' POLYGON.
         */
	polyp_getpts  ( poly_link.first, npo, xo, yo );

	/*
	 * Free 'linked' POLYGON memory.
	 */
	if ( poly_link.first != NULL_POLYPOINT )  {
	    polyp_destroy ( poly_link.first );
	}

    }
    else  {

        inout = NEW ( int, ((*npin0)*(*npin1)) );

	/*
	 * Test polygon #0 points against polygon #1
	 */
        cgr_inpoly ( sys_D, npin0, xin0, yin0, sys_D, npin1, xin1, yin1,
		     inout, &ier );

        found = G_FALSE;
        ii = 0;
        while ( !found && ii < *npin0 )  {
	    if ( inout[ii] == OUT )  {
	        found = G_TRUE;
	        break;
	    }
	    ii++;
        }

	if ( !found )  {

	    /*
	     * None of polygon #0 points are outside polygon #1;
	     * return polygon #1.
	     */
	    *npo = *npin1;
	    for ( ii = 0; ii < *npin1; ii++ )  {
		xo[ii] = xin1[ii];
		yo[ii] = yin1[ii];
	    }
	}
	else  {

	    /*
	     * Return polygon #0 for all other cases.
	     */
	    *npo = *npin0;
	    for ( ii = 0; ii < *npin0; ii++ )  {
		xo[ii] = xin0[ii];
		yo[ii] = yin0[ii];
	    }

	}

        free ( inout );

	*iret = +1;

    }

    return;

}
