#include "geminc.h"
#include "gemprm.h"

void cgr_qrol ( int *np, float *xl, float *yl, int *closed,
                float *xp, float *yp, float *tol, int *rol, int *iret )
/************************************************************************
 * cgr_qrol								*
 *									*
 * This function determines if a given point is to the right of a 	*
 * multi-points line whose direction is determined by order of points.	*
 * The evaluation tolerance is used to determine if a point is on the	*
 * line. A tolerance of 0.0F forces the point to be COMPUTATIONALLY	*
 * EXACTLY on the line regardless of whether the point is THEORETICALLY	*
 * EXACTLY on the line.							*
 *									*
 * void cgr_qrol ( np, xl, yl, closed, xp, yp, rol, iret )		*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points of line	*
 *	*xl		float		X coordinates of line		*
 *	*yl		float		Y coordinates of line		*
 *	*closed		int		Flag indicate if line closed	*
 *	*xp		float		X coordinate of given point	*
 *	*yp		float		Y coordinate of given point	*
 *	*tol		float		Evaluation tolerance		*
 *									*
 * Output parameters:							*
 *	*rol		int		Position indicator		*
 *					   1 - right of line		*
 * 					   0 - on the line		*
 *					  -1 - left of line		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * R. Tian/SAIC		07/05		Created				*
 * D.W.Plummer/NCEP	12/06		Added evaluation tolerance	*
 ***********************************************************************/
{
    double va[3] = {0.0F}, vb[3] = {0.0F}, vc[3] = {0.0F};
    double z1, z2, z3;
    float  dist, xner, yner, tolerance;
    int clsd, duplicate, linend, nearest_vtx, next_vtx, l0, l1, l2;
/*---------------------------------------------------------------------*/
    *iret = 0;
    clsd = *closed;
    tolerance = *tol;

    /*
     * Check if closed line has duplicate end point.
     */
    if (G_DIFF(xl[0], xl[*np-1]) && G_DIFF(yl[0], yl[*np-1]) ) {
        duplicate = 1;
    } else {
        duplicate = 0;
    }
    if ( duplicate && ! clsd ) clsd = 1;

    /*
     * Get the nearest point on the line from the given point.
     */
    cgr_segdist ( np, xl, yl, xp, yp, &dist, &nearest_vtx, &next_vtx,
                  &xner, &yner, iret );

    /*
     * Check if the nearest point is a segment endpoint.
     */
    if ( ( G_DIFF(xner, xl[nearest_vtx]) && G_DIFF(yner, yl[nearest_vtx]) ) ||
         ( G_DIFF(xner, xl[next_vtx])    && G_DIFF(yner, yl[next_vtx]) ) ) {
        /*
         * Check if the nearest point is also a line endpoint.
	 */
	if ( ( G_DIFF(xner, xl[0])    && G_DIFF(yner, yl[0]) ) ||
	     ( G_DIFF(xner, xl[*np-1]) && G_DIFF(yner, yl[*np-1]) ) ) {
	    linend = 1;
	} else {
	    linend = 0;
	}

	if ( clsd || ( ! clsd && ! linend ) ) {
	    l1 = nearest_vtx;
            l0 = l1 - 1;
	    l2 = l1 + 1;
	    if ( l0 < 0 ) {
	        if ( duplicate ) {
	            l0 = *np - 2;
	        } else {
	            l0 = *np - 1;
	        }
	    }
	    if ( l2 == *np ) {
	        if ( duplicate ) {
	            l2 = 1;
	        } else {
	            l2 = 0;
	        }
	    }

            va[0] = *xp - xl[l0];
	    va[1] = *yp - yl[l0];
	    vb[0] = xl[l1] - xl[l0];
	    vb[1] = yl[l1] - yl[l0];
	    cgr_vectxprod ( va, vb, vc, iret );
	    z1 = vc[2];

            va[0] = *xp - xl[l1];
	    va[1] = *yp - yl[l1];
	    vb[0] = xl[l2] - xl[l1];
	    vb[1] = yl[l2] - yl[l1];
	    cgr_vectxprod ( va, vb, vc, iret );
	    z2 = vc[2];

            va[0] = xl[l1] - xl[l0];
	    va[1] = yl[l1] - yl[l0];
	    vb[0] = xl[l2] - xl[l1];
	    vb[1] = yl[l2] - yl[l1];
	    cgr_vectxprod ( va, vb, vc, iret );
	    z3 = vc[2];

	    if ( G_DIFFT(z1 * z2, 0.0F, tolerance) ) {
		if ( z3 > 0.0F ) {
		    *rol = ( z1 > 0.0F || z2 > 0.0F ) ? 1 : 0;
		} else if ( z3 < 0.0F ) {
		    *rol = ( z1 < 0.0F || z2 < 0.0F ) ? -1 : 0;
		} else {
		    *rol = 0;
		}
	    } else if ( z1 * z2 > 0.0F ) {
	        *rol = z1 > 0.0F ? 1 : -1;
	    } else {
	        *rol = z3 > 0.0 ? 1 : -1;
	    }
	} else {
            l1 = G_MIN ( nearest_vtx, next_vtx );
            l2 = l1 + 1;
            va[0] = *xp - xl[l1];
	    va[1] = *yp - yl[l1];
	    vb[0] = xl[l2] - xl[l1];
	    vb[1] = yl[l2] - yl[l1];
	    cgr_vectxprod ( va, vb, vc, iret );
	    *rol = ( G_DIFFT ( vc[2], 0.0F, tolerance ) ? 0 : vc[2] > 0.0 ? 1 : -1 );
	}
    } else {
        l1 = G_MIN ( nearest_vtx, next_vtx );
        l2 = l1 + 1;
        va[0] = *xp - xl[l1];
	va[1] = *yp - yl[l1];
	vb[0] = xl[l2] - xl[l1];
	vb[1] = yl[l2] - yl[l1];
	cgr_vectxprod ( va, vb, vc, iret );
	*rol = ( G_DIFFT ( vc[2], 0.0F, tolerance ) ? 0 : vc[2] > 0.0 ? 1 : -1 );
    }

    return;
}
