#include "geminc.h"
#include "gemprm.h"

void cgr_dist ( int np, float *xx, float *yy, float fx, float fy, 
			float *distance, int *nearest_vrt, int *iret )
/************************************************************************
 * cgr_dist								*
 *									*
 * This function determines the distance from a fixed point to a single	*
 * point or to the closest vertex in a multi-point line and also	*
 * returns the closest vertex number (0 if a single point).		*
 *									*
 * cgr_dist ( np, xx, yy, fx, fy, distance, nearest_vrt, iret )		*
 *									*
 * Input parameters:							*
 *	np		int		Number of points in figure	*
 *	*xx		float		X coordinates of figure		*
 *	*yy		float		Y coordinates of figure		*
 *	fx		float		X coordinate of fixed point	*
 *	fy		float		Y coordinate of fixed point	*
 *									*
 * Output parameters:							*
 *	*distance	float		Distance to the point		*
 *	*nearest_vrt	int		Closest vertex number		*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 2/97	Created					*
 * D.W.Plummer/NCEP	 7/97	Rewrite w/o trig functions		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * E. Safford/GSC	02/98	changed to call new cgr_segdist		*
 * S. Law/GSC		03/99	cleanup and commentary			*
 * W.D.Plummer/NCEP	12/02	chg call seg to cgr_segdist		*
 ***********************************************************************/
{
    int		next_vrt;
    float	kqx, kqy;
/*---------------------------------------------------------------------*/

    /*
     * For single point, simply figure distance to that point
     */
    if (np == 1) {
	*nearest_vrt = 0;
	*distance = (float) G_DIST (fx, fy, xx[0], yy[0]);
	*iret = 0;
    }
    else {
	cgr_segdist (&np, xx, yy, &fx, &fy, distance, nearest_vrt, 
		     &next_vrt, &kqx, &kqy, iret);

	*distance = (float) G_DIST (fx, fy, xx[*nearest_vrt], yy[*nearest_vrt]);
    }
}
