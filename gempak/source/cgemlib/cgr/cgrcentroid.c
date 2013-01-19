#include "geminc.h"
#include "gemprm.h"

void cgr_centroid ( float x[], float y[], int *np, float *xcent, 
				float *ycent, float *area, int *iret )
/************************************************************************
 * cgr_centroid								*
 *									*
 * This function computes the area and centroid (or center of mass)	*
 * of the given polygon.						*
 *									*
 * Reference:								*
 * Graphics Gems IV, "Centroid of a Polygon", Gerard Bashein and	*
 * Paul R. Detmer, pp 3-5.						*
 *									*
 * cgr_centroid ( x, y, np, xcent, ycent, area, iret )			*
 *									*
 * Input parameters:							*
 *	x [np]		float		X coordinates of polygon	*
 *	y [np]		float		Y coordinates of polygon	*
 *      *np             int             Number of point in polygon      *
 *									*
 * Output parameters:							*
 *	*xcent		float		X coordinate of centroid	*
 *	*ycent		float		Y coordinate of centroid	*
 *	*area		float		Area of polygon			*
 *	*iret		int		Return code			*
 *					  -1 = Not enough points	*
 *					  -2 = Area is zero		*
 *									*
 **									*
 * S. Jacobs/NCEP	11/01	Created					*
 * S. Jacobs/NCEP	12/01	Set return values if not enough points	*
 * M. Li/SAIC		03/04	Prolog change for *area			*
 * D.W.Plummer/NCEP	03/05	Rm relationship between area and cntrd	*
 * M. Li/SAIC		04/05	Output positive area			*
 ***********************************************************************/
{

    register int	i, j;
    float		ai, atmp = 0.0F, xtmp = 0.0F, ytmp = 0.0F;

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Check for at least 3 points to make a polygon.
     */
    if  ( *np < 3 )  {
	*iret  = -1;
    	*xcent = RMISSD;
    	*ycent = RMISSD;
	*area  = RMISSD;
    	return;
    }

    /*
     * Compute the summation of the area and the first moments.
     */
    for ( i = *(np)-1, j = 0; j < *np; i = j, j++ )  {

    	ai = x[i] * y[j] - x[j] * y[i];
	atmp += ai;
	xtmp += ( x[j] + x[i] ) * ai;
	ytmp += ( y[j] + y[i] ) * ai;
    }

    /*
     * Compute the area of the polygon.
     */
    *area = G_ABS ( atmp / 2.0F );

    /*
     * Compute the location of the centroid of the polygon.
     */
    if  ( !G_DIFF(atmp, 0.0F) )  {
    	*xcent = xtmp / ( 3.0F * atmp );
    	*ycent = ytmp / ( 3.0F * atmp );
    }
    else {
    	*xcent = 0; *ycent = 0;
	for ( i = 0; i < *np; i++ )  {
	    *xcent += x[i]; *ycent += y[i];
	}
	*xcent /= *np; *ycent /= *np;
    	*iret = -2;
    }

}
