#include "geminc.h"
#include "gemprm.h"


void cgr_dang ( float *pt1x, float *pt1y, float *pt2x, float *pt2y, 
					float *angle, int *iret )
/************************************************************************
 * cgr_dang								*
 *									*
 * This function determines the angle between two points in a cartesian	*
 * coordinate system.  The returned angle is in degrees with zero 	*
 * degrees along the positive x-axis.					*
 *									*
 * cgr_dang ( pt1x, pt1y, pt2x, pt2y, angle, iret )			*
 *									*
 * Input parameters:							*
 *	*pt1x		float		1st point X			*
 *	*pt1y		float		1st point Y			*
 *	*pt2x		float		2nd point X			*
 *	*pt2y		float		2nd point Y			*
 *									*
 * Output parameters:							*
 *	*angle		float		Device angle			*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97	Created					*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * D.W.Plummer/NCEP	08/03	Chg inputs to pointers, update doc	*
 ***********************************************************************/
{
    double opp;
    double adj;
    double tmpang;
/*---------------------------------------------------------------------*/

    *iret = 0;
    opp = (double)(*pt2y - *pt1y);
    adj = (double)(*pt2x - *pt1x);

    if( (adj < 0.01F) && (adj > -0.01F) )  {
	if (opp >= 0.0F)
		*angle = 90.0F;
	else
		*angle = 270.0F;

    }
    else  {
	tmpang = ( (180.0F / M_PI) *  ( atan( (double)(opp/adj) ) ) );
        if ( *pt1x > *pt2x )  {
	    if ( *pt1y > *pt2y )
		*angle = (float)(-180.0F + tmpang);
	    else
		*angle = (float)(180.0F + tmpang);
	}
	else  {
	    *angle = (float)tmpang;
	}
    }

}
