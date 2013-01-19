#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "crgcmn.h"

void crg_rngpt ( float mx, float my, float *llx, float *lly, 
					float *urx, float *ury, int *iret ) 
/************************************************************************
 * crg_rngpt                                                            *
 *                                                                      *
 * This function returns the range for a single point (in map coords).	*
 * This does not write anything to the range record array -- it just    *
 * returns the range coordinates.					*
 *                                                                      *
 * crg_rngpt ( mx, my, llx, lly, urx, ury, iret ) 			*
 *                                                                      *
 * Input parameters:                                                    *
 *	mx		float		x coordinate of point		*
 *	my		float		y coordinate of point		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*llx		float		lower left x range coordinate 	*
 * 	*lly		float		lower left y range coordinate 	*
 * 	*urx		float		lower right x range coordinate	*
 * 	*ury		float		lower right y range coordinate	*
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/98	initial coding                        	*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * J. Wu/GSC            03/01   Standardized extra space EXTRA/EXTRA_SM	*
 ***********************************************************************/
{
    float	rx, ry;
    float	srx, sry;
    int 	ier, np;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Convert point in element to device...
     */
    np = 1;
    rx = mx;
    ry = my;
    gtrans(sys_M, sys_D, &np, &rx, &ry, &srx, &sry, &ier,
                        strlen(sys_M), strlen(sys_D));
   
    if (ier < 0) {
        *iret = -1;
    }
    else {
        *lly = sry - (float)EXTRA;
        *llx = srx - (float)EXTRA;
        *urx = srx + (float)EXTRA;
        *ury = sry + (float)EXTRA;
    }
}
