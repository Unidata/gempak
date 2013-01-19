#include "crgcmn.h"


void crg_deselect ( float *llx, float *lly, float *urx, float *ury )
/************************************************************************
 * crg_deselect								*
 *									*
 * This function turns off the selected flag for all elements and 	*
 * returns the corners for a box containing all elements previously	*
 * selected.								*
 *									*
 * crg_deselect ( llx, lly, urx, ury) 					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	*llx	float	Left  boundary					*
 *	*lly	float	Lower boundary					*
 *	*urx	float	Right boundary					*
 *	*ury	float	Upper boundary					*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	Initial coding				*
 * T. Piper/GSC		10/98	Prolog update				*
 * J. Wu/GSC            03/01   Standardized extra space EXTRA/EXTRA_SM	*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    *llx = *lly = 999999.0F;
    *urx = *ury = 0.0F;

    for (ii = 0; ii < MAX_EDITABLE_ELEMS; ii++) {
        if (range[ii].selected != 0) {
	    range[ii].selected =  0;
	    if (*llx > range[ii].rleft)
		*llx = range[ii].rleft;
	    if (*lly > range[ii].rbottom)
		*lly = range[ii].rbottom;
	    if (*urx < range[ii].rright)
		*urx = range[ii].rright;
	    if (*ury < range[ii].rtop)
		*ury = range[ii].rtop;
	}
    }

    *llx -= (float)EXTRA;
    *lly -= (float)EXTRA;
    *urx += (float)EXTRA;
    *ury += (float)EXTRA;
}
