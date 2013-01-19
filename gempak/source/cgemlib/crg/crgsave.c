#include "crgcmn.h"

void crg_save ( int elnum, int joffset, float llx, float lly, 
					float urx, float ury, int *iret )
/************************************************************************
 * crg_save								*
 *                                                                      *
 * This function saves range information in a slot in the range 	*
 * array.  The calling routine must determine which slot the 		*
 * information is to be saved to, and what the extents of the range	*
 * are.									*
 *                                                                      *
 * crg_save ( elnum, joffset, llx, lly, urx, ury, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	elnum		int		Element number to save to (slot)*
 * 	joffset		int		File position of the element	*
 *	llx		float		Lower left x			*
 *	lly		float		Lower left y			*
 * 	urx		float		Upper right x			*
 *	ury		float		Upper right y			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					-2 = Element is out of bounds	*	
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97	Created					*
 * F.J.Yen/NCEP		 1/98	Renamed from crg_svrng.	 Cleaned up.	*
 * T. Piper/GSC		10/98	Prolog update				*
 * E. Safford/SAIC	04/02	verify elnum is in range		*
 ***********************************************************************/
{
    *iret = 0;

    if ( elnum < 0 || elnum > MAX_EDITABLE_ELEMS ) {
        *iret = -2;
    }
    else {
	range[elnum].rleft   = llx;
	range[elnum].rright  = urx;
	range[elnum].rtop    = ury;
	range[elnum].rbottom = lly;
	range[elnum].ioffset = joffset;
   }

}
