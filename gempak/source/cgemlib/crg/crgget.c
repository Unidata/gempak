#include "crgcmn.h"


void crg_get ( int elnum, int *layer, filter_t dsplyFilter, 
		float *llx, float *lly, float *urx, float *ury,
		int *iret )
/************************************************************************
 * crg_get								*
 *                                                                      *
 * This function returns the range information for a particular record	*
 * in the range array.							*
 *                                                                      *
 * crg_get ( elnum, layer, dsplyFilter, llx, lly, urx, ury, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*layer		int		layer number			*
 *	dsplyFilterr	filter_t	display filter			*
 *	*llx		float		Lower left X			*
 *	*lly		float		Lower left Y			*
 *	*urx		float		Upper right X			*
 * 	*ury		float		Upper right Y			*
 *      *iret           int             Return code                     *
 *                                       -2 = Element is out of bounds	*
 *					 -5 = Element is deleted	*
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97	Created					*
 * F.J.Yen/NCEP		 1/98	Renamed from crg_getrng.  Cleaned up.	*
 * J. Wu/SAIC		 1/02	add layer param. 			*
 * E. Safford/SAIC	04/02	init output params, ck for bad ioffset  *
 * J. Wu/SAIC		07/04	add param. "dsplyFilter"		*
 ***********************************************************************/
{
    *iret  = 0;
    *layer = -1;

    *llx   = 0.0F;
    *lly   = 0.0F;
    *urx   = 0.0F;
    *ury   = 0.0F;

    if ((elnum >=0 ) && ( elnum < MAX_EDITABLE_ELEMS) ) {
        if ( range[elnum].ioffset >= 0 ) {
            *layer = range[elnum].layer;
            strcpy ( dsplyFilter, range[elnum].dsplyFilter );
            *llx   = range[elnum].rleft;
	    *lly   = range[elnum].rbottom;
	    *urx   = range[elnum].rright;
	    *ury   = range[elnum].rtop;
	}
	else {
	    *iret = -5;			/* elnum is deleted */	
	}
    }
    else {
	*iret = -2;			/* elnum out of bounds */
    }

}
