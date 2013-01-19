#include "crgcmn.h"


void crg_getsecrange ( int elnum, float *llx, float *lly,
		 float *urx, float *ury, int *iret )
/************************************************************************
 * crg_getsecrange							*
 *                                                                      *
 * This function returns the second range box information for a 	*
 * particular record in the range array.				*
 *                                                                      *
 * crg_getsecrange ( elnum, llx, lly, urx, ury, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*llx		float		Lower left X			*
 *	*lly		float		Lower left Y			*
 *	*urx		float		Upper right X			*
 * 	*ury		float		Upper right Y			*
 *      *iret           int             Return code                     *
 *                                       -2 = Element is out of bounds	*
 *					 -6 = Element is deleted	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		04/08	Created					*
 ***********************************************************************/
{
    *iret  = 0;

    *llx   = 0.0F;
    *lly   = 0.0F;
    *urx   = 0.0F;
    *ury   = 0.0F;

    if ( ( elnum >=0 ) && ( elnum < MAX_EDITABLE_ELEMS ) ) {
        if ( range[elnum].ioffset >= 0 &&
	     range[elnum].auxRec == PRIMARY_REC && 
	     range[ elnum ].assocRec >= 0 ) {
	        
	    *llx   = range[ range[ elnum ].assocRec ].rleft;
	    *lly   = range[ range[ elnum ].assocRec ].rbottom;
	    *urx   = range[ range[ elnum ].assocRec ].rright;
	    *ury   = range[ range[ elnum ].assocRec ].rtop;
        }
        else {
	    *iret = -6;		 /* no second range box */	
        }
    }
    else {
	*iret = -2;		 /* elelment index out of bound */	    
    }

}
