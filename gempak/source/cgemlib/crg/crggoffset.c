#include "crgcmn.h"


void crg_goffset ( int elnum, int *joffset, int *iret )
/************************************************************************
 * crg_goffset                                                          *
 *                                                                      *
 * This function returns the file position for an element based		*
 * on the element number in the range array.				*
 *                                                                      *
 * crg_goffset ( elnum, joffset, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*joffset	int		File position of the element	*
 *      *iret           int             Return code                     *
 *					 -2 = elnum out of bounds	*
 *					 -5 = elnum is deleted		*
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97	Created					*
 * F.J.Yen/NCEP		 1/98	Renamed from crg_fpos.  Cleaned up.	*
 * E. Safford/SAIC	04/02	init joffset, add error checks		*
 ***********************************************************************/
{
    *iret    = 0;
    *joffset = -1;

    if ((elnum < MAX_EDITABLE_ELEMS) && ( elnum >= 0) ) {
	if ( range[elnum].ioffset >= 0 ) {
            *joffset = range[elnum].ioffset;
	}
	else {
	    *iret = -5;			/* elnum is deleted */
	}
    }
    else {
	*iret = -2;			/* elnum out of bounds */
    }

}
