#include "crgcmn.h"


void crg_getinx ( int joffset, int *elnum, int *iret )
/************************************************************************
 * crg_getinx                                                           *
 *                                                                      *
 * This function returns the range record index number for the element	*
 * at the input file position.                  			*
 *                                                                      *
 * crg_getinx ( joffset, elnum, iret ) 					*
 *                                                                      *
 * Input parameters:                                                    *
 * 	joffset		int		File position of the element	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*elnum		int		Element number			*
 *      *iret           int             Return code                     *
 *                                       -1 = Element not found		*
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97	Created					*
 * F.J.Yen/NCEP		 1/98	Renamed from crg_index.  Cleaned up.	*
 * E. Safford/SAIC	04/02	fix header, simplify			*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/
    *iret  = -1;
    *elnum = -1;

    for ( ii=0; ii< MAX_EDITABLE_ELEMS; ii++ ) {

        if ( range[ii].ioffset == joffset ) {
	    *elnum = ii;
	    *iret  = 0;
	    break;
	}
    }

}
