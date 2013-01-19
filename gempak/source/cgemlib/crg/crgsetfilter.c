#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "crgcmn.h"


void crg_setfilter ( int elnum, VG_DBStruct *el, int *iret )
/************************************************************************
 * crg_setfilter                                                   	*
 *                                                                      *
 * This function sets the display filter info for the given element.	*
 *                                                                      *
 * crg_setfilter ( elnum, el, iret ) 					*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 * 	*el		VG_DBStruct	VG element			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					  -2 - elnum out of range	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		07/04	initial coding				*
 * J. Wu/SAIC		10/04	use cvg_getFld to access GFA attributes	*
 ***********************************************************************/
{
    int		ier;
    char	value[32];
/*---------------------------------------------------------------------*/
    
    *iret = 0;

    if ( ( elnum < MAX_EDITABLE_ELEMS ) && ( elnum >= 0 ) ) {
        if ( el->hdr.vg_type != GFA_ELM ) {
	    strcpy ( range[elnum].dsplyFilter, "ALL" );
        }
	else {	    
	    cvg_getFld ( el, TAG_GFA_FCSTHR, value, &ier );
	    strcpy ( range[elnum].dsplyFilter, value );	
	}
    }
    else {
	*iret = -2;
    }

}
