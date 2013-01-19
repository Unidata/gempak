#include "cvgcmn.h"

void cvg_getElFilter ( VG_DBStruct *el, filter_t filter, int *iret )
/************************************************************************
 * cvg_getElFilter							*
 *									*
 * This function returns the display filter for the specified element.  *
 *									*
 * cvg_getElFilter ( el, filter, iret )					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	element structure		*
 *									*
 * Output parameters:							*
 *	filter          filter_t        Element's display filter        *
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * M. Li/SAIC		08/04	Created					*
 * J. Wu/SAIC		10/04	use cvg_getFld() for GFA_ELM		*
 ***********************************************************************/
{
    int		ier;
/*---------------------------------------------------------------------*/

    *iret = 0;
        
    if ( el != NULL ) {
	if ( el->hdr.vg_type != GFA_ELM ) {
	    strcpy ( filter, "ALL" );
	}
	else {
	    cvg_getFld ( el, TAG_GFA_FCSTHR, filter, &ier );
	}
    }
    else {
	*iret = -2;
    }

}
