#include "cvgcmn.h"

void cvg_rmFld ( VG_DBStruct *const el, const char *tag, int *iret )
/************************************************************************
 * cvg_rmFld								*
 *									*
 * This function removes a tag and its associated field value from a 	*
 * VG element's block.  The tag is usually in the form of 		*
 * "TAG_TYPE_FIELD", where "TYPE" is the element's VG type (e.g. GFA)	*
 * and "FIELD" is the element's attribute name (e.g. npts).		*
 *									*
 * cvg_rmFld  ( el, tag, iret )						*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct 	Pointer to the VG record	*
 *	*tag		char 		A VG tag defined in vgtag.h	*
 *									*
 * Output parameters:							*
 *	*iret		int 		Return code			*
 *					-1 - Wrong type			*
 *					-2 - Failed			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/04	initial coding				*
 ***********************************************************************/
{
    int		ii, ier;
/*---------------------------------------------------------------------*/

    *iret = 0;
    
    
    /*
     *  If it is not a GFA element, return.
     */         
    if ( el->hdr.vg_type != GFA_ELM ) {
	*iret = -1;
	return;
    }
    

    /*
     *  Loop over all blocks to remove the tag and its asscoicated value.
     */             
    for ( ii = 0; ii < el->elem.gfa.info.nblocks; ii++ ) {
        cst_rmtag ( tag, (void *)el->elem.gfa.info.blockPtr[ii], &ier );
        
	if ( ier == 0 )	break;
    }


    /*
     *  Signal an unsucessful removal.
     */         
    if ( ier != 0 ) {
        *iret = -2;
    }
        
}




