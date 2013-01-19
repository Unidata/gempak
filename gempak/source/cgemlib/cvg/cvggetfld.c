#include "cvgcmn.h"

void cvg_getFld ( const VG_DBStruct *el, const char *tag, 
		  			char *value, int *iret )
/************************************************************************
 * cvg_getFld								*
 *									*
 * This function gets a VG element's field value from its blocks which	*
 * contains many tags.  In an element's block, tags must be prefixed	*
 * and suffixed by < and >, e.g., "<TAG_GFA_NBLOCKS>5<TAG_GFA_NPTS>20".	*
 * The tag is usually in the form of "TAG_TYPE_FIELD", where "TYPE" is	*
 * the element's VG type (e.g, GFA ) and "FIELD" is the element's 	*
 * attribute name ( e.g, npts).						*
 *									*
 * Warning: The return value will be an empty string by default. So it	*
 *          better to check the return code before using the value.	*
 *									*
 * cvg_getFld  ( el, tag, value, iret )					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct 	Pointer to the VG record	*
 *	*tag		char 		A VG tag defined in vgtag.h	*
 *									*
 * Output parameters:							*
 *	*value		char 		Value associated the given tag	*
 *	*iret		int 		Return code			*
 *					-1 - Wrong type			*
 *					-2 - Search failed		*
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
     *  If it is not a GFA element or there are no blocks, return.
     */         
    if ( el->hdr.vg_type != GFA_ELM || 
         el->elem.gfa.info.nblocks <= 0 ) {	
        strcpy ( value, "" );
	*iret = -1;
	return;
    }
    

    /*
     *  Now loop over all blocks to search for the tag.
     */         
    for ( ii = 0; ii < el->elem.gfa.info.nblocks; ii++ ) {
        cst_gtag ( tag, (void *)el->elem.gfa.info.blockPtr[ii], 
                                  "", value, &ier );        
        if ( ier == 0 )  break;
    }
    

    /*
     *  Signal an unsucessful search.
     */         
    if ( ier != 0 ) {
        *iret = -2;
    }
    
    
}


