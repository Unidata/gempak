#include "cvgcmn.h"
#include "pgprm.h"
#include "drwids.h"

void cvg_initelm ( VG_DBStruct *el )
/************************************************************************
 * cvg_initelm								*
 *									*
 * This function sets initial common characteristics for VG elements.	*
 *									*
 * cvg_initelm ( el )	                           			*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct 	VG record structure		*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	12/00	Created				       		*
 * J. Wu/GSC	02/01	Modified 'unused1' & 'unused2' to 'smooth' &  	*
 *			'version' due to the renaming in VG structure	*
 * J. Wu/SAIC	06/02	update watch box version to CUR_WBX_VER		*
 * J. Wu/SAIC	09/02	add CLASS_LIST					*
 * D.W.Plummer/NCEP 06/03 chgs for ASHCLD_ELM and VOLC_ELM		*
 * J. Wu/SAIC	09/03	add CLASS_MET -> JET_ELM			*
 * J. Wu/SAIC	10/03	remove smooth setting for JET_ELM		*
 * T. Lee/SAIC	11/03	added USR_PREF_FILE				*
 * T. Lee/SAIC	11/03	used cvgcmn.h					*
 * J. Wu/SAIC	01/04	add CLASS_MET -> GFA_ELM			*
 * B. Yin/SAIC	02/04	added CLASS_MET -> TCA->ELM          		*
 * H. Zeng/SAIC 10/04	removed initialization of "filled" for WATCHES	*
 * B. Yin/SAIC	02/06	allow open line GFAs				*
 ***********************************************************************/
{          

    /* 
     *  Clear deletion flag
     */
    
    el->hdr.delete = 0;     
    
    /* 
     *  Set element-specific attributes.
     */
     
    el->hdr.version = 0;
    switch ( el->hdr.vg_class ) {

        case CLASS_FRONTS:
            el->hdr.filled  = 0;
            el->hdr.closed  = 0;
    	    el->hdr.version = 0;
	break;
	
        case CLASS_SYMBOLS:
        case CLASS_MARKER:
        case CLASS_COMSYM:
        case CLASS_WINDS:
        case CLASS_TEXT:
            el->hdr.filled  = 0;
            el->hdr.closed  = 0;
            el->hdr.smooth = 0; 		                  
    	    el->hdr.version = 0;
	break;
	
	case CLASS_WATCHES:

	    /*
	     * "filled" is used to indicate whether 
	     * to use county color fill or marker
             * and is no longer initialized to zero.
             */
         
            el->hdr.closed  = 0;
            el->hdr.smooth = 0; 		                  
            el->hdr.version = CUR_WBX_VER; 
	break;
	       
        case CLASS_CIRCLE:
            el->hdr.closed  = 0;
            el->hdr.smooth = 0;	
    	    el->hdr.version = 0;
	break;

        case  CLASS_TRACKS:
            el->hdr.version = 1;
	break;

        case  CLASS_SIGMETS:
	    if ( el->hdr.vg_type == SIGCCF_ELM )                       
                el->hdr.version = 0;
	    else if ( el->hdr.vg_type == ASHCLD_ELM )                       
                el->hdr.version = 0;
	    else if ( el->hdr.vg_type == VOLC_ELM )                       
                el->hdr.version = 0;
	    else
                el->hdr.version = 1;	    
 	break;

        case  CLASS_LIST:
            el->hdr.smooth = 0;
            el->hdr.version = 0;
 	break;
        
	case  CLASS_MET:
	    if ( el->hdr.vg_type == JET_ELM ) {                      
                el->hdr.filled  = 0;
                el->hdr.closed  = 0;
                el->hdr.version = 0;
	    }
	    else if ( el->hdr.vg_type == GFA_ELM ) {
                el->hdr.version = 0;	    
	    }
	    else if ( el->hdr.vg_type == TCA_ELM ) {
                el->hdr.filled  = 0;
                el->hdr.closed  = 0;
                el->hdr.version = 0;	    
	    }
	break;
    }       
}
