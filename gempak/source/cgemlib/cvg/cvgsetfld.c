#include "cvgcmn.h"

void cvg_setFld ( VG_DBStruct *el, const char *tag, 
		  			const char *value, int *iret )
/************************************************************************
 * cvg_setFld								*
 *									*
 * This function sets a new value for a VG element's field/attribute.	*
 * The tag is usually in the form of "TAG_TYPE_FIELD", where "TYPE" is	*
 * the element's VG type (e.g., GFA, ) and "FIELD" is the element's 	*
 * attribute name (e.g., npts).						*
 *									*
 * cvg_setFld  ( el, tag, value, iret )					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct 	Pointer to the VG record	*
 *	*tag		char 		A VG tag defined in vgtag.h	*
 *	*value		char 		Value associated the given tag	*
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
    int		nblks, ii, ier;
    int		maxLen, curBlk, len, svBlk;
    char	oldValue[STD_STRLEN];
    GfaInfo	*ptr;
/*---------------------------------------------------------------------*/

    *iret = 0;
    
    ptr = &el->elem.gfa.info;
    

    /*
     *  If it is not a GFA element, return.
     */         
    if ( el->hdr.vg_type != GFA_ELM ) {
	*iret = -1;
	return;
    }


    /*
     *  Locate the block that contains the tag.
     */                 
    nblks = G_MAX ( ptr->nblocks, 0 );
    curBlk = -1;        
    for ( ii = 0; ii < nblks; ii++ ) {
        cst_gtag ( tag, (void *)ptr->blockPtr[ii], 
                                    "", oldValue, &ier );        
        if ( ier == 0 )  {            		    	    
	    curBlk = ii;
	    break;
        }    
    }
       

    /*
     *  If the block was found, remove the tag and its associated
     *  value. Then check if the new value can fit into this block. 
     */             
    svBlk = -1;
    if ( curBlk >= 0 ) {
	cvg_rmFld ( el, tag, &ier );
        len = strlen( value ) - strlen( oldValue );        
        
	if ( len <= 0 ) {
            svBlk = curBlk;       	
	}
	else {
	    len = len + strlen ( (void *)ptr->blockPtr[curBlk] );	    
	    if ( len < (STD_STRLEN-1) ) {
                svBlk = curBlk;
	    }
	}	
    }
    
    
    /* 
     *  Check if the new value can fit into any existing blocks. 
     */                 
    if ( svBlk < 0 ) {
	for ( ii = 0; ii < nblks; ii++ ) {
            if ( ii != curBlk ) {
	        len = strlen ( value ) + 
	              strlen ( (void *)ptr->blockPtr[ii] );
	        if ( len < (STD_STRLEN-1) ) {
                    svBlk = ii;			
		   break;		    
	        }
	    }
        }    
    }
        

    /* 
     *  A new block is needed. 
     */                 
    if ( svBlk < 0 ) {
        cvg_allocGfaBlock ( el );    
        
	if ( !ptr->blockPtr[nblks] ) {
	    *iret = -2;
	    return;
        }
	
	svBlk = nblks;
        
    }
    
    
    /*
     *  Set the new tag value into the correct block.
     */                    
    maxLen = STD_STRLEN;
    cst_stag ( tag, value, &maxLen, 
                   (void *)ptr->blockPtr[svBlk], &ier );            

    
    /*
     *  Signal an unsucessful set.
     */         
    if ( ier != 0 ) {
        *iret = -2;
    }
        
}
