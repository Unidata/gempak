#include "cvgcmn.h"

void cvg_allocGfaBlock ( VG_DBStruct *el )
/************************************************************************
 * cvg_allocGfaBlock							*
 *									*
 * This function allocates a new block for the GFA element.		*
 *									*
 * cvg_allocGfaBlock  ( el )						*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct 	Pointer to the VG record	*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/04	initial coding				*
 ***********************************************************************/
{
    int		nblks, blk_size, ii;
/*---------------------------------------------------------------------*/

/*
 *  If not GFA element or all block pointers are used, do nothing.
 */         
    if ( el->hdr.vg_type != GFA_ELM || 
         el->elem.gfa.info.nblocks >= MAX_GFA_BLOCKS ) {
        return;
    }
    

/*
 *  Calculate the size of a single block.
 */         
    blk_size = sizeof(char) * STD_STRLEN;
    
    
/*
 *  Allocate memory for one more block. 
 */         
    nblks = G_MAX ( el->elem.gfa.info.nblocks, 0 );    
    el->elem.gfa.info.blockPtr[nblks] = malloc ( blk_size );
    
    if ( !el->elem.gfa.info.blockPtr[nblks] ) {
        return;
    }
    
    
/*
 *  Increase the number of blocks by 1.
 */         
    el->elem.gfa.info.nblocks = nblks + 1;
    
    
/*
 *  Initialize the new space to all '\0'.
 */         
    memset ( el->elem.gfa.info.blockPtr[nblks], 
    			'\0', (size_t)STD_STRLEN );
			
			
/*
 *  Initialize all unused block pointers to NULL.
 */         
    for ( ii = el->elem.gfa.info.nblocks; ii < MAX_GFA_BLOCKS; ii++ ) {
	el->elem.gfa.info.blockPtr[ii] = NULL;                
    }
            

/*
 *  Update the element's record size in the header.
 */         
    el->hdr.recsz = sizeof(VG_HdrStruct) + sizeof(int) * 2 + 
    		    el->elem.gfa.info.nblocks * blk_size +
		    el->elem.gfa.info.npts * sizeof(float) * 2;
        
}
