#include "cvgcmn.h"

void cvg_freeElPtr ( VG_DBStruct *el )
/************************************************************************
 * cvg_freeElPtr							*
 *									*
 * This function frees the block pointers in the GFA element.		*
 *									*
 * Note: this routines needs to be used as clean-up whenever a local	* 
 *       variable of type VG_DBStruct is declared and read anywhere	*
 *       (PGEN, cgemlib, and anywhere else).				*
 *									*
 * cvg_freeElPtr  ( el )						*
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
 * E. Safford/SAIC	11/04	add sanity checks & set blockPtr = NULL *
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

/*
 *  If the element is of type GFA, free its block pointers.
 */         
    if ( el->hdr.vg_type == GFA_ELM ) {
        for ( ii = 0; ( ii < el->elem.gfa.info.nblocks ) && 
		      ( ii < MAX_GFA_BLOCKS ); ii++ ) {
	    if ( el->elem.gfa.info.blockPtr[ii] && 
	    	 ( el->elem.gfa.info.nblocks <= MAX_GFA_BLOCKS &&
		   el->elem.gfa.info.nblocks > 0 ) ) {
	
	        free ( el->elem.gfa.info.blockPtr[ii] );
            }

	    el->elem.gfa.info.blockPtr[ii] = NULL;
	}
        
	el->elem.gfa.info.nblocks = 0;
    }
}
