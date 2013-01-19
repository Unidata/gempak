#include "crgcmn.h"


void crg_goffsets ( int vg_class,  int	vg_type,     int layer, 
		    int offsets[], int *num_offsets )
/************************************************************************
 * crg_goffsets								*
 *                                                                      *
 * This function returns the file offsets for elements in the range     *
 * records that match the vg_class, vg_type, and layer.			*
 *                                                                      *
 * crg_get ( vg_class, vg_type, layer, offsets, num_offsets )      	*
 *                                                                      *
 * Input parameters:                                                    *
 *	vg_class	char		vg class 			*
 *	vg_type 	char		vg type 			*
 *	layer 		int		layer   			*
 *                                                                      *
 * Output parameters:                                                   *
 *	offsets[]	int		array of file offsets		*
 *	*num_offsets	int		number of offsets returned 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	10/04	initial coding				*
 * J. Wu/SAIC		07/07	look through primary records only 	*
 ***********************************************************************/
{
    int	ii;
/*---------------------------------------------------------------------*/

    *num_offsets = 0;

    for( ii=0; ii < MAX_EDITABLE_ELEMS; ii++ ) {

        if( ((int)range[ii].vg_class == vg_class) && 
	    ((int)range[ii].vg_type  == vg_type)  &&
	    (range[ii].layer         == layer)    &&
	    (range[ii].auxRec        == PRIMARY_REC ) ) {

            offsets[*num_offsets] = range[ii].ioffset; 
	    *num_offsets = *num_offsets + 1;

	}
    }

}

