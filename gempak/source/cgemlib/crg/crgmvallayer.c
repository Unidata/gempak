#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "crgcmn.h"

void crg_mvallayer ( int layer, Boolean *moved, int *iret )
/************************************************************************
 * crg_mvallayer 							*
 *									*
 * This function moves all range records to the layer.  The effect of   *
 * this routine is to converge all elements to a single layer.		*
 *									*
 * void crg_mvallayer ( layer, moved, iret )					*
 *									*
 * Input parameters:							*
 *	layer		int	layer value				*
 *									*
 * Output parameters:							*
 *	*moved		Boolean		True - some elements are moved	*
 *					False - no elements are moved	*
 *	*iret		int		Return code			*
 *					 -4 = layer value out of bounds	*
 *									*
 **									*
 * Log:									*
 * E. Safford/SAIC	02/02	initial coding				*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    *moved = FALSE;
    
    if ( ( layer < 0 ) || ( layer >= MAX_LAYERS ) ) {
        *iret = -4;
	return;
    }    

    for ( ii=0; ii<MAX_EDITABLE_ELEMS; ii++ ) {

        if ( (range[ii].ioffset >= 0) && (range[ii].layer != layer) ) {
	    range[ii].layer = layer;
            *moved = TRUE;
	}

    }
}
