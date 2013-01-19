#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "crgcmn.h"

void crg_setLayer ( int elnum, int layer, int *iret )
/************************************************************************
 * crg_setLayer	 							*
 *									*
 * This function sets the range layer value for an element.		*
 *									*
 * void crg_setLayer ( elnum, layer, iret )				*
 *									*
 * Input parameters:							*
 *	elnum		int	Index for the elem. in the range array	*
 *	layer		int	layer value				*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  1 = Elem. ioffset < 0		*
 *					 -2 = elnum out of bounds	*
 *					 -4 = layer value out of bounds	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		12/01	initial coding				*
 ***********************************************************************/
{
    *iret = G_NORMAL;
    
    if ( ( elnum < 0 ) || ( elnum >= MAX_EDITABLE_ELEMS ) ) {
        *iret = -2;
    }
    else if ( ( layer < 0 ) || ( layer >= MAX_LAYERS ) ) {
        *iret = -4;
    }    
    else {
        if ( range[elnum].ioffset < 0 )  *iret = 1;
        
	range[elnum].layer = layer;
    }
}
