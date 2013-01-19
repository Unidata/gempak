#include "crgcmn.h"


int crg_getLayer ( int joffset )
/************************************************************************
 * crg_getLayer                                                      	*
 *                                                                      *
 * This function returns the layer for the elem. at the offset position	*
 *                                                                      *
 * int crg_getLayer ( joffset )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	joffset		int		Element offset in VG file	*
 *                                                                      *
 * Output parameters:                                                   *
 *    crg_getLayer	int		Element layer			*
 *                                      -1 - bad offset                	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           12/01	initial coding				*
 * E. Safford/SAIC	04/02	check joffset >= 0 			*
 ***********************************************************************/
{
    int		ii, el_layer;
/*---------------------------------------------------------------------*/

    el_layer = -1;

    if ( joffset >= 0 ) {

        for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++) {

	    if ( joffset == range[ ii ].ioffset ) {            
                el_layer = range[ ii ].layer;
	        break;
	    }
        }
    }
    
    return el_layer;
        
}
