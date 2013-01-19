#include "crgcmn.h"


void crg_clearLayer ( int layer, int *iret )
/************************************************************************
 * crg_clearLayer                                                      	*
 *                                                                      *
 * This function resets the offset values to -1 for all matching layer 	*
 * values in the range array.						*
 *                                                                      *
 * void crg_clearLayer( layer, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	layer		int		Element's layer			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		return code 			* 
 *					  -4 if layer out of range	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC         	12/01	initial coding				*
 * E. Safford/SAIC	04/02	call crg_clear to completely wipe rec   *
 ***********************************************************************/
{
    int		ii, ier;
/*---------------------------------------------------------------------*/    

    *iret = 0;

    if ( layer < 0 || layer >= MAX_LAYERS ) {
	*iret = -4;
    }
    else {

        for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++ ) {

            if ( layer == range[ ii ].layer ) {
  	        crg_clear ( ii, &ier );

		if ( ier < 0 ) {
		    *iret = ier;
		}
            }
        }
    }

}
