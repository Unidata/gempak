#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern  Layer_t	  layer [MAX_LAYERS];

void ctb_lygetcolor ( int ly, int *icolr, int *iret )
/************************************************************************
 * ctb_lygetcolor							*
 *									*
 * This function returns the "color_id" value for the requested 	*
 * layer.								*
 *									*
 * ctb_lygetcolor ( ly, icolr, iret )					*
 *									*
 * Input parameters:							*
 *  ly		int	Layer number					*
 *									*
 * Output parameters:							*
 * *icolr		int	Color id value				*
 * *iret		int	Return code				*
 *				  -3 - Layer out of range		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 2/02	Created					*
 * E. Safford/SAIC	11/03	changed MAX_LAYERS to MAX_LAYERS	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Return color id.
     */
    if ( ly < 0 || ly >= MAX_LAYERS ) {
	*iret = -3;
	return;
    }
    else {
	*icolr = layer[ly].cid;
    }
}
