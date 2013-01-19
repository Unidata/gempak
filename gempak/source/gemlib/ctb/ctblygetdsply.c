#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern  Layer_t   layer [MAX_LAYERS];

void ctb_lygetdsply ( int ly, char *dmod, int *iret )
/************************************************************************
 * ctb_lygetdsply							*
 *									*
 * This function returns the "display_mode" value for the requested 	*
 * layer.								*
 *									*
 * ctb_lygetdsply ( ly, dmod, iret )					*
 *									*
 * Input parameters:							*
 *  ly		int	Layer number					*
 *									*
 * Output parameters:							*
 * *dmod		char	Display mode value			*
 * *iret		int	Return code				*
 *				  -3 - Layer out of range		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 4/02	Created					*
 * E. Safford/SAIC	11/03	changed MAX_LAYER to MAX_LAYERS		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get display mode value.
     */
    if ( ly < 0 || ly >= MAX_LAYERS ) {
	*iret = -3;
	return;
    }
    else {
	strcpy ( dmod, layer[ly].dsply );
    }
}
