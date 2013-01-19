#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Layer_t	  layer [MAX_LAYERS];

void ctb_lygetcmode ( int ly, char *cmod, int *iret )
/************************************************************************
 * ctb_lygetcmode							*
 *									*
 * This function returns the "color_mode" value for the requested 	*
 * layer.								*
 *									*
 * ctb_lygetcmode ( ly, cmod, iret )					*
 *									*
 * Input parameters:							*
 *  ly		int	Layer number					*
 *									*
 * Output parameters:							*
 * *cmod		char	Color mode value			*
 * *iret		int	Return code				*
 *				  -3 - Layer out of range		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 2/02	Created					*
 # E. Safford/SAIC	11/03	changed MAX_LAYER to MAX_LAYERS		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Return layer name.
     */
    if ( ly < 0 || ly >= MAX_LAYERS ) {
	*iret = -3;
	return;
    }
    else {
	strcpy ( cmod, layer[ly].cmode );
    }
}
