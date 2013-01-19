#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern  Layer_t   layer [MAX_LAYERS];

void ctb_lygetfmode ( int ly, char *fmod, int *iret )
/************************************************************************
 * ctb_lygetfmode							*
 *									*
 * This function returns the "fill_mode" value for the requested 	*
 * layer.								*
 *									*
 * ctb_lygetfmode ( ly, fmod, iret )					*
 *									*
 * Input parameters:							*
 *  ly		int	Layer number					*
 *									*
 * Output parameters:							*
 * *fmod		char	Fill mode value				*
 * *iret		int	Return code				*
 *				  -3 - Layer out of range		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 2/02	Created					*
 * E. Safford/SAIC	11/03	Changed MAX_LAYER to MAX_LAYERS		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get fill mode value.
     */
    if ( ly < 0 || ly >= MAX_LAYERS ) {
	*iret = -3;
	return;
    }
    else {
	strcpy ( fmod, layer[ly].fmode );
    }
}
