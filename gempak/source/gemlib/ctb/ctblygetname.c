#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern  Layer_t   layer [MAX_LAYERS];

void ctb_lygetname ( int ly, int namlen, char *name, int *iret )
/************************************************************************
 * ctb_lygetname							*
 *									*
 * This function returns the "name" value for the requested layer.	*
 *									*
 * ctb_lygetname ( ly, namlen, name, iret )				*
 *									*
 * Input parameters:							*
 *  ly	int		Layer number					*
 *  namlen	int	length of layer name string			*
 *									*
 * Output parameters:							*
 * *name		char	Layer name				*
 * *iret		int	Return code				*
 *				  -3 - Layer out of range		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 2/02	Created					*
 * E. Safford/SAIC	06/02	add namlen param			*
 * E. Safford/SAIC	11/03	Changed MAX_LAYER to MAX_LAYERS		*
 ***********************************************************************/
{
    int		ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Get layer name.
     */
    if ( ly < 0 || ly >= MAX_LAYERS ) {
	*iret = -3;
	return;
    }
    else {
	cst_ncpy ( name, layer[ly].name, namlen-1, &ier );
    }
}
