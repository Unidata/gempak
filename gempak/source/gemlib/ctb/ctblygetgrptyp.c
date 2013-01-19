#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern  Layer_t   layer [MAX_LAYERS];

void ctb_lygetgrptyp ( int ly, char *grptyp, int *iret )
/************************************************************************
 * ctb_lygetgrptyp							*
 *									*
 * This function returns the "group_type" for the requested layer.	*
 *									*
 * ctb_lygetgrptyp ( ly, grptyp, iret )					*
 *									*
 * Input parameters:							*
 *  ly			int	Layer number				*
 *									*
 * Output parameters:							*
 * *grptyp		char	Group type				*
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
     * Get group type.
     */
    if ( ly < 0 || ly >= MAX_LAYERS ) {
	*iret = -3;
	return;
    }
    else {
	strcpy ( grptyp, layer[ly].gtype );
    }
}
