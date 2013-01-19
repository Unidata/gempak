#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern  Layer_t   layer [MAX_LAYERS];

void ctb_lygetfile ( int ly, char *file, int *iret )
/************************************************************************
 * ctb_lygetfile							*
 *									*
 * This function returns the "file" value for the requested layer.	*
 *									*
 * ctb_lygetfile ( ly, file, iret )					*
 *									*
 * Input parameters:							*
 *  ly		int	Layer number					*
 *									*
 * Output parameters:							*
 * *file		char	File name				*
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
     * Retrieve file value.
     */
    if ( ly < 0 || ly >= MAX_LAYERS ) {
	*iret = -3;
	return;
    }
    else {
	strcpy ( file, layer[ly].vgfile );
    }
}
