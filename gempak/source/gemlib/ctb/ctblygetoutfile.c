#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern  Layer_t   layer [MAX_LAYERS];

void ctb_lygetoutfile ( int ly, char *outfile, int *iret )
/************************************************************************
 * ctb_lygetoutfile							*
 *									*
 * This function returns the "outfile" value for the requested layer.	*
 *									*
 * ctb_lygetoutfile ( ly, outfile, iret )				*
 *									*
 * Input parameters:							*
 *  ly		int	Layer number					*
 *									*
 * Output parameters:							*
 * *outfile		char	Outfile name				*
 * *iret		int	Return code				*
 *				  -3 - Layer out of range		*
 **									*
 * Log:									*
 * E. Safford/SAIC	11/03	Created					*
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
	strcpy ( outfile, layer[ly].outfile );
    }
}
