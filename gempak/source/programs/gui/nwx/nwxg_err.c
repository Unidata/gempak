#include "nwx_cmn.h"
#include "nwx_gui.h"

/************************************************************************
 * nwxg_err.c								*
 *									*
 * This module handles generating error messages to the user.		*
 *									*
 * CONENTS:								*
 *	err_showError()		open an error dialog to report error	*
 ***********************************************************************/

void err_showError ( int iret )
/************************************************************************
 * err_showError							*
 *									*
 * Report errors with opening or reading files.				*
 *									*
 * err_showError ( iret )						*
 *									*
 * Input parameters:                                                    *
 *	iret		int		error code			*
 *									*
 * Output parameters:                                                   *
 *	NONE								*
 **									*
 * Log:                                                                 *
 * T. Piper/SAIC	01/04	Created					*
 * E. Safford/SAIC	12/07	rename, wrap Nxm functions		*
 ***********************************************************************/
{
char message[LLPATH];
char mess8[32] = "NWX:  ERROR scanning directory";
char mess9[32] = "NWX:  ERROR opening/reading";
char messtag[28] = "See system administrator.";

/*----------------------------------------------------------------------*/

    if ( iret == -8 ) {
         sprintf(message, "%s '%s'.\n %s", mess8,
                 srchInfo.dir_info.dirpath, messtag);
         wnxm_NxmWarn_show(mapCanvW, message);
    }
    else if ( iret == -9 ) {
         sprintf(message, "%s '%s'.\n %s", mess9, 
		nwxTable->dtyp_info[srchInfo.idtyp].loctbl, messtag);
         wnxm_NxmWarn_show(mapCanvW, message);
    }
}
