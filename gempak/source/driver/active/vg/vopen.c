#include "vgcmn.h"

void vopen ( int *iret )
/************************************************************************
 * vopen								*
 *									*
 * This function opens a VG file with read/write permission.		*
 *									*
 * vopen ( iret )			               			*
 *                                                                      *
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					 -1 = error opening VG file	*
 *                                                                      *
 **									*
 * Log:									*
 * J. Wu/GSC	 02/01		Created based on psopen()		*
 ***********************************************************************/
{
    int		ier;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *	open the file and mark it.
     */
    cvg_open( curfil, G_TRUE, &flun, &ier );
    
    if ( ier >= 0 ) {       /* when a new VGF is created, ier = 2 */
        opnfil = G_TRUE;  }  
    else {            
        *iret  = -1;
        opnfil = G_FALSE;
    }
}
