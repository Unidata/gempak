#include "geminc.h"
#include "gemprm.h"
#include "proto_cmm.h"

void cmm_free2d ( void **ptr2d, int *iret )
/************************************************************************
 * cmm_free2d                                                           *
 *                                                                      *
 * This subroutine frees the 2-D array allocated by cmm_malloc2d.	*
 *                                                                      *
 * cmm_free2d ( ptr2d, iret )						*
 *                                                                      *
 * Input parameters:                                         		*
 *	**ptr2d		void		Pointer to 2-D array		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret		int		Return code			*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         01/06                                           *
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Free the 2-D array.
     */
    if ( *ptr2d != NULL ) free ( *ptr2d );

    /*
     * Free the row array.
     */
    if ( ptr2d != NULL ) {
	free ( ptr2d );
	ptr2d = NULL;
    }
    
    return;
}
