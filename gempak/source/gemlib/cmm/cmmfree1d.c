#include "geminc.h"
#include "gemprm.h"
#include "proto_cmm.h"

void cmm_free1d ( void *ptr1d, int *iret )
/************************************************************************
 * cmm_free1d                                                           *
 *                                                                      *
 * This subroutine frees the 1-D array allocated by cmm_malloc1d.	*
 *                                                                      *
 * cmm_free1d ( ptr1d, iret )						*
 *                                                                      *
 * Input parameters:                                         		*
 *	*ptr1d		void		Pointer to 1-D array		*
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
     * Free the 1-D array.
     */
    if ( ptr1d != NULL ) {
	free ( ptr1d );
	ptr1d = NULL;
    }
    
    return;
}
