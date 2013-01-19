#include "geminc.h"
#include "gemprm.h"
#include "spfcmn.h"

void spf_clnup ( int *iret )
/************************************************************************
 * spf_clnup								*
 *									*
 * This function frees any allocated space for the buffer in spfcmn.h.	*
 *									*
 * spf_clnup ( iret )							*
 *									*
 * Input parameters:							*
 *      none								*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 - Normal			*
 *                                      2 - SPF buffer is not loaded    *
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	6/01	create						*
 ***********************************************************************/
{
    int   ier;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    if ( _initSPF != G_TRUE ) spf_init( &ier ); 
    
    if ( _spfBuffer == (char *)NULL ) {
    
        *iret = 2;
	
    }
    else {
    
        free( _spfBuffer );
        _spfBuffer = (char *)NULL;
	
    }
       
}
