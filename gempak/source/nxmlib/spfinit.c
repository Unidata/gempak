#include "geminc.h"
#include "gemprm.h"

#define  SPF_GLOBAL
#include "spfcmn.h"

void spf_init ( int *iret )
/************************************************************************
 * spf_init								*
 *									*
 * This function initialize the global variables in SPF library.	*
 *									*
 * spf_init ( iret )							*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 - Normal			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	6/01	create						*
 ***********************************************************************/
{
   
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    
    _initSPF = G_TRUE;        
    _spfBuffer = (char *)NULL;
    
}
