#include "geminc.h"
#include "gemprm.h"

void spf_close ( FILE *fptr, int *iret )
/************************************************************************
 * spf_close								*
 *									*
 * This function closes an SPF file.					*
 *									*
 * spf_close ( fptr, iret )						*
 *									*
 * Input parameters:							*
 *      *fptr		FILE		File pointer			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 - Normal			*
 *				       -6 - No file has been opened	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	6/01	create						*
 ***********************************************************************/
{
    int    ier, ierr;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    
    if ( fptr == NULL ) {
        
	*iret = -6;
	return;
	
    }
    
    cfl_clos( fptr, &ier );
    
    if ( ier != 0 ) {
    
        er_wmsg ( "CFL", &ier, " ", &ierr, 3, 1 ); 
    
    }
        
}
