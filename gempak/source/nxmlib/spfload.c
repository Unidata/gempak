#include "geminc.h"
#include "gemprm.h"
#include "spfcmn.h"

void spf_load ( char *filnam, int *iret )
/************************************************************************
 * spf_load								*
 *									*
 * This function loads the file contents into the "_spfBuffer". If the	*
 * file does not exist, it will not be created & nothing will be read.	*
 *									*
 * spf_load ( filnam, iret )						*
 *									*
 * Input parameters:							*
 *      *filnam		char		file name with full path	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					4 - Reach the end of file	*
 *					0 - Normal 			*
 *				       -1 - File does not exist		*
 *				       -2 - File cannot be opened	*
 *				       -4 - Failure to read SPF file	*
 *				       -7 - Failure to allocate buffer	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	6/01	create						*
 * J. Wu/GSC	7/01	allow access to read-only SPF files		*
 * H. Zeng/SAIC 9/04    spf_read() para. list change			*
 ***********************************************************************/
{

    int     flen, ier;
    FILE    *fptr;
        
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    fptr = NULL;
    
    if ( _initSPF != G_TRUE )  spf_init( &ier ); 
    
    spf_open( filnam, FALSE, &fptr, &flen, iret );
        
    if ( *iret >= 0 ) { 
        
       spf_read( fptr, filnam, flen, iret );

    }    
        
    if ( fptr != NULL ) {
       
       spf_close( fptr, &ier );
       
    }
           
}
