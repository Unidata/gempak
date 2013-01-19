#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "spfcmn.h"

void npf_load ( char *filnam, int *iret )
/************************************************************************
 * npf_load								*
 *									*
 * This function loads the file contents into the "_spfBuffer". If the	*
 * file does not exist, it will not be created & nothing will be read.	*
 *									*
 * npf_load ( filnam, iret )						*
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
 *				       -4 - Failure to read NPF file	*
 *				       -7 - Failure to allocate buffer	*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	5/05	Modified from spf_load			*
 * T. Piper/SAIC	06/06	declared flen long			*
 ***********************************************************************/
{

    int     ier;
    FILE    *fptr;
    long    flen;
        
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    fptr = NULL;
    
    if ( _initSPF != G_TRUE )  {
	spf_init( &ier ); 
    } 

    npf_open( filnam, FALSE, &fptr, &flen, iret );
    if ( *iret >= 0 ) { 
       spf_read( fptr, filnam, (int)flen, iret );
    }    
        
    if ( fptr != NULL ) {
       spf_close( fptr, &ier );
    }
}
