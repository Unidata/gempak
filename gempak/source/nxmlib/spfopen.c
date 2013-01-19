#include "geminc.h"
#include "gemprm.h"

void spf_open ( char *filnam, Boolean crt, FILE **fptr, int *flen, int *iret )
/************************************************************************
 * spf_open								*
 *									*
 * This function opens an SPF file for update. If the file doesn't 	*
 * exist and flag "crt" is TRUE, then it will be created. If the file   *
 * suffix is not ".spf", it will be added. If the user has only read    *
 * permission to the SPF file, it will be opened as "read-only".	*
 *									*
 * spf_open ( filnam, crt, fptr, flen, iret )				*
 *									*
 * Input parameters:							*
 *      *filnam		char		File name with full path	*
 *      crt		Boolean		Flag, create a new file or not	*
 *									*
 * Output parameters:							*
 *      **fptr		FILE		Pointer to a file pointer	*
 *      *flen		int		File size			*
 *	*iret		int		Return code			*
 *					1 - File opened as READ_ONLY	*
 *					0 - Normal 			*
 *				       -1 - File does not exist		*
 *				       -2 - File cannot be opened	*
 *				       -3 - Failure to create SPF file	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		6/01	create					*
 * J. Wu/GSC		6/01	use cst_srch()				*
 * J. Wu/GSC		7/01	allow access to read-only SPF files	*
 * E. Safford/SAIC	02/06	fix cfl_inqr call, make 3rd param long  *
 ***********************************************************************/
{
    int	    ipos, ier, ierr;
    char    newfil[FILE_FULLSZ];
    char    filepart[MXFLSZ], pathpart[LLPATH];
    long    localFlen; 
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    *flen = 0;
       
    /*
     *  Check if the file suffix is ".spf". If not, add it. 
     */
    cfl_path ( filnam, pathpart, filepart, &ier ) ;   
    cst_srch ( 0, strlen(filepart), ".spf", filepart, &ipos, &ier );
    if ( ier == -4 ) {
        strcat (filnam, ".spf");
    }
    
    /*
     *  Check the file status, if it exists, open it; otherwise, create
     *  it if authorized. 
     */ 
    cfl_inqr( filnam, NULL, &localFlen, newfil, &ier ); 
    *flen = ( int ) localFlen;

    if ( ier == 0 ) { 

        *fptr = cfl_aopn( filnam, &ierr );
            
	if ( *fptr == NULL ) {
                
            *fptr = cfl_ropn( filnam, NULL, &ierr );
	    
	    if ( *fptr == NULL ) {
	        *iret = -2;
	        er_wmsg ( "CFL", &ierr, filnam, &ier, 3, strlen( filnam ) );
	    }
	    else {
	       *iret = 1; 
	    }
	
	}    

    }
    else {
    
        *iret = -1;
	
	if ( crt == TRUE ) {
       
            *fptr = ( FILE *) spf_create( filnam, flen, iret ); 
	     	
	}
	         
    }
               
}
