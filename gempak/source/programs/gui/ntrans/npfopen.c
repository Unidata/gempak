#include "geminc.h"
#include "gemprm.h"
#include "interface.h"

void npf_open ( char *filnam, Boolean crt, FILE **fptr, long *flen, int *iret )
/************************************************************************
 * npf_open								*
 *									*
 * This function opens an SPF file for update. If the file doesn't 	*
 * exist and flag "crt" is TRUE, then it will be created. If the file   *
 * suffix is not ".npf", it will be added. If the user has only read    *
 * permission to the NPF file, it will be opened as "read-only".	*
 *									*
 * npf_open ( filnam, crt, fptr, flen, iret )				*
 *									*
 * Input parameters:							*
 *      *filnam		char		File name with full path	*
 *      crt		Boolean		Flag, create a new file or not	*
 *									*
 * Output parameters:							*
 *      **fptr		FILE		Pointer to a file pointer	*
 *      *flen		long		File size			*
 *	*iret		int		Return code			*
 *					1 - File opened as READ_ONLY	*
 *					0 - Normal 			*
 *				       -1 - File does not exist		*
 *				       -2 - File cannot be opened	*
 *				       -3 - Failure to create NPF file	*
 *									*
 **									*
 * Log:									*
 * C. Bailey/HPC	5/05	Modifiy from spf_open			*
 * T. Piper/SAIC	06/06	Declared *flen long 			*
 ***********************************************************************/
{
    int	    hlen, ipos, ier, ierr;
    char    newfil[FILE_FULLSZ];
    char    filepart[MXFLSZ], pathpart[LLPATH];
    
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    *flen = 0;
       
/*
 *  Check if the file suffix is ".npf". If not, add it. 
 */
    cfl_path ( filnam, pathpart, filepart, &ier ) ;   
    cst_srch ( 0, strlen(filepart), ".npf", filepart, &ipos, &ier );
    if ( ier == -4 ) {
        strcat (filnam, ".npf");
    }
    
/*
 *  Check the file status, if it exists, open it; otherwise, create
 *  it if authorized. 
 */ 
    cfl_inqr( filnam, NULL, flen, newfil, &ier ); 
    
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
       
            *fptr = npf_create( filnam, &hlen, iret ); 
	    *flen = (long)hlen;	     	
	}
    }
}
