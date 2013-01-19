#include "geminc.h"
#include "gemprm.h"
#include "spfcmn.h"

void spf_read ( FILE *fptr, char *filnam, int flen, int *iret )
/************************************************************************
 * spf_read								*
 *									*
 * This function reads the contents of an SPF file into "_spfBuffer".	*
 *									*
 * spf_read ( fptr, filnam, flen, iret )				*
 *									*
 * Input parameters:							*
 *      *fptr		FILE		File pointer			*
 *	filnam		char*		file name			*
 *      flen		int		File size			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					4 - Reach the end of file	*
 *					0 - Normal			*
 *				       -4 - Failure to read SPF file	*
 *				       -6 - No file has been opened	*
 *				       -7 - Failure to allocate buffer	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	6/01	create						*
 * J. Wu/SAIC	8/01	add ability to handle comment lines		*
 * J. Wu/SAIC	4/02	add ability to handle longer lines		*
 * H. Zeng/SAIC	9/04	added a new argument filnam			*
 ***********************************************************************/
{
    int	    nbin, ier, ierr, quit;
    char    lineBuf[TAGDATA_BUF], dirnam[128], basnam[128];   
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
        
    if ( _initSPF != G_TRUE ) spf_init( &ier ); 
    
    if ( fptr == NULL ) {
    
        *iret = -6;
	return;
    
    }
    
    /*
     *  Allocate the space for the _spfBuffer and then read all file
     *  contents into it.
     */    
    spf_clnup( &ier );
        
    _spfBuffer = (char *) malloc( ( flen + 2*(MXFLSZ) + 1 ) * sizeof( char ) );
    
    if ( _spfBuffer == (char *)NULL ) {
        
	*iret = -7;
	return;
	
    }
    
    cfl_seek( fptr, 0, SEEK_SET, &ier );
    
    if ( ier < 0 ) {
        
	*iret = -4;        /* Failure to read */
	er_wmsg ( "CFL", &ier, " ", &ierr, 3, 1 );
    
    }
    else {    
            
        _spfBuffer[0] = '\0';

	quit = G_FALSE;
	while ( !quit ) {

	    cfl_trln ( fptr, sizeof(lineBuf), lineBuf, &ier );
        
	    if ( ier == 4 ) {
	        quit = G_TRUE;  /* EOF reached */
	    }
	    else if ( ier < 0 ) {	
	        *iret = -4;    /* Failure to read */
	        er_wmsg ( "CFL", &ier, " ", &ierr, 3, 1 );
	        quit = G_TRUE;
	    }
	    else {
	        cst_ldsp ( lineBuf, lineBuf, &nbin, &ier );
		if ( lineBuf[0] != '!' ) {		    		    
		    if ( lineBuf[0] == '<' ) {
		        strcat( _spfBuffer, "\n" );
		    }		    
		    strcat( _spfBuffer, lineBuf );
		}
	    }	    
	}

	strcat ( _spfBuffer, "<file_name>" );

        cfl_path (filnam, dirnam, basnam, &ier);
	strcat ( _spfBuffer, basnam	   );
				     
    }

}
