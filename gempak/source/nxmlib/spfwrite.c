#include "geminc.h"
#include "gemprm.h"
#include "spfcmn.h"

void spf_write ( FILE *fptr, char *tag, char *data, int *iret )
/************************************************************************
 * spf_write								*
 *									*
 * This function writes the given tag and data into an SPF file.	*
 *									*
 * spf_write ( fptr, tag, data, iret )					*
 *									*
 * Input parameters:							*
 *      *fptr		FILE		File pointer			*
 *      *tag		char		Tag name			*
 *      *data		char		Data string			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 - Normal			*
 *				       -5 - Failure to wirte to file	*
 *				       -6 - No file has been opened	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	 6/01	create						*
 * J. Wu/GSC	 4/02	adjust output format & allow comment line	*
 * J. Wu/SAIC	 6/03	allow escaped '<' in the data string		*
 * T. Lee/SAIC	10/04	correct bin hours output to SP files		*
 ***********************************************************************/
{
    char    buffer[TAGDATA_BUF], value[TAGDATA_BUF];
    int	    bufsiz, ier, ierr, maxsiz = TAGDATA_BUF;
            
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    if ( fptr == NULL ) {
        
	*iret = -6;
        return;
	
    }
        
    /*
     * Format the tag and data.
     */ 
    if ( data[0] == '!' ) { 
        strcpy( buffer, data );    
    }
    else {
        if ( strlen(tag) > (size_t)20 ) {
            strcpy( value, "\t" );
        }    
        else if ( strlen(tag) > (size_t)13 ) {
            strcpy( value, "\t\t" );
        }
        else {
            strcpy( value, "\t\t\t" );     
        }
	
        strcat( value, data );     
	
	buffer[0] = '\0';
	cst_stag ( tag, value, &maxsiz, buffer, &ier );
    
    }
    
    strcat( buffer, "\n" );
    
    bufsiz = (int)(strlen( buffer ) * sizeof( char ));
   
    cfl_seek( fptr, 0, SEEK_END, &ier );
    
    if ( ier < 0 ) {
        
	*iret = -5;        /* Failure to write */
	er_wmsg ( "CFL", &ier, " ", &ierr, 3, 1 );
    
    }
    else {    
            
        cfl_writ( fptr, bufsiz, (unsigned char *)buffer, &ier );
	
	if ( ier < 0 ) {
	
	    *iret = -5;      /* Failure to write */
 	    er_wmsg ( "CFL", &ier, " ", &ierr, 3, 1 );
	    	
	}           	    		
	
    }

}
