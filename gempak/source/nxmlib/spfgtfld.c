#include "geminc.h"
#include "gemprm.h"
#include "spfcmn.h"

void spf_gtfld ( char *tag, char *data, int *iret )
/************************************************************************
 * spf_gtfld								*
 *									*
 * This function returns the data associated with the given tag in the  *
 * "_spfBuffer". If the same tag contains different data in the SPF     *
 * file, the data associated with the first occurrence of the tag is    *
 * returned. If no data are associated with the tag, a default "NONE"   *
 * value is returned.							*
 *									*
 * spf_gtfld ( tag, data, iret )					*
 *									*
 * Input parameters:							*
 *      *tag		char		Tag name			*
 *									*
 * Output parameters:							*
 *      *data		char		Data string			*
 *	*iret		int		Return code			*
 *					3 - No data read with the tag 	*
 *				        2 - SPF Buffer is not loaded	*
 *					0 - Normal 			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	6/01	create						*
 * J. Wu/GSC	8/01	handle strings with all white spaces		*
 ***********************************************************************/
{

    char *def = DEF_DATA, data1[TAGDATA_BUF];
    int  ier, nn, nc;
                    
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    strcpy( data, def );
    
    if ( _initSPF != G_TRUE ) spf_init( iret ); 

    if ( _spfBuffer == (char *)NULL ) {
    
        *iret = 2;   /* Buffer not loaded */
    
    }
    else {
    
        cst_gtag ( tag, _spfBuffer, def, data1, &ier );
	               
	if ( ier == 3 ) {
	
	    *iret = 3;  /* No data associated with the tag */
	    
	}
	else { 
	
	    /* 
	     *  Chop off the leading/ending space in the data string. 
	     */                
	    cst_ldsp ( data1, data1, &nn, &ier );
	    cst_lstr ( data1, &nc, &ier );
	    if ( nc == 0 ) {
	        *iret = 3;
            }
	    else {
	        cst_ncpy ( data, data1, nc, &ier );	    
	    }
	}
	      
    }
    
}
