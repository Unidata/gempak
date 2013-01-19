#include "geminc.h" 
#include "gemprm.h"
#include "spfcmn.h"

int main ( void )
/************************************************************************
 * TESTSPF								*
 *									*
 * This program tests the CGEMLIB "SPF" library functions.		*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	 6/01	create						*
 * H. Zeng/SAIC  9/04	spf_read() para. list change			*
 ***********************************************************************/
{
    int    cont, numsub, iret, nbytes, ier, eliblen, errlen;
    char   select[LLSCRN], filnam[LLSCRN], newfil[FILE_FULLSZ]; 
    char   ans[LLSCRN], tag[LLSCRN], data[TAGDATA_BUF];
    char   errlib[3], errstr[2];
    long   lflen;
    FILE   *filptr;

/*---------------------------------------------------------------------*/
    
    cont = G_TRUE;
    filptr = NULL;
    strcpy( errlib, "SPF" );
    strcpy( errstr, " " );
    eliblen = strlen( errlib );
    errlen =  strlen( errstr );
             
    while ( cont == G_TRUE ) {
        
	printf ( "\n\n" );
	printf ( "   ? = Print HELP file\n\n");
	printf ( "   D - Dump a loaded SPF file\n\n");
        printf ( "   1 = SPF_CREATE   2 = SPF_OPEN   3 = SPF_READ\n" );
        printf ( "   4 = SPF_LOAD     5 = SPF_WRITE  6 = SPF_GTFLD\n" );
        printf ( "   7 = SPF_CLNUP    8 = SPF_CLOSE               \n" );
        printf ( "\n" );
        
	printf ( "Select a subroutine number or type EXIT: " );
        scanf ( " %s", select );
        
	numsub = 0;
	
	switch ( select[0] ) {

	    case '?':
 	        printf ("\n               *** HELP FILE ***\n\n");
	        printf ("  D - Dump a loaded SPF file\n\n");
	        printf ("  1 = spf_create - Create an SPF file\n" );
	        printf ("  2 = spf_open   - Open an SPF file\n" );
	        printf ("  3 = spf_read   - Read an opened SPF into buffer\n" );
	        printf ("  4 = spf_load   - Load an unopened SPF into buffer\n" );
	        printf ("  5 = spf_write  - Write a tag/data pair to an SPF\n" );
	        printf ("  6 = spf_gtfld  - Get data field associated with a tag\n" );
	        printf ("  7 = spf_clnup  - Clean up SPF buffer\n" );
	        printf ("  8 = spf_close  - Close an SPF file\n" );
	        printf ( "\n" );
           
	      break;
	      
	    case 'd':
            case 'D':
                 if ( _spfBuffer != (char *)NULL ) {
		     printf ( "\n%s\n", _spfBuffer );		 
		 }
		 else {
		     printf ( "\nPlease load the buffer first.\n" );
		 }
	      
	      break;
	
            case 'e':
            case 'E':
                cont = G_FALSE;
	      break;
	    
	    default:
                numsub = atoi ( select );
              break;
        
	}

/*---------------------------------------------------------------------*/
        if ( numsub == 1 ) {

	    if ( filptr != NULL ) {	    
	        printf ( "\nPlease close the opened file first.\n" );
	    }
	    else {
                printf ( "Enter the file name:\n" );
                scanf ( " %s", filnam );
            
	        cfl_inqr( filnam, NULL, &lflen, newfil, &ier );
                if ( ier != 0 ) {        
 	            strcat( filnam, ".spf" );
                    cfl_inqr( filnam, NULL, &lflen, newfil, &ier );
	        }
	    
	        if ( ier == 0 ) {
                    printf ( "\nCan't create, file %s already exists!\n", newfil );
                }
	        else {	    
		    nbytes = (int)lflen;                
	    	    filptr = (FILE *)spf_create ( filnam, &nbytes, &iret );
                    if ( iret == 0 ) {
		        printf ( "\nSPF_CREATE: iret = %d\n\n", iret );
		    }
		    else {
		        er_wmsg ( errlib, &iret, filnam, &ier, eliblen, strlen(filnam) );	
		    }		
	        }
	    }
	    
        }
	
/*---------------------------------------------------------------------*/
        if ( numsub == 2 ) {
	    
	    if ( filptr != NULL ) {	    
	        printf ( "\nPlease close the opened file first.\n" );
	    }
	    else {
	        printf ( "\nEnter the file name:\n" );
	        scanf ( " %s", filnam );
	        printf ( "Do you want to create it if the file doesn't exist?\n" );
	        scanf ( " %s", ans );

	        if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
	            spf_open ( filnam, TRUE, &filptr, &nbytes, &iret );
	        } 
	        else {
                    spf_open ( filnam, FALSE, &filptr, &nbytes, &iret );
	        }

                if ( iret == 0 ) {
		    printf ( "\nSPF_OPEN: iret = %d\n\n", iret );
		}
		else {
		    er_wmsg ( errlib, &iret, filnam, &ier, eliblen, strlen(filnam) );
                }
		
	    }
	    
	}

/*---------------------------------------------------------------------*/
        if ( numsub == 3 ) {

	    printf ( "Enter length in bytes:\n" );
	    scanf ( " %d", &nbytes );
	    
            spf_read ( filptr, filnam, nbytes, &iret );
                
            if ( iret == 0 ) {
                printf ( "\nSPF_READ: iret = %d\n\n", iret );
	    }
            else {
		er_wmsg ( errlib, &iret, errstr, &ier, eliblen, errlen );
	    }

        }

/*---------------------------------------------------------------------*/
        if ( numsub == 4 ) {
	    	    
	    if ( filptr != NULL ) {	    
	        printf ( "\nPlease close the opened file first.\n" );
	    }
	    else {
	        printf ( "\nEnter the file name:\n" );
	        scanf ( " %s", filnam );
	        
                spf_load ( filnam, &iret );
                
		if ( iret == 0 ) {	    
                    printf ( "\nSPF_LOAD: iret = %d\n\n", iret );
		}
		else {		
		    er_wmsg ( errlib, &iret, filnam, &ier, eliblen, strlen(filnam) );
		}
            }
	    
        }

/*---------------------------------------------------------------------*/
        if ( numsub == 5 ) {
	    	    
	    printf ( "\nEnter the tag name:\n" );
	    scanf ( " %s", tag );
	    printf ( "Enter the data string:\n" );
	    scanf ( " %s", data );
	    
            spf_write ( filptr, tag, data, &iret );

 	    if ( iret == 0 ) {	    
                 printf ( "\nSPF_WRITE: iret = %d\n\n", iret );
	    }
	    else {		
		 er_wmsg ( errlib, &iret, errstr, &ier, eliblen, errlen );		
	    }
			    	           
	}

/*---------------------------------------------------------------------*/
        if ( numsub == 6 ) {
	    
	    printf ( "Enter the tag name:\n" );
	    scanf ( " %s", tag );
	    
            spf_gtfld ( tag, data, &iret );
	    
 	    if ( iret == 0 ) {
                printf ( "\nSPF_GTFLD: iret = %d\n\n", iret );
	    }
	    else {
		er_wmsg ( errlib, &iret, tag, &ier, eliblen, strlen(tag) );
	    }

	    printf ( "\n Tag: %s \t Data: %s\n", tag, data );
	    	           
	}

/*---------------------------------------------------------------------*/
        if ( numsub == 7 ) {
	    	    
            spf_clnup ( &iret );

 	    if ( iret == 0 ) {
                printf ( "\nSPF_CLNUP: iret = %d\n\n", iret );
	    }
	    else {
		er_wmsg ( errlib, &iret, errstr, &ier, eliblen, errlen );
	    }

        }

/*---------------------------------------------------------------------*/
	if ( numsub == 8 ) {

	    spf_close ( filptr, &iret );
            filptr = NULL;	    	    
	        
 	    if ( iret == 0 ) {	    
                printf ( "\nSPF_CLOSE: iret = %d\n\n", iret );
	    }
	    else {
		er_wmsg ( errlib, &iret, errstr, &ier, eliblen, errlen );	     	    
	    }
	
	}

/*---------------------------------------------------------------------*/

    }

    if  ( filptr != NULL )  fclose ( filptr );
    return 0;
}
