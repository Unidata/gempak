#include "geminc.h"
#include "gemprm.h"

FILE *spf_create ( char *filnam, int *hlen, int *iret )
/************************************************************************
 * spf_create								*
 *									*
 * This function creates a new SPF file with an appropriate header. If  *
 * the file suffix is not ".spf", it will be added.			*
 *									*
 * Note: If the tag value contains a "<", it must be preceeded by a	*
 * backslash '\\' so as not to confuse it with the start of another tag.*
 *									*
 * FILE *spf_create ( filnam, hlen, iret )				*
 *									*
 * Input parameters:							*
 *      *filnam		char		file name with full path	*
 *									*
 * Output parameters:							*
 *	*hlen		int		Header length			*
 *	*iret		int		Return code			*
 *					0 - Normal			*
 *				       -3 - Failure to create SPF file	*
 * Return parameters:							*
 *	*spf_create	FILE		File pointer			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC	6/01	create						*
 * J. Wu/GSC	6/01	correct cfl_aopn to cfl_wopn			*
 * J. Wu/SAIC	6/03	add comments about escaped '<'			*
 * B. Yin/SAIC	3/04	changed css_gtim calling sequences		*
 * T. Piper/SAIC	03/06	Added GROUP_PERMS_SPF logic and actions	*
 ***********************************************************************/
{
    int	    ier, ierr, ipos, itype = 0;
    char    outstr[1024], perms_prefs[16], curdat[20], *usernam;
    char    filepart[MXFLSZ], pathpart[LLPATH];
    char    exn[] = "!\n", ex[] = "!",
            linf[] = "!----------------------------------------",
            linb[] = "----------------------------------------\n",
            str1[] = "This is a stored procedure file for nmap2.",
            str2[] = "Creation date:",
	    str3[] = "Created by:",
	    esp1[] = " Note: If the tag value contains a '<', it must be ",
	    esp2[] = "preceeded by a backslash \n!        '\\' so as not to ",
	    esp3[] = "confuse it with the start of another tag.";
    FILE    *fptr;
    mode_t  mode = S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH; /*rw_rw_r*/

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    fptr = (FILE *)NULL;
    
    /*
     *  If the file suffix is not ".spf", add it.
     */
    cfl_path ( filnam, pathpart, filepart, &ier ) ;   
    cst_srch ( 0, (int)strlen(filepart), ".spf", filepart, &ipos, &ier );
    if ( ier == -4 ) {
        strcat ( filnam, ".spf" );
    }
            
    /*
     *  Create the file for writing and add the header discription.
     */
    fptr = cfl_wopn( filnam, &ier );    
    
    if ( fptr == NULL ) {
        
	*iret = -3;          /* fail to create */
	er_wmsg ( "CFL", &ier, filnam, &ierr, 3, strlen( filnam ) );
        
    }    
    else {
	ctb_pfstr ( "GROUP_PERMS_SPF", perms_prefs, &ier);
	if ( strcmp(perms_prefs, "TRUE") == 0 ) {
	    chmod(filnam, mode);
	}
	/*
         *  Write an appropriate header to the file.
         */   
        css_gtim( &itype, curdat, &ier );
        usernam = getenv("LOGNAME");
    
        sprintf( outstr, "%s%s%s%s%s%s %s\n%s%s %s\n%s%s%s%s%s \n%s%s %s\t%s\n%s%s %s\t\t%s\n%s%s%s%s%s%s", 
             linf, linb, linf, linb,
	     exn, ex, filnam, 
	     exn, ex, str1, 
	     exn, ex, esp1, esp2, esp3, 
	     exn, ex, str2, curdat, 
	     exn, ex, str3, usernam, 
	     exn, linf, linb, linf, linb, exn );
       
	*hlen = (int)(strlen( outstr ) * sizeof( char ));
	cfl_writ( fptr, *hlen, (unsigned char*)outstr, &ier );
	
	if ( ier < 0 ) {
	
	    *iret = -3;     /* fail to create */
 	    er_wmsg ( "CFL", &ier, filnam, &ierr, 3, strlen( filnam ) );
            fptr = (FILE *)NULL;
	    	
	}           	    	
	     
    }
        
    return fptr;    
    
}
