#include "geminc.h"
#include "gemprm.h"


Boolean cfl_isdir ( char *cfile )
/************************************************************************
 * cfl_isdir								*
 *									*
 * This function reads the specified path and determines if it points   *
 * to a directory.  True is returned if it does.  Environment variables *
 * may be included in the cfile string.                               	*
 *									*
 * Booelan cfl_isdir ( *cfile )                                  	*
 *									*
 * Input parameters:							*
 *	*cfile		char		path string                   	*
 *									*
 * Output parameters:							*
 *			None						*
 * 									*
 * Return:								*
 *	cfl_isdir	Boolean		True = path is a directory	*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/02	initial coding				*
 ***********************************************************************/
{
    char		newname[LLPATH];
    int			ier;
    Boolean     	is_dir = FALSE;
    struct stat 	fstat;
/*---------------------------------------------------------------------*/

    if ( cfile != NULL ) {

	css_envr ( cfile, newname, &ier );

	if ( ier >= 0 ) {

	     if ( stat (newname, &fstat) == 0 ) {
                 is_dir = S_ISDIR ( fstat.st_mode );
	     }
	}
    }
	
    return (is_dir);
}


/*=====================================================================*/
