#include "geminc.h"
#include "gemprm.h"

FILE *cfl_uopn ( char *filnam, int *iret )
/************************************************************************
 * cfl_uopn								*
 *									*
 * This function opens a file for updating.  If the file does not	*
 * exist, it is not created.						*
 *									*
 * FILE *cfl_uopn ( filnam, iret )					*
 *									*
 * Input parameters:							*
 *	*filnam		char		File name			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	*cfl_uopn	FILE		File pointer			*
 **									*
 * G. Krueger/EAI	3/96						*
 * G. Krueger/EAI	8/96	Match with FL library			*
 * S. Law/GSC		05/00	changed to use file size defines	*
 ***********************************************************************/
{
    int		ier;
    char	newname[FILE_FULLSZ];
    FILE	*fptr;
/*---------------------------------------------------------------------*/

    *iret = 0;

    css_envr ( filnam, newname, &ier );

    fptr = fopen ( newname, "r+" );
    if ( fptr == NULL ) {
	cfl_iret ( errno, iret, &ier );
    }

    return fptr;
}
