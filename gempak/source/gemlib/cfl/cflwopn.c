#include "geminc.h"
#include "gemprm.h"

FILE *cfl_wopn ( char *filnam, int *iret )
/************************************************************************
 * cfl_wopn								*
 *									*
 * This function opens a file for writing.  If the file exists, the	*
 * previous contents are discarded.  To modify a file, use CFL_AOPN or	*
 * CFL_UOPN instead of this function.					*
 *									*
 * FILE *cfl_wopn ( filnam, iret )						*
 *									*
 * Input parameters:							*
 *	*filnam		char		File name			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	*cfl_wopn	FILE		File pointer			*
 **									*
 * G. Krueger/EAI	 3/96						*
 * G. Krueger/EAI	 8/96	Match with FL library			*
 * S. Law/GSC		05/00	changed to use file size defines	*
 ***********************************************************************/
{
    int		ier;
    char	newname[FILE_FULLSZ];
    FILE	*fptr;
/*---------------------------------------------------------------------*/

    *iret = 0;

    css_envr ( filnam, newname, &ier );

    fptr = fopen ( newname, "w" );
    if ( fptr == NULL ) {
	cfl_iret ( errno, iret, &ier );
    }

    return fptr;
}
