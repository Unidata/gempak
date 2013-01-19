#include "geminc.h"
#include "gemprm.h"

#include "mkdirs_open.h"

FILE *cfl_aopn ( char *filnam, int *iret )
/************************************************************************
 * cfl_aopn								*
 *									*
 * This function opens a file for appending.  If the file does not	*
 * exist, it is created.  All writes are forced to the end of the file.	*
 *									*
 * FILE *cfl_aopn ( filnam, iret )						*
 *									*
 * Input parameters:							*
 *	*filnam		char		File name			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	*cfl_aopn	FILE		File pointer			*
 **									*
 * G. Krueger/EAI	 3/96						*
 * G. Krueger/EAI	 8/96	Match with FL library			*
 * S. Law/GSC		05/00	changed to use file size defines	*
 * S. Chiswell/Unidata	11/02	Added directory creation		*
 ***********************************************************************/
{
    FILE	*fptr;
    int	ier;
    char	newname[FILE_FULLSZ];
/*---------------------------------------------------------------------*/

    *iret = 0;

    css_envr (filnam, newname, &ier);

    ier = diraccess(newname,  (R_OK | W_OK), !0);

    fptr = fopen (newname, "a+");
    if (fptr == NULL) {
	cfl_iret (errno, iret, &ier);
    }

    return fptr;
}
