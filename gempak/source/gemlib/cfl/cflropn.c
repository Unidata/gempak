#include "geminc.h"
#include "gemprm.h"

FILE *cfl_ropn ( char *filnam, char *defdir, int *iret )
/************************************************************************
 * cfl_ropn								*
 *									*
 * This function opens a file for reading.				*
 *									*
 * The file is located by searching in the following order:		*
 *									*
 *	1. filnam (as given)						*
 *	2. defdir/filnam						*
 *									*
 * FILE *cfl_ropn ( filnam, defdir, iret )				*
 *									*
 * Input parameters:							*
 *	*filnam		char		File name			*
 *	*defdir		char		Default directory		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	*cfl_ropn	FILE		File pointer			*
 **									*
 * G. Krueger/EAI	 3/96						*
 * G. Krueger/EAI	 8/96	Match with FL library			*
 * S. Law/GSC		05/00	changed to use file size defines	*
 ***********************************************************************/
{
    FILE	*fptr;
    int		ier;
    long	lflen;
    char	fullname[FILE_FULLSZ];
/*---------------------------------------------------------------------*/

    *iret = 0;
    fptr = NULL;

    cfl_inqr ( filnam, defdir, &lflen, fullname, iret );

    if ( *iret == 0 ) {
	fptr = fopen ( fullname, "r" );
	if ( fptr == NULL ) {
	    cfl_iret ( errno, iret, &ier );
	}
    }

    return fptr;
}
