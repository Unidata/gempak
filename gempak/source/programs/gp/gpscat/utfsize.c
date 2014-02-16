#include "geminc.h"
#include "gemprm.h"


void utf_size ( char *filnam, long *nbytes, int *iret )
/************************************************************************
 * utf_size								*
 *									*
 * This function determines the size of a UTF file.			*
 *									*
 * utf_size ( filnam, nbytes, iret )					*
 *									*
 * Input parameters:							*
 *	*filnam		char		UTF filename			*
 *									*
 * Output parameters:							*
 *	*nbytes		long		Size of UTF file		*
 *	*iret		int		Return code			*
 *					 -2 = file does not exist	*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96						*
 ***********************************************************************/
{
    int			ier;
    long		size;
    char		path[133], string[2];
/*---------------------------------------------------------------------*/
    *iret = 0;
    strcpy(string, " ");

/*
**  Inquire if the file exists and the size of the file.
*/
    cfl_inqr(filnam, string, &size, path, &ier);
    *nbytes = size;
    if ( ier != 0 )
	*iret = -2;
}
