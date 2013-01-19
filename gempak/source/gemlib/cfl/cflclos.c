#include "geminc.h"
#include "gemprm.h"

void cfl_clos ( FILE *fptr, int *iret )
/************************************************************************
 * cfl_clos								*
 *									*
 * This function closes the specified file.  Closing a file that has	*
 * been opened as a temporary file will cause the file to be removed.	*
 *									*
 * cfl_clos ( fptr, iret )						*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -6 = No file has been opened	*
 **									*
 * G. Krueger/EAI	3/96						*
 ***********************************************************************/
{
	int	ier;
/*---------------------------------------------------------------------*/
	*iret = 0;

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}
/*
 *	Close the file and check the status.
 */
	ier = fclose (fptr);

	if ( ier != 0 ) cfl_iret ( errno, iret, &ier );
}
