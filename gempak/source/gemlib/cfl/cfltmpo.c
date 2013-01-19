#include "geminc.h"
#include "gemprm.h"

FILE *cfl_tmpo ( int *iret )
/************************************************************************
 * cfl_tmpo								*
 *									*
 * This function opens a temporary file for writing.  The system	*
 * generates a file name internally.  The file will be automatically	*
 * removed when closed.							*
 *									*
 * FILE *cfl_tmpo ( iret )						*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	*cfl_tmpo	FILE		File pointer			*
 **									*
 * G. Krueger/EAI	3/96						*
 ***********************************************************************/
{
	FILE	*fptr;
	int	ier;
/*---------------------------------------------------------------------*/
	*iret = 0;
/*
 *	Try to open the file.
 */
	fptr = tmpfile ( );

	if ( fptr == NULL ) {
	    cfl_iret ( errno, iret, &ier );
	}

	return fptr;
}
