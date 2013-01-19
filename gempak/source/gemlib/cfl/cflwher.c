#include "geminc.h"
#include "gemprm.h"

void cfl_wher ( FILE *fptr, long *lfaddr, int *iret )
/************************************************************************
 * cfl_wher								*
 *									*
 * This function determines the current position in the specified file.	*
 *									*
 * cfl_wher ( fptr, lfaddr, iret )					*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *									*
 * Output parameters:							*
 *	*lfaddr		long		Current file position		*
 *	*iret		int		Return code			*
 *					 -6 = No file opened		*
 **									*
 * G. Krueger/EAI	3/96						*
 ***********************************************************************/
{
	int	ier;
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}

	*lfaddr = ftell (fptr);

	if ( *lfaddr < 0L ) {
	    cfl_iret ( errno, iret, &ier );
	}
}

