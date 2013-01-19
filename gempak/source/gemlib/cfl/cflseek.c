#include "geminc.h"
#include "gemprm.h"

void cfl_seek ( FILE *fptr, long loffset, int iorigin, int *iret )
/************************************************************************
 * cfl_seek								*
 *									*
 * This function positions the file according to the specified offset	*
 * from the specified origin.						*
 *									*
 * Valid values for offset origin include:				*
 *									*
 *	SEEK_SET	Beginning of file				*
 *	SEEK_CUR	Current position				*
 *	SEEK_END	End of file					*
 *									*
 * cfl_seek ( fptr, loffset, iorigin, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	loffset		long		Offset to new file position	*
 *	iorigin		int		Origin for offset		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -21 = Invalid seek origin	*
 *					 -22 = Seek beyond file		*
 *					  -6 = No file has been opened	*
 **									*
 * G. Krueger/EAI	3/96						*
 * G. Krueger/EAI	8/96	Changed iret values; Changed comments 	*
 ***********************************************************************/
{
	int	ier, ier1;
	long	lstart, lend;
/*---------------------------------------------------------------------*/
	*iret = 0;
	ier1 = 0;

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}
/*
 *	Locate end of file, and go back for specified seek.
 */
	lstart = ftell (fptr);
	ier1 = fseek ( fptr, 0L, SEEK_END );
	lend = ftell ( fptr );
	ier1 = fseek ( fptr, lstart, SEEK_SET );
/*
 *	Seek to specified location, if not beyond end.
 */
	if ( iorigin == SEEK_CUR ) {
	    if ( (loffset > lend - lstart) || (loffset < -lstart) ) {
		*iret = -22;
		return;
	    }
	} else if ( iorigin == SEEK_SET ) {
	    if ( (loffset > lend) || (loffset < 0) ) {
		*iret = -22;
		return;
	    }
	} else if ( iorigin == SEEK_END ) {
	    if ( (loffset > 0) || (loffset < -lend) ) {
		*iret = -22;
		return;
	    }
	} else {
	    *iret = -21;
	    return;
	}

	ier1 = fseek ( fptr, loffset, iorigin );

	if ( ier1 != 0 ) {
	    cfl_iret ( errno, iret, &ier );
	} 
}
