#include "geminc.h"
#include "gemprm.h"

void cfl_writ ( FILE *fptr, int nbytes, unsigned char *buffer, int *iret )
/************************************************************************
 * cfl_writ								*
 *									*
 * This function writes the specified number of bytes from the buffer	*
 * to the given file.							*
 *									*
 * cfl_writ ( fptr, nbytes, buffer, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	nbytes		int		Number of bytes to write	*
 *	*buffer		unsigned char	Data to write to file		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -6 = No file has been opened	*
 **									*
 * G. Krueger/EAI	3/96						*
 ***********************************************************************/
{
	int	ier, nbout;
/*---------------------------------------------------------------------*/
	*iret = 0;

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}
/*
 *	Write the record.
 */
	nbout = (int)fwrite ( buffer, sizeof(unsigned char), (size_t)nbytes, fptr );

	if ( nbout != nbytes ) {
	    cfl_iret ( errno, iret, &ier );
	}
}
