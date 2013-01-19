#include "geminc.h"
#include "gemprm.h"

void cfl_read ( FILE *fptr, int nbytes, unsigned char *buffer, int *nbin,
		int *iret )
/************************************************************************
 * cfl_read								*
 *									*
 * This function reads the specified number of bytes from the given	*
 * file into the buffer.						*
 *									*
 * cfl_read ( fptr, nbytes, buffer, nbin, iret )			*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	nbytes		int		Number of bytes to read		*
 *									*
 * Output parameters:							*
 *	*buffer		unsigned char	Data from file			*
 *	*nbin		int		Number of bytes read		*
 *	*iret		int		Return code			*
 *					  4 = Reached end of file	*
 *					 -3 = Read failure		*
 *					 -6 = No file has been opened	*
 **									*
 * G. Krueger/EAI	3/96						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	*iret = 0;

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}
/*
 *	Read the record.
 */
	*nbin = fread ( buffer, sizeof (unsigned char), nbytes, fptr );

	if ( *nbin != nbytes ) {
	    if ( feof (fptr) ) {
		*iret = 4;
	    } else {
		*iret = -3;
	    }
	}
}
