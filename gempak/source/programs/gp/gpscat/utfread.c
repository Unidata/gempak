#include "geminc.h"
#include "gemprm.h"

void utf_read ( FILE *fp, long nbytes, unsigned char *buffer, 
		long *nbread, int *iret )
/************************************************************************
 * utf_read								*
 *									*
 * This function reads the content of a UTF file into a buffer.		*
 *									*
 * utf_read ( fp, nbytes, buffer, nbread, iret )			*
 *									*
 * Input parameters:							*
 *	*fp		FILE		UTF file pointer		*
 *	nbytes		long		Size of UTF file		*
 *									*
 * Output parameters:							*
 *	*buffer		unsigned char	Contents of UTF file		*
 *	*nbread		long		Number of bytes read		*
 *	*iret		int		Return code			*
 *					 -3 = no buffer space allocated	*
 *					 -4 = read failure		*
 *					-11 = no file has been opened	*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96						*
 ***********************************************************************/
{
    int			nbyt, size, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;
    *nbread = 0;

/*
**  Read the contents of the file. 
*/
    if ( fp == 0 )
	*iret = -11;
    else if ( nbytes <= 0 )
	*iret = -3;
    else {
    	nbyt = (int) nbytes;
	cfl_read(fp, nbyt, buffer, &size, &ier);
	*nbread = (long int) size;
    	if ( ier != 0 ) {
	    *nbread = 0;
	    *iret = -4;
	}
    }

}
