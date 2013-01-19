#include "geminc.h"
#include "gemprm.h"

void awrfil ( char fname[], int *nbytes, unsigned char barr[], int *iret )
/************************************************************************
 * awrfil								*
 *									*
 * This function calls the inquire function to determine the record	*
 * length before calling the open and read functions.			*
 *									*
 * awrfil ( fname, nbytes, barr, iret )					*
 *									*
 * Input parameters:							*
 *	fname[] 	char		File name			*
 *									*
 * Output parameters:							*
 *	*nbytes		int		Length of record in bytes	*
 *	barr[]	 	unsigned char	Array of bytes			*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		 7/98						*
 * T. Piper/GSC		11/98	Updated prolog				*
 * A. Hardy/GSC		 3/99	Updated prolog				*
 ***********************************************************************/
{
	FILE	*fptr;
        char    *defdir, newfil[160];
	int     ierr;
	long    flen;
/*---------------------------------------------------------------------*/

	*iret = 0;

	/*
	 *  Obtain the record length of the input file. 
	 */

	defdir = NULL;
        
	cfl_inqr(fname, defdir, &flen, newfil, &ierr);

	/*
	 *  Open the input file.
	 */

        fptr = cfl_ropn(fname, defdir, &ierr);

	/*
	 *  Read the file.
	 */

        cfl_read(fptr, (int)flen, barr, nbytes, &ierr); 

	/*
	 *  Close the file.
	 */
	cfl_clos(fptr, &ierr);
}
