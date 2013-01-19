#include "geminc.h"
#include "gemprm.h"

void cfl_srch ( FILE *fptr, char *pattrn, int idir, int *iret )
/************************************************************************
 * cfl_srch								*
 *									*
 * This function positions the file at the first character of the	*
 * specified pattern.							*
 *									*
 * Valid values for direction of search include:			*
 *									*
 *	G_FSRCH		Forward						*
 *	G_BSRCH		Backward					*
 *									*
 * cfl_srch ( fptr, pattrn, idir, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	*pattrn		char		Pattern to locate		*
 *	idir		int		Direction of search		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   4 = Reached end of file	*
 *					   3 = Pattern not found	*
 *					  -6 = No file has been opened	*
 *					 -23 = Invalid search direction	*
 **									*
 * G. Krueger/EAI	3/96						*
 * G. Krueger/EAI	8/96	Changed iret value; Changed comments	*
 ***********************************************************************/
{
	int	bufsiz, nbin, iorig, lenpat, nread, iresult, istep,
		ibegin, iend, ipos, ier;
	long	lmov, lstart, lcurr;
	unsigned char	buffer[LLBSIZ];
/*---------------------------------------------------------------------*/
	*iret = 0;

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}

	lenpat = (int)strlen (pattrn);
	bufsiz = sizeof (buffer);
	lstart = ftell (fptr);

/*
 *	Set buffer fill parameters.
 */
	iorig = SEEK_CUR;
	nread = bufsiz;
	if ( idir == G_FSRCH ) {
	    lmov = 0;
	} else if ( idir == G_BSRCH ) {
	    lmov = (long)-bufsiz;
	    if ( -lmov > lstart ) {
		iorig = SEEK_SET;
		lmov = 0;
		nread = (int)lstart;
	    }
	} else {
	    *iret = -23;
	    return;
	}

	while ( *iret == 0 ) {
/*
 *	    Fill buffer from the file.
 */
	    cfl_seek ( fptr, lmov, iorig, iret );
	    if ( *iret != 0 ) return;

	    if ( nread <= 0 ) {
		nbin = 0;
	    } else {
		cfl_read ( fptr, nread, buffer, &nbin, iret );
		if ( *iret < 0 ) return;
	    }
/*
 *	    Search the buffer for the pattern.
 */
	    if ( idir == G_FSRCH ) {
		istep = 1;
		ibegin = 0;
		iend = nbin - lenpat;
	    } else {
		istep = -1;
		ibegin = nbin - lenpat;
		iend = 0;
	    }
	    ipos = ibegin;
	    while ( istep * (iend - ipos) >= 0 ) {
		if ( buffer[ipos] == pattrn[0] ) {
		    iresult = memcmp ( buffer + ipos, pattrn, (size_t)lenpat );
		    if ( iresult == 0 ) {
/*
 *			Found the string.  Position file pointer at start.
 */
			lmov = (long)(ipos - nbin);
			cfl_seek ( fptr, lmov, SEEK_CUR, iret );
			return;
		    }
		}
		ipos = ipos + istep;
	    }
	    if ( nbin <= lenpat || *iret == 4 ) *iret = 3;
/*
 *	    Set buffer fill parameters.
 */
	    lmov = (long)-lenpat;
	    iorig = SEEK_CUR;
	    nread = bufsiz;
	    if ( idir == G_BSRCH ) {
		lcurr = ftell (fptr);
		lmov = (long)(lenpat - (2 * bufsiz));
		if ( -lmov > lcurr ) {
		    iorig = SEEK_SET;
		    lmov = 0;
		    nread = (int)lcurr - bufsiz + 1;
		}
	    }
	}
/*
 *	String not found or error.  Return to original position.
 */
	cfl_seek ( fptr, lstart, SEEK_SET, &ier );
	if ( ier != 0 ) *iret = ier;
}
