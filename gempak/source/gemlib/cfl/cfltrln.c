#include "geminc.h"
#include "gemprm.h"

#define	LENBUF	256

void cfl_trln ( FILE *fptr, int bufsiz, char *buffer, int *iret )
/************************************************************************
 * cfl_trln								*
 *									*
 * This function reads a line of data from a GEMPAK table file into	*
 * BUFFER.  It terminates the line after the last non-blank character	*
 * with a null character.  This	function skips over comments and 	*
 * blank lines.								*
 *									*
 * A GEMPAK table file is a text file that may have comment records	*
 * within the file.  A comment record is a record where the first	*
 * non-blank character is an exclamation point.				*
 *									*
 * This routine has an internal buffer size of LENBUF=256.  If the	*
 * file which is being read contains records longer than 256, the	*
 * chances are great that the file is not a GEMPAK table file and an	*
 * error is returned.							*
 *									*
 * cfl_trln ( fptr, bufsiz, buffer, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE	File pointer				*
 *	bufsiz		int	Size of buffer				*
 *									*
 * Output parameters:							*
 *	*buffer		char	Text string from file			*
 *	*iret		int	Return code				*
 *				  0 = Normal; full record returned	*
 *				  1 = Record length exceeds passed-in	*
 *					buffer size; 			*
 *					partial record returned		*
 *				  4 = EOF reached			*
 *				 -3 = Read failure			*
 *				 -6 = No file has been opened		*
 *				-24 = Record > internal buffer size	*
 **									*
 * G. Krueger/EAI	 3/96						*
 * D.W.Plummer/NCEP	 2/98	minor rewrite to simplify 		*
 * D.W.Plummer/NCEP	 3/98	bug fix to end line w/ NULL		*
 ***********************************************************************/
{
int	found, lenbuf, nonblank, b1, ier;
char	readbuf[LENBUF], *string, tchar;
/*---------------------------------------------------------------------*/
	*iret = 0;

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}

	/*
	 *  Read records until a data record is found.
	 */
	found = 0;
	while ( found == 0 ) {

	    /*
	     *  Read the record and check for errors.
	     */
	    string = fgets ( readbuf, LENBUF, fptr );

	    if ( feof(fptr) && !ferror(fptr) )  {
		*iret = 4;
		return;
	    }
	    if ( string == NULL || ferror(fptr) ) {
		*iret = -3;
		return;
	    }

	    /*
	     *  Check the length of returned record.
	     */
	    lenbuf = (int)strlen (readbuf);
	    if ( lenbuf >= LENBUF )  {
		*iret = -24;
		return;
	    }

	    /*
	     *  Check first non-blank character.  
	     *  If valid, copy string and return; otherwise continue.
	     */
	    nonblank = (int)strspn (readbuf, " \t");
	    tchar = readbuf[nonblank];
	    if ( tchar != '!' && tchar != '\n' && tchar != '\0' ) {
		found = 1;
		if ( lenbuf < bufsiz )  {
		    strcpy (buffer, readbuf);
		    cst_lstr( buffer, &b1, &ier );
		    buffer[b1] = '\0';
		}
		else  {
		    *iret = 1;
		    b1 = bufsiz - 1;
		    strncpy( buffer, readbuf, (size_t)b1 );
		    buffer[b1] = '\0';
		}
	    }
	}
}
