#include "geminc.h"
#include "gemprm.h"

void cfl_rdln ( FILE *fptr, int bufsiz, char *buffer, int *iret )
/************************************************************************
 * cfl_rdln								*
 *									*
 * This function reads a line of text from a file into BUFFER.  It	*
 * terminates the line with a null character.				*
 *									*
 * cfl_rdln ( fptr, bufsiz, buffer, iret )				*
 *									*
 * Input parameters:							*
 *	*fptr		FILE		File pointer			*
 *	bufsiz		int		Size of buffer			*
 *									*
 * Output parameters:							*
 *	*buffer		char		Text string from file		*
 *	*iret		int		Return code			*
 *					  4 = Reached end of file	*
 *					  2 = Line exceeds buffer size	*
 *					 -3 = Read failure		*
 *					 -6 = No file has been opened	*
 **									*
 * G. Krueger/EAI	3/96						*
 * F. J. Yen/NCEP	4/99	Changed inchar from char to int.	*
 ***********************************************************************/
{
	int	ier, lenbuf, inchar;
	char	*string;
/*---------------------------------------------------------------------*/
	*iret = 0;
	inchar = ' ';

	if ( fptr == NULL ) {
	    *iret = -6;
	    return;
	}

	string = fgets ( buffer, bufsiz, fptr );

	if ( string != NULL ) {
	    lenbuf = strlen (buffer);
/*
 *	    If newline not reached, position file after next newline.
 */
	    if ( lenbuf == (bufsiz - 1) ) {
		while ( (inchar != EOF) && (inchar != '\n') ) {
		    inchar = fgetc (fptr);
		}
		if ( ferror (fptr) ) {
		    cfl_iret ( errno, iret, &ier );
		} else {
		    *iret = 2;
		}
	    }
	    if ( buffer[lenbuf - 1] == '\n' ) buffer[lenbuf - 1] = '\0';
	} else {
	    *iret = -3;
	}

	if ( (feof (fptr)) && (! ferror (fptr)) ) *iret = 4;
}
