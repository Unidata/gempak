#include "geminc.h"
#include "gemprm.h"

void cst_srch ( int ibeg, int iend, char *string, char *text, 
						int *ipos, int *iret )
/************************************************************************
 * cst_srch								*
 *									*
 * This function will search through the given TEXT for the given	*
 * STRING. It will search forward if ibeg is greater that iend, or	*
 * backward if iend is greater. It will return the position in the	*
 * text where the string is found, or a missing value if the string	*
 * was not found.							*
 *									*
 * cst_srch ( ibeg, iend, string, text, ipos, iret )			*
 *									*
 * Input parameters:							*
 *	ibeg		int		Beginning position		*
 *	iend		int		End position			*
 *	*string		char		Search string			*
 *	*text		char		Text to search			*
 *									*
 * Output parameters:							*
 *	*ipos		int		Position of string		*
 *	*iret		int		Return code			*
 *					 -4 = string not found		*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 6/94						*
 * L. Williams/EAI	 7/94	Reformat header				*
 * S. Jacobs/NMC	 7/94	Added error for not found		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * T. Piper/SAIC	 5/02	Fixed ABR; return if no text string	*
 ***********************************************************************/
{
	int	i, istart, istop, incr, len, txtlen;

/*---------------------------------------------------------------------*/
/*
 *	Initialize the position and return code.
 */
	*ipos = IMISSD;
	*iret = -4;

/*
 *	Get the length of the text and search strings.
 */
	txtlen = (int)strlen ( text );
	if ( txtlen == 0 ) return;
	len = (int)strlen ( string );
/*
 *	Set the start and stop positions and the search direction.
 *	Check for begin and end out of range of text character array.
 */
	if  ( ibeg > iend ) {
	    if  ( iend < 0 )  iend = 0;
	    if  ( ibeg > txtlen )  ibeg = txtlen;
	    istart = ibeg - len + 1;
	    istop  = iend;
	    incr   = -1;
	}
	else {
	    if  ( ibeg < 0 )  ibeg = 0;
	    if  ( iend > txtlen )  iend = txtlen;
	    istart = ibeg;
	    istop  = iend - len + 1;
	    incr   = 1;
	}
/*
 *	Search the text for the string, as long as the index is between
 *	the start and stop values. If the string is found, set the
 *	position and return.
 */
	for ( i = istart; ( ( istart <= i && i <= istop  ) ||
			    ( istop  <= i && i <= istart ) );
	      i += incr ) {
	    if  ( strncmp ( &text[i], string, (size_t)len ) == 0 ) {
		*ipos = i;
		*iret = 0;
		return;
	    }
	}

}
