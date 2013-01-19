#include "geminc.h"
#include "gemprm.h"

void cst_nocc ( const char *str, char srchc, int fndocc, int sstate, 
						int *nocc, int *iret )
/************************************************************************
 * cst_nocc								*
 *									*
 * This subroutine finds the Nth occurrence of a character in a string.	*
 * The comparison is case sensitive unless sensitivity state is set to	*
 * zero.								*
 *									*
 * cst_nocc ( str, srchc, fndocc, sstate, nocc, iret )			*
 *									*
 * Input parameters:							*
 *	*str		const char	String to be searched		*
 *	srchc		char		Search character		*
 *	fndocc		int		Occurrence to find		*
 *	sstate		int		Sensitivity state		*
 *					  default = case sensitive	*
 *					  0 = non sensitive		*
 *									*
 * Output parameters:							*
 *	*nocc		int		Position of Nth occurrence	*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					 -5 = Nth occurrence not found	*
 *					 -6 = invalid occurrence number	*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 * G. Krueger/EAI	10/97	Rewritten to remove MALLOC; Cleanup	*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * S. Jacobs/NCEP	 2/98	Changed to return position as an integer*
 ***********************************************************************/
{
int	last_occ;
char	tmpsc;
long	straddr;
/*---------------------------------------------------------------------*/
	*iret = 0;
	last_occ = fndocc;

	/*
	 * check for valid Nth occurrence
	 */
	if( fndocc <= 0 ) {
	   *nocc = 0;
	   *iret = -6;
	   return;
	}

	/*
	 * check sensitivity state
	 */
	straddr = (long)str;
	if( sstate == 0 ) {
	    /*
	     * case insensitive search loop
	     */
	    tmpsc = toupper( srchc );
	    while( ( *str ) && ( last_occ > 0 ) ) {
		if( toupper(*str) == tmpsc )
		    --last_occ;
		++str;
	    }
	} else {
	    /*
	     * case sensitive ( default ) search loop
	     */
	    tmpsc = srchc;
	    while( ( *str ) && ( last_occ > 0 ) ) {
		if( *str == tmpsc )
		    --last_occ;
		++str;
	    }
	}

	/*
	 * check if the Nth occurrence was found
	 */
	if( last_occ != 0 ) {
	   *iret = -5;
	   *nocc = 0;
	}
	else
	   *nocc = (long)(--str) - straddr;

}
