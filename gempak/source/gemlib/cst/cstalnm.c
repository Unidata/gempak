#include "geminc.h"
#include "gemprm.h"


void cst_alnm ( char ch, int *type, int *iret )
/************************************************************************
 * cst_alnm								*
 *									*
 * This subroutine determines whether a character is a letter, number	*
 * or non-alphanumeric character.					*
 *									*
 * cst_alnm ( ch, type, iret )						*
 *									*
 * Input parameters:							*
 *	ch		char		Input character			*
 *									*
 * Output parameters:							*
 *	*type		int		Character type			*
 *					  0 = non alphanumeric		*
 *					  1 = letter			*
 *					  2 = number			*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = 0;
	*type = 0;

	if( isalpha( ch ) ) {
	   /*
	    * character is a letter.
	    */
	   *type = 1;
	}
	else {
	   if( isdigit( ch ) ) {
	      /*
	       * character is a number
	       */
	      *type = 2;
	   }
	}

}
