#include "geminc.h"
#include "gemprm.h"

void cst_inch ( int intg, char *str, int *iret )
/************************************************************************
 * cst_inch								*
 *									*
 * This function encodes an integer in a character string.		* 
 *									*
 * cst_inch ( intg, str, iret )						*
 *									*
 * Input parameters:							*
 *	intg		int		Integer				*
 *									*
 * Output parameters:							*
 *	*str		char		Encoded value			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96	Created					*
 * T. Piper/GSC		10/98	Prolog update				*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = 0;

	/*
	 * encode number into string
	 */
	sprintf( str, "%d", intg );

}
