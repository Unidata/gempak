#include "geminc.h"
#include "gemprm.h"

void cst_rlch ( float flnum, int np, char *str, int *iret )
/************************************************************************
 * cst_rlch								*
 *									*
 * This routine encodes an floating-point number in a character string.	* 
 *									*
 * cst_rlch ( flnum, np, str, iret )					*
 *									*
 * Input parameters:							*
 *	flnum		float		Floating-point number		*
 *	np		int		Number of decimal points	*
 *									*
 * Output parameters:							*
 *	*str		char		Encoded value			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = np <= 0			*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = 0;

	/*
	 * check the number of decimal points
	 */
	if( np <= 0 ) {
	   *str = '\0';
	   *iret = -1;
	   return;
	}

	/*
	 * encode number into string
	 */
	sprintf( str, "%.*f", np, flnum );

}
