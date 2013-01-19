#include "geminc.h"
#include "gemprm.h"

void cst_cins ( char *insptr, char ch, int *iret )
/************************************************************************
 * cst_cins								*
 *									*
 * This routine will insert a character into a string.  This routine is *
 * passed the address of the point in the string where the character	*
 * is to be inserted.							*
 *									*
 * Note:  INSPTR must have enough space for the insertion.		*
 *									*
 * cst_cins ( insptr, ch, iret )					*
 * 									*
 * Input parameters:							*
 *	*insptr		char            The point of insertion        	*
 * 	ch		char		Char to insert			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 * 									*
 **									*
 * Log:									*
 * L. Williams		 6/96						*
 * G. Krueger/EAI	10/97	Rewritten to remove MALLOC		*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = 0;

	memmove( insptr + 1, insptr, strlen(insptr) + 1);
	*insptr = ch;
}
