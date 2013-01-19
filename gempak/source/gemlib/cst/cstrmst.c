#include "geminc.h"
#include "gemprm.h"

void cst_rmst ( char *str, char *substr, int *pos, 
					char *outstr, int *iret )
/************************************************************************
 * cst_rmst								*
 *									*
 * This subroutine finds the first occurrence of a substring within a 	*
 * string and returns the position of that substring and the output	*
 * string with the substring removed.  If the substring is not found,	*
 * the position, POS, is set to -1.					*
 *									*
 * cst_rmst ( str, substr, pos, outstr, iret )				*
 *									*
 * Input parameters:							*
 *	*str		char		Input string			*
 *	*substr		char		Substring			*
 *									*
 * Output parameters:							*
 *	*pos		int		Position of substring		*
 *	*outstr		char		Output string			* 
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					 -1 = substring not found	*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 * G. Krueger/EAI	10/97	Rewritten to remove MALLOC		*
 ***********************************************************************/
{
char	*ptr;
int	lenss, lenr;

/*---------------------------------------------------------------------*/
	*pos = -1;
	*iret = 0;

	/*
	 * locate the substring
	 */
	ptr = strstr( str, substr );
	if( ptr )
	   *pos = ptr - str;
	else {
	   *iret = -1;
	   return;
	}

	/*
	 * remove the substring
	 */
	lenss = strlen(substr);
	lenr = strlen( ptr + lenss );
	memmove( outstr, str, *pos );
	memmove( outstr + *pos, ptr + lenss, lenr + 1 );
}
