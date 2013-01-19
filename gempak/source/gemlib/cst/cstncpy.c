#include "geminc.h"
#include "gemprm.h"

void cst_ncpy ( char *str1, const char *str2, int len, int *iret )
/************************************************************************
 * cst_ncpy								*
 *									*
 * This subroutine copies the contents of str2 into str1 up to len	*
 * characters then appends the NULL at the end of str1.			* 
 *									*
 * cst_ncpy ( str1, str2, len, iret )					*
 *									*
 * Input parameters:							*
 *	*str2		const char	Target string			*
 *	len		int		Length 				*
 *									*
 * Output parameters:							*
 *	*str1		char		Destination string		*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					 -1 = invalid length		*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
/*
 * check the length
 */
    if ( len <= 0 ) {
	*str1 = '\0';
	*iret = -1;
    }
    else {
	strncpy( str1, str2, len );
	str1[len] = '\0';
	*iret = 0;
    }
}
