#include "geminc.h"
#include "gemprm.h"

void cst_find ( const char *str, const char **slist, int nstr,
						int *pos, int *iret )
/************************************************************************
 * cst_find								*
 *									*
 * This function searches for a particular string in a list of strings	*
 * The position in the array is returned in pos.  If the string is not 	*
 * found, pos is set to -1.						*
 *									*
 * cst_find ( str, slist, nstr, pos, iret )				*
 *									*
 * Input parameters:							*
 *	*str		const char	String to be searched		*
 *	**slist		const char	Pointer to list of strings	*
 *	nstr		int		Number of strings in list	*
 *									*
 * Output parameters:							*
 *	*pos		int		Position of string in list	*
 *					 -1 = not found			*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					 -1 = Invalid number of strings	*
 **									*
 * Log:									*
 * L. Williams/EAI	 4/96	Created					*
 * T. Piper/GSC		10/98	Prolog update				*
 ***********************************************************************/
{
    int	ii;

/*---------------------------------------------------------------------*/

    *iret = 0;
    *pos = -1;

/*
 * check the number of strings in list
 */
    if( nstr <= 0 ) {
	*iret = -1;
	return;
    }
/*
 * search each string in the list
 */
    for( ii=0; ii < nstr; ii++ ) {
	if( strcmp( str, slist[ii] ) == 0 ) {
/*
 * string found
 */
	    *pos = ii;
	    break;
	}
    }
}
