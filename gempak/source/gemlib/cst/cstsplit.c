#include "geminc.h"
#include "gemprm.h"

char *cst_split ( char *pstart, char delim, int maxchar, 
					char *result, int *iret )
/************************************************************************
 * cst_split								*
 *									*
 * This function splits a string into parts based on delim.  It		*
 * searches for delim in pstart, places the portion of the string	*
 * before delim (or the whole string if delim is not found) into	*
 * result, and returns a pointer to just after delim (or NULL if not	*
 * found).								*
 *									*
 * char cst_split ( pstart, delim, maxchar, result, iret )		*
 *									*
 * Input parameters:							*
 *	*pstart		char	start of string to search		*
 *	delim		char	single character delimiter		*
 *	maxchar		int	maximum number of chars in result	*
 *									*
 * Output parameters:							*
 *	*result		char	split off string			*
 *	*iret		int	returns -1 if maxchar is exceeded	*
 *	*cst_split	char	new starting point or NULL		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 ***********************************************************************/
{
    size_t	nchar, max;
    char	*pend;
/*---------------------------------------------------------------------*/

    *iret = 0;
    max = maxchar - 1;
    pend = strchr (pstart, delim);

    if (pend) {
	nchar = (pend - pstart);
	if (nchar > max) {
	    nchar = max;
	    *iret = -1;
	}

	strncpy (result, pstart, nchar);
	result[nchar] = '\0';

	pend++;
    }
    else {
	if (strlen(pstart) > max) {
	    strncpy (result, pstart, max);
	    result[max] = '\0';
	    *iret = -1;
	}
	else {
	    strcpy (result, pstart);
	}

	pend = NULL;
    }

    return (pend);
}
