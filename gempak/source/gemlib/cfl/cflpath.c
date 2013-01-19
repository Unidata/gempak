#include "geminc.h"
#include "gemprm.h"

void cfl_path ( char *fulnam, char *dirnam, char *basnam, int *iret )
/************************************************************************
 * cfl_path								*
 *									*
 * This function breaks a full UNIX file path into its directory and	*
 * file names. This function does not expand environment variables.	*
 * It also does not check for the existence of the full path name.	*
 *									*
 * cfl_path ( fulnam, dirnam, basnam, iret )				*
 *									*
 * Input parameters:							*
 *	*fulnam		char		Full path name			*
 *									*
 * Output parameters:							*
 *	*dirnam		char		Directory			*
 *	*basnam		char		Last level of path		*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	11/99	Created					*
 ***********************************************************************/
{
	int	ip, lenf, lenb, lend, found, ier;
/*---------------------------------------------------------------------*/
	*iret = 0;

	dirnam[0] = CHNULL;
	strcpy ( basnam, fulnam );
	found = G_FALSE;
	lenf = strlen ( fulnam );

/*
 *	Find the last '/' in the full path name.
 */
	ip = lenf;
	while ( ( ip >= 0 ) && ( ! found ) )  {
	    if  ( fulnam[ip] == '/' )  {
		if  ( ip+1 != lenf )  {
		    found = G_TRUE;
		    cst_ncpy ( dirnam, fulnam, ip+1, &ier );
		    strcpy ( basnam, &fulnam[ip+1] );
		}
	    }
	    ip--;
	}

/*
 *	If the base name ends with a '/', remove it.
 */
	lenb = strlen ( basnam );
	if  ( basnam[lenb-1] == '/' )  basnam[lenb-1] = CHNULL;

/*
 *	If the directory name ends with a '/', remove it, unless
 *	the string contains only a '/'.
 */
	lend = strlen ( dirnam );
	if  ( lend > 1 )  dirnam[lend-1] = CHNULL;
}

