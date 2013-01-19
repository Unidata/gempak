#include "geminc.h"
#include "gemprm.h"

void css_envr ( const char *filnam, char *file, int *iret )
/************************************************************************
 * css_envr								*
 *									*
 * This routine checks filnam for an environment variable. If one is	*
 * found, it is replaced with the actual path name.  The new expanded	*
 * file name is returned.  Environment variables are identified by names*
 * beginning with $ and ending with a /.				*
 *									*
 * This routine only checks for environmental variables at the beginning*
 * of 'filnam', not embedded within the string.  Additionally, the 	*
 * environmental variable must be immediately followed by a '/' and	*
 * the remainder of the full path filename.  The environmental variable	*
 * may not be surrounded by braces, i.e., '{}'.  The old VAX syntax is	*
 * supported, in that the leading '$' may be omitted and the 		*
 * environmental variable followed by a colon ':' in which case the '/'	*
 * may be omitted.							*
 *									*
 * To summarize, if an environmental variable is used in a filename the	*
 * filname must be in one of the following formats:			*
 *									*
 * 	$ENV/dir1/dir2/.../dirN/file					*
 * 	ENV:dir1/dir2/.../dirN/file					*
 *									*
 * css_envr ( filnam, file, iret )					*
 *									*
 * Input parameters:							*
 *	*filnam		const char	User input file name		*
 *									*
 * Output parameters:							*
 *	*file		char		Expanded file name		*
 *	*iret		int		Return code			*
 *					  2 = Symbol not found		*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 6/94						*
 * L. Williams/EAI	 7/94		Reformat header			*
 * S. Jacobs/NMC	 7/94		Fixed bug with getenv		*
 * G. Krueger/EAI	 3/96		ANSI, always return a file name	*
 * R. Tian/SAIC		10/04		Handle '~' in file name		*
 * D.W.Plummer/NCEP	10/06	Add colon processing (VAX syntax)	*
 * T. Piper/SAIC	03/07	Fixed '~/' case, put result in file	*
 ***********************************************************************/
{
	char	symbol[133], filtmp[73], usernm[73], *cptr;
	int	ipos, ier;
	size_t	ii, jj;
	struct passwd *pw;

/*---------------------------------------------------------------------*/
	*iret     = 0;
	file[0]   = '\0';
	symbol[0] = '\0';
	filtmp[0] = '\0';
	strcpy ( file, filnam );

/*
 *	Handle '~' in file name.
 */
 	if ( filnam[0] == '~' ) {
	    if ( filnam[1] == '/' ) {
/*
 *		'~/user'.
 */
	        cst_rpst ( filnam, "~", "$HOME", file, &ier );
	    } else {
/*
 *		'~user'.
 */
	        cst_nocc ( filnam, '/', 1, 0, &ipos, &ier );
		if ( ier == 0 ) {
		    cst_ncpy ( usernm, &filnam[1], ipos-1, &ier );
		} else if ( ier == -5 ) {
		    ipos = strlen ( filnam );
		    strcpy ( usernm, &filnam[1] );
		}
		pw = getpwnam ( usernm );
		if ( pw ) {
		    strcpy ( file, pw->pw_dir );
		    strcat ( file, &filnam[ipos] );
		} else {
		    *iret = 2;
		    return;
		}

	    }
	}
/*
 *	Find the part of the name which is an environmental variable.
 *	Translate the variable to a path name.
 */
	else if ( filnam[0] == '$' ) {
	    ii = 1;
	    while ( ( ii < strlen(filnam)+1 ) && ( filnam[ii] != '/' ) ) {
		symbol[ii-1] = filnam[ii];
		symbol[ii] = '\0';
		ii++;
	    }
	    jj = ii;
	    while ( jj < strlen(filnam)+1 ) {
		filtmp[jj-ii] = filnam[jj];
		jj++;
	    }
	    if  ( getenv ( symbol ) != NULL ) {
		strcpy ( file, getenv ( symbol ) );
		strcat ( file, filtmp );
	    } else {
		*iret = 2;
	    }
	}

/*
 * 	Search for a colon ':' and, if found, pull out and process.
 */
	else  {

	    cptr = strchr ( filnam, ':' );

	    if ( cptr != (char *)NULL )  {

		strncpy ( symbol, filnam, (size_t)(cptr-filnam) );
		symbol[(cptr-filnam)] = '\0';

	        if  ( getenv ( symbol ) != NULL ) {
		    strcpy ( file, getenv ( symbol ) );
		    strcat ( file, "/" );
		    strcat ( file, (cptr+1) );
	        } else {
		    *iret = 2;
	        }
	    }
	}
}
