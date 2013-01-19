#include "geminc.h"
#include "gemprm.h"

void cfl_scnd ( char *path, int *plen, char *tmplt, int *tlen, char *sep, 
		int *maxlen, int *order, char *filstr, int *flen, 
		int *nf, int *iret )
/************************************************************************
 * cfl_scnd								*
 *									*
 * This subroutine reads a directory and returns all entry names in	*
 * the directory in one string, each separated by the given single	*
 * character separator.  The directory name may include an environment	*
 * variable.  The output names are sorted in alphabetical order.	*
 * The search pattern constrains the directory scan.  The pattern may	*
 * be a simple regular expression (see CST_PTMT).			*
 *									*
 * cfl_scnd ( path, plen, tmplt, tlen, sep, maxlen, order, filstr, 	*
 *			flen, nf, iret )				*
 *									*
 * Input parameters:							*
 *	*path		char		Directory name			*
 *	*plen		int		Length of directory name	*
 *	*tmplt		char		Search pattern			*
 *	*tlen		int		Length of search pattern	*
 *	*sep		char		Separator			*
 *	*maxlen		int		Maximum length of output string	*
 *	*order		int		Sort order			*
 *					   1 = Alphabetical		*
 *					  -1 = Reverse alphabetical	*
 *									*
 * Output parameters:							*
 *	*filstr		char		String of directory entry names	*
 *	*flen		int		Length of output string		*
 *	*nf		int		Number of entries in the string	*
 *	*iret		int		Return code			*
 *					  0 = normal, file exists	*
 *					 -1 = return code from scandir	*
 *						(directory cannot be	*
 *						opened for reading, or	*
 *						cannot allocate enough	*
 *						memory to hold all the	*
 *						data structures)	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/96						*
 * D.W.Plummer/NCEP	 2/97	Changed calling sequence to add template*
 * D.W.Plummer/NCEP	 2/97	Added pattern matching function		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 6/99	Added the sorting order to call		*
 * M. Li/GSC		 5/00	Changed to GEMPAK style			*
 * D. Kidwell/NCEP	 1/02	Initialized flen to zero                *
 * T. Piper/SAIC	04/07	Re-wrote using cdl_scandir		*
 ***********************************************************************/
{
int		ii, nfound;
char		sepr[2];
struct dirent	**namelist=NULL;

/*---------------------------------------------------------------------*/

/*
 *  In case the caller did NOT call ST_NULL...
 */
    path[*plen] = '\0';
    tmplt[*tlen] = '\0';
    sepr[0] = sep[0];
    sepr[1] = '\0';

    if ( *order == 1 ) {
	nfound = cfl_scandir(path, tmplt, _selecttmplt, _alphasort,
						&namelist);
    }
    else {
	nfound = cfl_scandir(path, tmplt, _selecttmplt, _ralphasort,
						&namelist);
    }

    if ( filstr == NULL ) {
	*flen = 0;
	*nf = nfound;
	*iret = G_NORMAL;
    } 
    else if ( nfound > 0 ) {
	strcpy(filstr, namelist[0]->d_name);
	for ( ii = 1; ii < nfound; ii++) {
	    if (((int)(strlen(filstr) + strlen(namelist[ii]->d_name) + 1))
		<= *maxlen ) {
		sprintf(filstr, "%s%s%s", filstr, sepr, namelist[ii]->d_name);
	    }
	    else {
		break;
	    }
	}
	*flen = strlen(filstr); 
	*nf = ii;
        *iret = G_NORMAL;
    }
    else {
	filstr = '\0';
	*flen = 0;
	*nf = 0;
	*iret = -1;
    }

/*
 *  Free namelist from scandir.
 */
    for ( ii = 0; ii < nfound; ii++) {
	free(namelist[ii]);
    }
    if ( namelist != NULL ) free ( namelist );
}
