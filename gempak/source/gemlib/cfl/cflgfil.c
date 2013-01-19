#include "geminc.h"
#include "gemprm.h"

int cfl_gfil ( int sortby, int max_files, char *dirname, char *search, 
						char flist[][MXFLSZ] )
/************************************************************************
 * cfl_gfil								*
 *									*
 * This function reads the specified directory.  The directory name may	*
 * include an environment variable.  The variable flist is filled with	*
 * the files in the directory that meet the search pattern and are	*
 * sorted by name or by date depending on the sortby variable.		*
 * Wild-card characters are not supported.  The '.' and '.*' entries in	*
 * the directory are always excluded.					*
 *									*
 * int cfl_gfil (sortby, max_files, dirname, search, flist)		*
 *									*
 * Input parameters:							*
 *	sortby		int		Type of sort			*
 *					 0 = by name			*
 *					 1 = by date			*
 *	max_files	int		maximum number of files		*
 *	*dirname	char		Directory name (including path)	*
 *	*search		char		Search pattern			*
 *									*
 * Output parameters:							*
 *	flist[][MXFLSZ]	char		file list			*
 * 	cfl_gfil	int		Number of entries in flist[]	*
 **									*
 * Log:									*
 * S. Law/GSC		01/00	reworked from cfl_rdir			*
 * S. Law/GSC		05/00	changed to use file size defines	*
 * T. Piper/SAIC	04/07	Re-wrote using cfl_scandir		*
 ***********************************************************************/
{
    int ii, nf, nfound;
    struct dirent **namelist=NULL;

/*---------------------------------------------------------------------*/

    if ( sortby == 0 ) {
	nfound = cfl_scandir( dirname, search, _selectdirExcd,
			 _alphasort, &namelist);
    }
    else {
	nfound = cfl_scandir( dirname, search, _selectdirExcd,
			 _rdatesort, &namelist);
    }

    if ( flist == NULL ) {
	nf = nfound;
    }
    else if ( nfound > 0 ) {
        for ( ii = 0; ii < nfound; ii++) {
            if ( ii < max_files ) {
		strcpy(flist[ii], namelist[ii]->d_name);
            }
	    else {
		break;
	    }
        }
	nf = ii;
    }
    else {
        flist[0][0] = '\0';
	nf = 0;
    }

/*
 *  Free namelist from scandir.
 */
    for ( ii = 0; ii < nfound; ii++) {
	free(namelist[ii]);
    }
    if ( namelist != NULL ) free(namelist);

    return(nf);
}
