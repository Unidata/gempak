#include "geminc.h"
#include "gemprm.h"

int cfl_rdir ( int type, char *dir, char *search, 
				struct dirent **dnlist[], int *nsdir )
/************************************************************************
 * cfl_rdir								*
 *									*
 * This function reads the specified directory.  The directory name may	*
 * include an environment variable.  Based on the type parameter, this	*
 * function reads either everything in the directory (files and		*
 * subdirectories) or only the subdirectories.  The output is sorted in	*
 * alphabetical order.							*
 *									*
 * The search pattern constrains the directory scan.  A match is	*
 * made when a file or subdirectory name contains the search pattern	*
 * string exactly as given.  Wild-card characters are not supported.	*
 * The '.' and '.*' entries in the directory are always excluded.	*
 *									*
 * int cfl_rdir ( type, dir, search, dnlist, nsdir )			*
 *									*
 * Input parameters:							*
 *	type		int		Type of search (search for...)	*
 *					  0 = everything		*
 *					  1 = subdirectories only	*
 *					  2 = subdirectories excluded	*
 *	*dir		char		Directory name			*
 *	*search		char		Search pattern			*
 *									*
 * Output parameters:							*
 *	**dnlist [ ]	struct dirent	Directory content		*
 *	*nsdir		int		Number of subdirectories	*
 *	cfl_rdir	int		Number of entries in dnlist [ ]	*
 **									*
 * Log:									*
 * C. Lin/EAI		 4/96						*
 * E. Wehner/EAi	 1/97	Added a search string param.		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * H. Zeng/SAIC		01/05	added a new type of search		*
 * T. Piper/SAIC	04/07	Re-wrote using cfl_scandir		*
 ***********************************************************************/
{
    int nf;
/*---------------------------------------------------------------------*/
    if ( type == 0 ) {
	nf = cfl_scandir(dir, search, _selectdir, _alphasort, dnlist);
	*nsdir = IMISSD;
    }
    else if (type == 1) {
	nf = cfl_scandir(dir, search, _selectdirOnly, _alphasort, dnlist);
	*nsdir = nf;
    }
    else if (type == 2) {
	nf = cfl_scandir(dir, search, _selectdirExcd, _alphasort, dnlist);
	*nsdir = 0;
    }
    else {
	nf = 0;
	*nsdir = 0;
    }
    return(nf);
}
