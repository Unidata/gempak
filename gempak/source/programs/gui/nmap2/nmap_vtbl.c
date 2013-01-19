#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"
#include "vgftbl.h"  


#define VGF_FILE_EXT	".vgf"
#define MAX_FILES	1000
#define CWD_TITLE	"Local"

static short	 _defUser;
static char      _curDirFlg;


/************************************************************************
 * nmap_vtbl.c								*
 *									*
 * This module reads VGF user table.					*
 *									*
 * CONTENTS:								*
 *   vtbl_readUsrTbl()	process surface table file into data structure	*
 *									*
 *   vtbl_getDefUser()	get indext to the default user			*
 *   vtbl_getPath()	get path for given name				*
 *   vtbl_checkCurDir()	check current directory for '.vgf' file		*
 ***********************************************************************/

/*=====================================================================*/

void vtbl_readUsrTbl ( char *tblname, int *iret )
/************************************************************************
 * vtbl_readUsrTbl							*
 *									*
 * This routine will read VGF user table and create the data structure	*
 * to store the information.						*
 *									*
 * void vtbl_readUsrTbl(tblname, iret) 		                        *
 *									*
 * Input parameters:                                                    *
 *      *tblname        char            table name                      *
 *									*
 * Output parameters:                                                   *
 *      *iret		int             0 - success                     *
 *									*
 * Return code:                                                         *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		 5/97						*
 * C. Lin/EAI		 6/97	rework the logic, allow empty table	*
 * C. Lin/EAI		 6/97	fix getlogin() return NULL problem	*
 * C. Lin/EAI		 7/97	alway create an entry for current dir	*
 * C. Lin/EAI		11/97	add free dnamelist			*
 * S. Law/GSC		01/00	changed to storing table info here	*
 * S. Law/GSC		01/00	replaced cfl_rdir with cfl_gfil		*
 * S. Law/GSC		03/00	changed to ignore usrnam, always add	*
 *				the cwd to the bottom of the list	*
 * S. Jacobs/NCEP	 3/00	Renamed header file nmap_vtbl to vgftbl	*
 * T. Piper/SAIC	 2/02	Fixed memory leak - made table global	*
 * T. Piper/SAIC	10/05	removed unused call to cfl_gfil		*
 ***********************************************************************/
{
    int		ii, nn, num, ier;
    char	buffer[256], title[60], path[256], cwd[256], format[256];
    char	dum1[256], dum2[256], dum3[256];
    FILE	*fp;

/*---------------------------------------------------------------------*/

    *iret = 0; 

    strcpy ( format, "%s %s" );

    fp = cfl_tbop (tblname, "nmap", &ier);

    nn = 0;
    if (fp && ier == 0) { 
	while (!feof(fp)) {
	    /*
	     * read a record
	     */
	    cfl_trln (fp, 256, buffer, &ier);

	    if  ( ier == 0 )  {

	        if  ( nn == 0 )  {
		    num = sscanf ( buffer, "%s %s %s", dum1, dum2, dum3 );
		    if  ( num == 3 )  {
			strcpy ( format, "%s %*s %s" );
		    }
		}

		nn++;
	    }

	}
    }

    /*
     * allocate one additional item in case the current user
     * has to be added in
     */
    _vgfUsrTbl.nitems = nn;
    _vgfUsrTbl.items  = (vgfusr_ent_t *) malloc ((nn+1) *sizeof (vgfusr_ent_t));

    if (nn > 0) {
	rewind(fp);

	ii = 0;
	while (ii < nn) {
	    cfl_trln(fp, 256, buffer, &ier);

	    if (ier == 0) {
		sscanf(buffer, format, title, path);

		_vgfUsrTbl.items[ii].title = (char *) malloc (strlen (title) + 1);
		strcpy(_vgfUsrTbl.items[ii].title, title);

		_vgfUsrTbl.items[ii].usrpath = (char *) malloc (strlen(path) + 1);
		strcpy(_vgfUsrTbl.items[ii].usrpath, path);

		ii++;
	    }
	}
    }

    if (fp) fclose(fp);

    /*
     * set default user
     */
    getcwd (cwd, sizeof (cwd));

    ii = _vgfUsrTbl.nitems;
    _vgfUsrTbl.items[ii].title = (char *) malloc (strlen (CWD_TITLE) + 1);
    strcpy (_vgfUsrTbl.items[ii].title, CWD_TITLE);

    _vgfUsrTbl.items[ii].usrpath =	(char *) malloc(strlen(cwd) + 1);
    strcpy (_vgfUsrTbl.items[ii].usrpath, cwd);

    /*
     * use current directory as default VGF
     */ 
    _vgfUsrTbl.nitems++;
    _defUser = _vgfUsrTbl.nitems - 1;
    _curDirFlg = 1;

}

/*=====================================================================*/

int vtbl_getDefUser ( void )
/************************************************************************
 * vtbl_getDefUser							*
 *									*
 * This routine will return the index to the default user.		*
 *									*
 * int vtbl_getDefUser ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * vtbl_getDefUser	int	index to the default user		*
 *				-1 - error				*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		 5/97						*
 ***********************************************************************/
{
    return (_defUser);
}

/*=====================================================================*/

void vtbl_checkCurDir ( void )
/************************************************************************
 * vtbl_checkCurDir							*
 *									*
 * This routine checks the current working directory again to add	*
 * the user into the VGF table if there is at least one ".vgf" file.	*
 *									*
 * void vtbl_checkCurDir ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return code:								*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		 7/97						*
 * S. Law/GSC		01/00	changed _vgfTbl to not be a pointer	*
 * S. Law/GSC		01/00	replaced cfl_rdir with cfl_gfil		*
 ***********************************************************************/
{
    int		nn;
    char	cwd[256];
/*---------------------------------------------------------------------*/

    if (_curDirFlg) return;

    getcwd(cwd, sizeof(cwd));

    nn = cfl_gfil(0, MAX_FILES, cwd, VGF_FILE_EXT, NULL);

    /*
     * add the user if there is any VGF file in the 
     * current directory 
     */ 
    if (nn > 0) {
	_vgfUsrTbl.nitems++;
	_curDirFlg = 1;
    }
}

/*=====================================================================*/

void vtbl_getPath ( char *name, char *path, int *iret )
/************************************************************************
 * vtbl_getPath								*
 *									*
 * This routine will return the path for the given name.		*
 *									*
 * void vtbl_getPath (name, path, iret)					*
 *									*
 * Input parameters:							*
 *	*name		char		Name to match			*
 *									*
 * Output parameters:							*
 *	*path		char		Path to data			*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/99	Initial coding				*
 * S. Law/GSC		01/00	changed _vgfTbl to not be a pointer	*
 ***********************************************************************/
{
    int	ii, done;
/*---------------------------------------------------------------------*/

    *iret = 0;

    path[0] = '\0';

    done = 0;
    ii   = 0;
    while ( ( ii < _vgfUsrTbl.nitems ) && ( ! done ) )  {
	if  ( strcmp ( name, _vgfUsrTbl.items[ii].title ) == 0 )  {
	    strcpy ( path, _vgfUsrTbl.items[ii].usrpath );
	    done = 1;
	}
	ii++;
    }
}

/*=====================================================================*/
