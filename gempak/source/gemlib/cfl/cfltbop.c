#include "geminc.h"
#include "gemprm.h"

FILE *cfl_tbop ( char *table, char *type, int *iret )
/************************************************************************
 * cfl_tbop								*
 *									*
 * This function opens a GEMPAK table file for reading.			*
 *									*
 * The table is split into the path and filename, and the file is	*
 * located by searching in the following order:				*
 *									*
 *	1. filename (local)						*
 *	2. path/filename (table as given)				*
 *	3. $NCDESK/type/filename					*
 *	4. $NCSITE/type/filename					*
 *	5. $GEMTBL/type/filename					*
 *									*
 * FILE *cfl_tbop ( table, type, iret )					*
 *									*
 * Input parameters:							*
 *	*table		char		Table file name			*
 *	*type		char		File name type			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *	*cfl_tbop	FILE		File pointer			*
 **									*
 * G. Krueger/EAI  3/96							*
 * G. Krueger/EAI  8/96	   CFL_TOPN->TBOP; Match with FL library	*
 * C. Lin/EAI	   8/97	   fix bug in while loop (cpos>=0)->(cpos>0)	*
 * S. Law/GSC		05/00	changed to use file size defines	*
 * A. Hardy/GSC		12/00   Added search paths NCDESK and NCSITE	*
 * S. Jacobs/NCEP        5/01   Changed to call CFL_TINQ                *
 ***********************************************************************/
{
    FILE	*fptr;
    int		ier;
    long	lfsize;
    char	actualpath[LLPATH];

/*---------------------------------------------------------------------*/

    *iret = 0;

    cfl_tinq ( table, type, &lfsize, actualpath, &ier );

    fptr = cfl_ropn (actualpath, NULL, iret);
    return fptr;
}
