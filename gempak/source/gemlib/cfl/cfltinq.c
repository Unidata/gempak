#include "geminc.h"
#include "gemprm.h"

void cfl_tinq ( char *table, char *type, long *flen, char *newfil, 
								int *iret )
/************************************************************************
 * cfl_tinq								*
 *									*
 * This function returns the path to a GEMPAK table file, if the table	*
 * exists.								*
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
 * void cfl_tinq ( table, type, flen, newfil, iret )			*
 *									*
 * Input parameters:							*
 *	*table		char		Table file name			*
 *	*type		char		File name type			*
 *									*
 * Output parameters:							*
 *	*flen 		long		File size			*
 *	*newfil		char		Expanded file name		*
 *	*iret		int		Return code			*
 **									*
 * S. Jacobs/NCEP	 5/01	Copied from cfl_tbop			*
 ***********************************************************************/
{
    int		found;
    int		lenp, lenf, lent, ier;
    long	lfsize;
    char	filepart[MXFLSZ], pathpart[LLPATH];
    char	actualpath[LLPATH], pathfile[FILE_FULLSZ];

/*---------------------------------------------------------------------*/

    *iret = 0;
    found = G_FALSE;
    newfil[0] = CHNULL;
    actualpath[0] = CHNULL;
    lfsize = 0;

    /*
     *	Strip GEMTBL from beginning of the table file name if present.
     */
    if ( strstr (table, "$GEMTBL/") == table ) {
	strcpy (pathfile, &table[8]);
    } else if ( strstr (table, "GEMTBL:") == table ) {
	strcpy (pathfile, &table[7]);
    } else {
	strcpy (pathfile, table);
    }

    /*
     *	Extract the filename from the end of the path.
     */
    cfl_path ( pathfile, pathpart, filepart, &ier );
    cst_lstr ( pathpart, &lenp, &ier );
    cst_lstr ( filepart, &lenf, &ier );
    cst_lstr ( type, &lent, &ier );

    /*
     *	Check to see if the file is local.
     */
    cfl_inqr (filepart, NULL, &lfsize, actualpath, iret );
    if ( *iret == 0 ) {
	found = G_TRUE;
    }

    /*
     *	Check to see if the file is down a path.
     */
    if ( (found == G_FALSE) && ( lenp != 0 ) ) {
	cfl_inqr (filepart, pathpart, &lfsize, actualpath, iret );
	if ( *iret == 0 ) {
	    found = G_TRUE;
	}
    }

    /*
     *	Check to see if the file exists in terms of type in NCDESK.
     */
    if ( (found == G_FALSE) && (lent != 0) ) {
	strcpy (pathfile, "$NCDESK/");
	strcat (pathfile, type);
	strcat (pathfile, "/");
	strcat (pathfile, filepart);
	cfl_inqr (pathfile, type, &lfsize, actualpath, iret );
	if ( *iret == 0 ) {
	    found = G_TRUE;
	}
    }

    /*
     *	Check to see if the file exists in terms of type in NCSITE.
     */
    if ( (found == G_FALSE) && (lent != 0) ) {
	strcpy (pathfile, "$NCSITE/");
	strcat (pathfile, type);
	strcat (pathfile, "/");
	strcat (pathfile, filepart);
	cfl_inqr (pathfile, type, &lfsize, actualpath, iret );
	if ( *iret == 0 ) {
	    found = G_TRUE;
	}
    }

    /*
     *	Check to see if the file exists in terms of type in GEMTBL.
     */
    if ( (found == G_FALSE) && (lent != 0) ) {
	strcpy (pathfile, "$GEMTBL/");
	strcat (pathfile, type);
	strcat (pathfile, "/");
	strcat (pathfile, filepart);
	cfl_inqr (pathfile, type, &lfsize, actualpath, iret );
	if ( *iret == 0 ) {
	    found = G_TRUE;
	}
    }

    strcpy ( newfil, actualpath );
    *flen = lfsize;

}
