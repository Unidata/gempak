#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

void ctb_rdprf ( char *tblnam, char *dirsym, char *tag, char *value, 
 		 int *iret )
/************************************************************************
 * ctb_rdprf								*
 *									*
 * This subroutine reads a prefs table and returns a single value 	*
 * associated with a single tage in the table.				* 
 *									*
 * ctb_rdprf ( tblnam, dirsym, tag, value, iret )			*
 *									*
 * Input parameters:							*
 *	*tblnam		char		Prefs table file name		*
 *	*dirsym 	char		Lowest level directory name	*
 *	*tag		char		Tags for prefs table definition	*
 *									*
 * Output parameters:							*
	*value		char		Value for prefs table tag	*
 *	*iret		int		Return code			*
 *					  -1 = table cannot be opened	*
 *					  -2 = entry not found		*
 **									*
 * Log:									*
 * M. Li/SAIC		10/04	Modified from ctb_rdwou			*
 ***********************************************************************/
{

	FILE   *ftbl;
	char   chname[25], label[120], buffer[180];
        int    ier, ilen2, lens;
	Boolean found;

/*---------------------------------------------------------------------*/

/*
 *	Initialize output variables.
 */
	*iret     = -2;
	ilen2     = 0;
	chname[0] = '\0';
        label[0] = '\0';
        value[0] = '\0';
	found    = False;

/*
 *	Open the prefs table file.
 */
	ftbl = cfl_tbop ( tblnam, dirsym, &ier );
	if  ( ier != 0 )  {
	    *iret = -1;
	    return;
	}
	cst_lstr ( tag, &lens, &ier );

/*
 *	Read in the next record, check for a comment,
 *	and process valid table entries.
 */
	while ( ( fgets ( buffer, 180, ftbl ) != NULL ) && ( !found ) ) {
	    if  ( buffer[0] != '!' )  {
		if  ( sscanf ( buffer,"%s %s", chname, label) ) {
		    cst_rmbl ( chname, chname, &ilen2, &ier );
	            cst_lstr ( label, &lens, &ier );
		    if (strcmp ( chname, tag ) == 0 ) {
		        cst_ncpy ( value, label, lens, &ier );
			found    = True;
		        *iret    = 0;
	            }
		    else {
	                chname[0] = '\0';
                        label[0] = '\0';
		    }
		}
	    }
	}
	if ( !found ) {
	    *iret = -2;
	}

	cfl_clos ( ftbl, &ier );
}
