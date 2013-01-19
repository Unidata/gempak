#include "geminc.h"
#include "gemprm.h"

char *cvg_getoutdir ( char *prefsTag, char *filename )
/************************************************************************
 * cvg_getoutdir                                                     	*
 *                                                                      *
 * This function gets the user preferred output directory from 		*
 * prefs.tbl for tag '*prefsTag' and appends '*filename'.		*
 *                                                                      *
 * char *cvg_getoutdir ( prefsTag, filename )                        	*
 *                                                                      *
 * Input parameters:                                                    *
 * *prefsTag		char	Preference table TAG name		*
 * *filename		char	File name to append to directory	*
 *									*
 * Output parameters:                                                   *
 * *out_file		char   full file name                  		*
 **                                                                     *
 * Log:                                                                 *
 *  T. Piper/SAIC	06/04	initial coding                          *
 ***********************************************************************/
{
    int                 ier;
    static char		out_dir[LLPATH], out_file[LLPATH];
    static Boolean      readTbl = FALSE;
/*---------------------------------------------------------------------*/
    
    if ( !readTbl ) {
        ctb_pfstr (prefsTag, out_dir, &ier );
	if ( out_dir[strlen(out_dir) - 1] != '/' ) {
            strcat ( out_dir, "/" );
        }
        readTbl = TRUE;
    }
    strcpy (out_file, out_dir);
    strcat (out_file, filename);
    return (out_file);
}
