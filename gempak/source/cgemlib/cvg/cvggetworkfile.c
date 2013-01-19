#define	 USR_PREF_FILE
#include "cvgcmn.h"

char *cvg_getworkfile ( void )
/************************************************************************
 * cvg_getworkfile                                                     	*
 *                                                                      *
 * This function gets the user preferred file name and directory.  The  *
 * file directory is obtained from prefs.tbl and the file name is from  *
 * cvgcmn.h.                                                            *
 *                                                                      *
 * char *cvg_getWorkFile ( void )                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      *work_file       char   full working file name                  *
 **                                                                     *
 * Log:                                                                 *
 *  T. Lee/SAIC         11/03   initial coding                          *
 *  T. Lee/SAIC		 7/04	checked "/" properly			*
 ***********************************************************************/
{
    int                 ier;
    static Boolean      readTbl = FALSE;
/*---------------------------------------------------------------------*/

    if ( !readTbl ) {
        ctb_pfstr ( "WORK_FILE_DIR", work_file, &ier );
        if ( work_file [ ( strlen ( work_file ) - 1 ) ] != '/' ) {
            strcat ( work_file, "/" );
        }
        strcat ( work_file, FILE_NAME );
        readTbl = TRUE;
    }
    return ( work_file );
}
