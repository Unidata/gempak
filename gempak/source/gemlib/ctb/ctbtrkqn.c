#include "geminc.h"
#include "gemprm.h"

void ctb_trkqn ( int *nintv, int *iret )
/************************************************************************
 * ctb_trkqn                                                            *
 *                                                                      *
 * This routine queries the total number of intervals in the track	*
 * interval table.							*
 *                                                                      *
 * ctb_trkqn ( nintv, iret )					        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *nintv          int            number of intervals in the table*
 *      *iret           int             Return code                     *
 *                                         		                *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		10/00	created                                 *
 ***********************************************************************/
{
        FILE    *ftbl;
        char    text[6], record[133], tblnam[72], dirsym[72];
        int     i, ier;

/*---------------------------------------------------------------------*/
        *iret = 0;

/*
 *      Open the table file.
 */
    	strcpy ( tblnam, "trkint.tbl" );
        strcpy ( dirsym, "$GEMTBL/config" );
        ftbl = cfl_ropn ( tblnam, dirsym, &ier );
        if  ( ier != 0 )  {
            *iret = -1;
            return;
        }
 
/*
 *      Read in the next record, check for a comment,
 *      and process valid table entries.
 */
	i = 0;
        while ( fgets ( record, 132, ftbl ) != NULL ) {
            if ( record[0] != '!' ) {
                if ( sscanf ( record, "%s", text ) > 0 ) {
                    i++;
                }
            }
        }

        *nintv = i;

        fclose ( ftbl );
}



