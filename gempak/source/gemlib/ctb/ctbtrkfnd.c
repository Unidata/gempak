#include "geminc.h"
#include "gemprm.h"

void ctb_trkfnd ( int *intv, int *index, int *iret )
/************************************************************************
 * ctb_trkfnd                                                           *
 *                                                                      *
 * This routine finds the index of track interval for the given 	*
 * time interval value							*
 *                                                                      *
 * ctb_trkfnd ( intv, index, iret )				        *
 *                                                                      *
 * Input parameters:                                                    *
 * 	*intv		int		input time interval		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *index          int            index of time interval		*
 *      *iret           int             Return code                     *
 *                                         		                *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		10/00	created                                 *
 * T. Piper/SAIC	12/01	close file 				*
 ***********************************************************************/
{
        FILE    *ftbl;
        char    text[6], record[133], tblnam[72], dirsym[72];
        int     i, jj, min, iarr[2], ier;

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
		    cst_ilst(text, ':', 0, 2, iarr, &jj, &ier);

		    if ( jj == 2 )
		        min = iarr[0] * 60 + iarr[1];
        	    else
            		min = iarr[0];

        	    if ( *intv == min ) {
            		*index = i;
			fclose (ftbl );
       			return;
		    } else
                        i++;
                }
            }
        }

        *index = i;

        fclose ( ftbl );
}
