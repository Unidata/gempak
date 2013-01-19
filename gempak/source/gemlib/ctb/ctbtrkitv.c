#include "geminc.h"
#include "gemprm.h"

void ctb_trkitv ( int max_itvs, char *intv[], int *iret )
/************************************************************************
 * ctb_trkitv                                                           *
 *                                                                      *
 * This routine gets all intervals from the track time interval table 	*
 *                                                                      *
 * ctb_trkitv ( max_itvs, intv, iret )				        *
 *                                                                      *
 * Input parameters:                                                    *
 *	max_itvs	int		Max. number of interval times	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *intv[]	        char            Array of track interval names	*
 *      *iret           int             Return code                     *
 *                                 1 - More entries found than max_itvs	*
 *                                -1 - table hasn't been read  		*
 *                                         		                *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		10/00	created                                 *
 * T. Piper/SAIC	12/01	add fclos when record found		*
 * J. Wu/SAIC	 	02/02	read all entries instead of a single one*
 ***********************************************************************/
{
        FILE    *ftbl;
        char    text[6], record[133], tblnam[72], dirsym[72];
        int     ii, ier;

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
	ii = 0;
        while ( fgets ( record, 132, ftbl ) != NULL ) {
            if ( record[0] != '!' ) {                 
		if ( sscanf ( record, "%s", text ) > 0 ) {
		    if ( ii < max_itvs ) {
		        strcpy( intv[ii], text );		    
		        ii++;
                    }
		    else {
		        *iret = 1;
			return;
		    }
		} 
	    }
	}
	
        fclose ( ftbl );
}
