#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

void ctb_mzrd ( char *tblnam, char *dirsym, int *nstn, 
	  	Marzon_t *mznms, int *iret )
/************************************************************************
 * ctb_mzrd								*
 *									*
 * This routine reads a station table into an array of marine zone	*
 * structures. The table consists of the zone id and it's station name  *
 * which can be greater than 33 characters but less than 256 characters.*
 *									*
 * ctb_mzrd ( tblnam, dirsym, nstn, mzarr, iret )			*
 *									*
 * Input parameters:							*
 *	*tblnam		char		Station table name		*
 *	*dirsym		char		Directory path/symbol		*
 *									*
 * Output parameters:							*
 *	*nstn		int		Number of marine zones		*
 *	*mznms		Marzone_t 	Marine zone structure		*
 *	*iret		int		Return code			*
 *					   As for cfl_ropn		*
 **									*
 * Log:									*
 * A. Hardy/NCEP	 3/04						*
 ***********************************************************************/
{
    FILE    *ftbl;
    int	    ii, numzn, ier;
    long    flen;
    char    record[257], actualpath[LLPATH];

/*---------------------------------------------------------------------*/
    *iret = 0;

   /*
    * Open the table file.
    */
    cfl_tinq ( tblnam, dirsym, &flen, actualpath, &ier );
    ftbl = cfl_ropn ( actualpath, NULL, &ier );
    if ( *iret != 0 ) {
	mznms->nummz = 0;
        return;
    }

   /*
    * Get number of valid table entries.
    */
    cfl_tbnr( ftbl, &numzn, &ier );

    if ( numzn != 0 )  {

       /*
        *  Allocate the structure elements.
        */

        mznms->nummz  = numzn;
        mznms->mzones = (MZinfo *) malloc( numzn * sizeof(MZinfo) );
    }
    else  {

       /*
        *  Problem opening table file; set error code and return.
        */

        cfl_clos( ftbl, &ier );
        *iret = -2;
        return;
    }

    rewind ( ftbl );


   /*
    * Read in the next record, check for a comment,
    * and process valid table entries.
    */

    ii = 0;
    while ( fgets ( record, 256, ftbl ) != NULL )  {
        if ( ( record[0] != '!' ) && ( ii < numzn ) ) {
	    if ( sscanf ( record, "%s %s", mznms->mzones[ii].mzid, 
		                 mznms->mzones[ii].name) > 0 ) {
	        ii++;
	    }
	}
    }

	*nstn = ii;

        cfl_clos( ftbl, &ier );
}
