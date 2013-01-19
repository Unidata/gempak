#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

void ctb_ccrd ( char *tblnam, char *dirsym, Clustcnty_t *cc, int *iret )
/************************************************************************
 * ctb_ccrd								*
 *									*
 * This routine will read the clustered county table into a structure.	*
 *									*
 * ctb_ccrd ( tblnam, dirsym, cc, iret )				*
 *									*
 * Input parameters:							*
 *	*tblnam		char		Data type table name		*
 *	*dirsym		char		Directory			*
 *									*
 * Output parameters:							*
 *	*cc	Clustcnty_t 		Clustered county structure	*
 *	*iret	int			Return code			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/01						*
 * A. Hardy/NCEP	 1/05	Added error check on 1st cst_split	*
 ***********************************************************************/
{
FILE    *ftbl;
char    buff[256], ccname[32], *next;
int	ii, jj, numclust, fips[50], nfips, len, ier, ierr;

/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Open the table.
     */
    ftbl = cfl_tbop ( tblnam, dirsym, iret );
    if ( *iret != 0 )  {
        cc->nclust = 0;
        return;
    }

    /*
     *  Get number of valid table entries.
     */
    cfl_tbnr( ftbl, &numclust, &ier );

    if ( numclust != 0 )  {
        /*
         *  Allocate the structure elements.
         */
        cc->nclust = numclust;
        cc->clust = (CCinfo *) malloc( numclust * sizeof(CCinfo) );
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
     *  For every valid table entry, read in, parse, put in structure
     */

    ii = 0;
    while ( ii < numclust )  {

	cfl_trln( ftbl, sizeof(buff), buff, &ier );

	if ( ier == 0 )  {

	    next = cst_split ( buff, '|', 4, cc->clust[ii].ccwfo, &ier );

            if ( ier == 0 ) { 
	        next = cst_split ( next, '|', 32, ccname, &ier );
	        cc->clust[ii].ccname = 
		    (char *)malloc( (strlen(ccname)+1) * sizeof(char) );
	        strcpy ( cc->clust[ii].ccname, ccname );

	        cst_rmbl ( next, next, &len, &ier );

	        cst_ilst ( next, '+', IMISSD, sizeof(fips)/sizeof(fips[0]),
		           fips, &nfips, &ier );

	        cc->clust[ii].ncc = nfips;
                cc->clust[ii].cc = (int *)malloc( nfips * sizeof(int) );

	        for ( jj = 0; jj < nfips; jj++ )  {
		    cc->clust[ii].cc[jj] = fips[jj];
	        }

            }
            else {
                ierr = 3;
                er_wmsg ( "CTB", &ierr, buff, &ier, 3, strlen (buff) );
            }
	    ii++;

	}

    }

    cfl_clos ( ftbl, &ier );

}
