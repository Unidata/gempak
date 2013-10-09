#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_rdbf ( FILE *fp, int rec, dbf_header *dbfhdr, int *iret )
/************************************************************************
 * shp_rdbf                                                             *
 *                                                                      *
 * This function reads each database field record.             		*
 *                                                                      *
 * shp_rdbf ( fp, rec, dbfhdr, iret )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *      *fp             FILE            Database file pointer           *
 *	rec		int		The record index 		*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *dbfhdr         dbf_header      Database header structure       *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         12/03   	Initial coding                  *
 ***********************************************************************/
{
    char record[MAXRECLEN], *fldptr;
    long offset;
    int nbin, ier, ifld;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Position to the beginning of the record.
     */
    offset = dbfhdr->hlen + rec * dbfhdr->rlen;
    cfl_seek ( fp, offset, SEEK_SET, &ier );

    /*
     * Read in one record, and extract corresponding fields.
     */
    cfl_read ( fp, dbfhdr->rlen, (unsigned char *)record, &nbin, &ier );
    if ( record[0] == DBF_VALID ) {
        for ( fldptr = &record[1], ifld = 0; ifld < dbfhdr->nfld;
	      fldptr += dbfhdr->dbflds[ifld].lens, ifld++ ) {
	    strncpy ( dbfhdr->dbflds[ifld].data, fldptr,
	              dbfhdr->dbflds[ifld].lens );
	    dbfhdr->dbflds[ifld].data[dbfhdr->dbflds[ifld].lens] = '\0';
        }
    }
}
