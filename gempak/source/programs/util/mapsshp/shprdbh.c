#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_rdbh ( FILE *fp, dbf_header *dbfhdr, int *iret )
/************************************************************************
 * shp_rdbh                                                             *
 *                                                                      *
 * This function reads information from the database header.          	*
 *                                                                      *
 * shp_rdbh ( fp, dbfhdr, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      *fp             FILE            Database file pointer           *
 *                                                                      *
 * Output parameters:                                                   *
 *	*dbfhdr		dbf_header    	Database header sturcture  	*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         12/03	Initial coding				*
 * S. Jacobs/NCEP	 3/11	Enhance error messages			*
 ***********************************************************************/
 {
    dbf_hdrmap hdrmap;
    dbf_fldmap fldmap;
    int numflds, nbin, ier, ifld;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Position to the beginning of the database file.
     */
    cfl_seek ( fp, 0, SEEK_SET, &ier );

    /*
     * Map the database header to the header map structure, and extract
     * those corresponding fields.
     */
    cfl_read ( fp, DBF_HEADER_SIZE, (unsigned char *)&hdrmap, 
               &nbin, &ier );
    dbfhdr->nrec = (int)( shp_get_llong  ( hdrmap.dbf_nrec ) );
    dbfhdr->hlen = (int)( shp_get_lshort ( hdrmap.dbf_hlen ) );
    dbfhdr->rlen = (int)( shp_get_lshort ( hdrmap.dbf_rlen ) );
    numflds  = (int)((dbfhdr->hlen - DBF_HEADER_SIZE)/DBF_FIELD_SIZE);
    dbfhdr->nfld = numflds;

    if ( debug_flag ) {
	printf ( "Number of records = %d\n", dbfhdr->nrec );
	printf ( "Header length     = %d\n", dbfhdr->hlen );
	printf ( "Record length     = %d\n", dbfhdr->rlen );
	printf ( "Number of fields  = %d\n", dbfhdr->nfld );
    }

    if ( numflds > MAXFLD ) {
        fprintf ( stderr, "Number of fields (%d) exceeds MAXFLD. "
                          "Increase MAXFLD in shpprm.h\n", numflds );
        exit (-1);
    }
    if ( dbfhdr->rlen > MAXRECLEN ) {
        fprintf ( stderr, "Record length (%d) exceeds MAXRECLEN. "
                          "Increase MAXRECLEN in shpprm.h\n", dbfhdr->rlen );
        exit (-1);
    }

    /*
     * Map each database field to the field map structure, and extract
     * those corresponging fields.
     */
    for ( ifld = 0; ifld < numflds; ifld++ ) {
        cfl_read ( fp, DBF_FIELD_SIZE, (unsigned char *)&fldmap, 
	           &nbin, &ier );
	
	strncpy ( dbfhdr->dbflds[ifld].name, (char *)fldmap.fld_name, 
	          FLD_NAMELEN );
	dbfhdr->dbflds[ifld].type = fldmap.fld_type;
	dbfhdr->dbflds[ifld].lens = fldmap.fld_lens;
	dbfhdr->dbflds[ifld].decs = fldmap.fld_decs;
    }

    if ( debug_flag ) {
	printf ( "  Field name   type  length  decimal_count\n" );
	for ( ifld = 0; ifld < numflds; ifld++ ) {
	    printf ( "  %-11.11s  %4d  %6d  %6d\n",
		     dbfhdr->dbflds[ifld].name,
		     dbfhdr->dbflds[ifld].type,
		     dbfhdr->dbflds[ifld].lens,
		     dbfhdr->dbflds[ifld].decs );
	}
    }

}
