#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_rshp ( FILE *fp, dbf_header *dbfhdr, shx_record *shxrec, 
                shp_record **shprecp, int *iret )
/************************************************************************
 * shp_rshp                                                             *
 *                                                                      *
 * This function reads one record from the shape file *.shp.            *
 *                                                                      *
 * shp_rshp ( fp, dbfhdr, shxrec, shprecp, iret )               	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *fp             FILE            Shape file pointer              *
 *	*dbfhdr		dbf_header	Database header structure	*
 *      *shxrec         shx_record      Index record structure          *
 *                                                                      *
 * Output parameters:                                                   *
 *	**shprecp	shp_record	One record                   	*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                       -1 = Error                     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04		Initial coding                  *
 ***********************************************************************/
{
    long offset;
    int shp_type, nbin, ier, ifld, iprt, jpts;
    int num_parts, num_points, *parts;
    unsigned char uch4[4], uch16[16];
    float ptx, pty;
    shp_record *newrec;
    shp_part *curprt, *newprt;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Allocate a new shape record.
     */
    newrec = shp_mnew ( SHP_RECSZ );
    newrec->prvrec = NULL;
    newrec->nxtrec = NULL;

    /*
     * Set up the fields.
     */
    newrec->numfld = dbfhdr->nfld;
    for ( ifld = 0; ifld < dbfhdr->nfld; ifld++ ) {
        strcpy ( newrec->fields[ifld].name, dbfhdr->dbflds[ifld].name );
        newrec->fields[ifld].type = dbfhdr->dbflds[ifld].type;
        newrec->fields[ifld].lens = dbfhdr->dbflds[ifld].lens;
        newrec->fields[ifld].decs = dbfhdr->dbflds[ifld].decs;
        strcpy ( newrec->fields[ifld].data, dbfhdr->dbflds[ifld].data );
    }

    /*
     * Position to the beginning of the record content.
     */
    offset = shxrec->offs * 2 + SHP_RECHDR_LEN;
    cfl_seek ( fp, offset, SEEK_SET, &ier );

    /*
     * Get the shape type for this record.
     */
    cfl_read ( fp, INTEGER_SIZE, uch4, &nbin, &ier );
    shp_type = shp_get_llong ( uch4 );

    /*
     * Deal with different shape.
     */
    switch ( shp_type ) {
        case 0:		/* Null Shape */
        break;

	case 1:		/* Point */
	    newrec->numprt = 1;
	    newrec->shpart = shp_mnew ( SHP_PRTSZ );
	    newrec->shpart->prvprt = NULL;
	    newrec->shpart->nxtprt = NULL;
	    newrec->shpart->numpts = 1;
	    newrec->shpart->ptx = shp_mnew ( sizeof(float) );
	    newrec->shpart->pty = shp_mnew ( sizeof(float) );

            cfl_read ( fp, POINT_SIZE, uch16, &nbin, &ier );
	    shp_get_point ( uch16, &ptx, &pty, &ier );
	    newrec->shpart->ptx[0] = ptx;
	    newrec->shpart->pty[0] = pty;
	break;

        case 3:		/* PolyLine */
        case 5:		/* Polygon */
	    /* 
	     *Skip the Box.
	     */
            cfl_seek ( fp, BOX_SIZE, SEEK_CUR, &ier );

	    /*
	     * Get number of parts and number of points.
	     */
            cfl_read ( fp, INTEGER_SIZE, uch4, &nbin, &ier );
            num_parts = shp_get_llong ( uch4 );
            cfl_read ( fp, INTEGER_SIZE, uch4, &nbin, &ier );
            num_points = shp_get_llong ( uch4 );
	    newrec->numprt = num_parts;

	    if ( ( parts = shp_mnew ( (num_parts + 1) * sizeof(int) ) ) 
	        == NULL ) {
	        fprintf ( stderr, "Memory allocation failed.\n" );
	        exit ( -1 );
	    }

	    /*
	     * Get the parts array.
	     */
	    for ( iprt = 0; iprt < num_parts; iprt++ ) {
                cfl_read ( fp, INTEGER_SIZE, uch4, &nbin, &ier );
                parts[iprt] = shp_get_llong ( uch4 );
	    }
	    parts[num_parts] = num_points;

	    /*
	     * Get the points for all parts.
	     */
	    curprt = NULL;
	    for ( iprt = 0; iprt < num_parts; iprt++ ) {
	        newprt = shp_mnew ( SHP_PRTSZ );
		newprt->prvprt = NULL;
		newprt->nxtprt = NULL;
		newprt->numpts = parts[iprt+1] - parts[iprt];
	        newprt->ptx = shp_mnew ( newprt->numpts * sizeof(float) );
	        newprt->pty = shp_mnew ( newprt->numpts * sizeof(float) );
	        for ( jpts = 0; jpts < newprt->numpts; jpts++ ) {
	            cfl_read ( fp, POINT_SIZE, uch16, &nbin, &ier );
		    shp_get_point ( uch16, &ptx, &pty, &ier );
		    newprt->ptx[jpts] = ptx;
		    newprt->pty[jpts] = pty;
		}

		newprt->prvprt = curprt;
		if ( curprt != NULL ) {
		    curprt->nxtprt = newprt;
		} else {
		    newrec->shpart = newprt;
		}
		curprt = newprt;
	    }

            shp_mfree ( parts );
        break;

        default:
	    fprintf ( stderr, "Unrecognized shape type.\n" );
	    *iret = -1;
        break;
    }

    /*
     * Return the new record.
     */
    *shprecp = newrec;
}
