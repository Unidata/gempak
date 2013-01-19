#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

#define KEYLEN 20

void shp_drec ( shp_record **shprecp, int *numrec, int *iret)
/************************************************************************
 * shp_drec                                                             *
 *                                                                      *
 * This function deletes invalid records and records specified by user.	*
 *                                                                      *
 * shp_drec ( shprecp, numrec, iret )                  			*
 *                                                                      *
 * Input parameters:                                                    *
 *      **shprecp       shp_record  	Shape record list		*
 *	*numrec		int		Number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 * X. Guo/CWS            4/12           Increased KEYLEN to 20          *
 ***********************************************************************/
{
    shp_record *shprec, *currec, *tmprec;
    shp_part *curprt, *tmpprt;
    char curkey[20], *delkey, *keys;
    int nkey, fwdcur, delcur, ikey, ier;
    FILE *fp;
/*---------------------------------------------------------------------*/
    *iret = 0;
    fp = NULL;
    nkey = 0;
    keys = NULL;

    switch ( maptyp ) {
        case MAP_MZCN:
            /*
             * Combined Marine and County
             */
	    fp = cfl_tbop ( "mardel.tbl", "stns", &ier );
        break;
    }

    if ( fp != NULL ) {
        cfl_tbnr ( fp, &nkey, &ier );
    }

    if ( nkey > 0 ) {
	keys = shp_mnew ( nkey * KEYLEN );
	for ( ikey = 0; !feof(fp); ikey++ ) {
            cfl_trln ( fp, 256, &keys[ikey*KEYLEN], &ier );
	}
	cfl_clos ( fp, &ier );
    }

    /*
     * Delete records.
     */
    shprec = *shprecp;
    currec = shprec;
    while ( currec != NULL ) {
	delcur = G_FALSE;
	fwdcur = G_FALSE;

	shp_gkey ( currec, curkey, &ier );
	if ( ier == 1 ) {
	    /*
	     * Invalid record.
	     */
	    delcur = G_TRUE;
	}

	for ( ikey = 0; ikey < nkey; ikey++ ) {
	    delkey = &keys[ikey*KEYLEN];
	    if ( strcmp ( curkey, delkey ) == 0 ) {
		/*
		 * Record specified by user for deletion.
		 */
	        delcur = G_TRUE;
	    }
        }

	/*
	 * Remove this record from record list.
	 */
	if ( delcur ) {
	    if ( currec->prvrec != NULL ) {
	        currec->prvrec->nxtrec = currec->nxtrec;
	    } else {
	        /*
	         * This is the record list head.
		 */
		shprec = currec->nxtrec;
		if ( shprec != NULL ) {
		    shprec->prvrec = NULL;
		}
	    }

	    if ( currec->nxtrec != NULL ) {
	        currec->nxtrec->prvrec = currec->prvrec;
	    }

	    curprt = currec->shpart;
	    while ( curprt != NULL ) {
	        tmpprt = curprt;
	        curprt = curprt->nxtprt;

	        shp_mfree ( tmpprt->ptx );
	        shp_mfree ( tmpprt->pty );
		shp_mfree ( tmpprt );
	    }
	    tmprec = currec;
	    currec = currec->nxtrec;
	    fwdcur = G_TRUE;
	    shp_mfree ( tmprec );
	    *numrec -= 1;
        }

	if ( ! fwdcur ) {
	    currec = currec->nxtrec;
	}
    }
    
    if ( keys != NULL ) {
        shp_mfree ( keys );
    }
    *shprecp = shprec;
}
