#include "geminc.h"
#include "gemprm.h"
#define SHP_GLOBAL
#include "shpprm.h"

typedef struct shpexception_t {
    char	    id[64];		/* unique id of the shape	*/
    unsigned long   fips;	        /* Fips code of the shape	*/
    float	    clat;		/* lat of centroid point	*/
    float	    clon;		/* lon of centroid point        */
} ShpException_t;

static ShpException_t	*_shpException;
static int		_numShpException = 0;

/*
 * private functions
 */
void shp_rdException ( int *iret );

/************************************************************************
 * shpcv.c                                                              *
 *                                                                      *
 * This module contains the main program of shpcv.  The program shpcv	*
 * creates bound, bound info, and station table from shapefile.		*
 *                                                                      *
 * CONTENTS:                                                            *
 *      main()             main program of shpcv.			*
 *	shp_rdException()  read info from shape exception table		*
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc, char *argv[] )
/************************************************************************
 * main                                                                 *
 *                                                                      *
 * Main program of shpcv.                                               *
 *                                                                      *
 * int main(argc, argv)                                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *  argc		int	number of parameters of command line	*
 *  **argv		char	parameter array of command line		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 3/04		Initial coding			*
 * R. Tian/SAIC		 2/05		Modified shp_mtyp		*
 * T. Piper/SAIC        01/06   Call ip_help if inputs incorrect        *
 * H. Zeng/SAIC		07/07	added calling to shp_rdException()	*
 * S. Jacobs/NCEP	 3/11	Added debug_flag and print statements	*
 * S. Jacobs/NCEP	 6/14	Moved split func to after dump output	*
 ***********************************************************************/
{
    dbf_header dbfhdr;
    shx_record shxrec;
    shp_record *reclst, *newrec, *currec;
    shp_part *curprt;
    char dbfnam[MAXSHP][LLPATH], shxnam[MAXSHP][LLPATH],
	 shpnam[MAXSHP][LLPATH];
    FILE *dbffp, *shxfp, *shpfp;
    long flen;
    int  ifld, nf, opt, rec, prt, file_code, nbin, ier;
    float ratio, clat, clon;
    int lfld, dump, rdpt, cbnd, ctbl, pagflg;
    int numrec, numfil, ii;
    int mode, istat, iunit, itype;
    unsigned long    shp_fips;
    char device[8], proj[8], filnam[20], prognm[6];
    float xsize, ysize, angle1, angle2, angle3, lllat, lllon, urlat,
	  urlon;
    Boolean      exception_found;

/*---------------------------------------------------------------------*/
    lfld = G_FALSE;
    dump = G_FALSE;
    rdpt = G_FALSE;
    cbnd = G_FALSE;
    ctbl = G_FALSE;
    pagflg = G_FALSE;
    strcpy ( prognm, "shpcv" );

    debug_flag = G_TRUE;
/*=================== Parse command line arguments ====================*/

    while ( ( opt = getopt ( argc, argv, "r:btldh" ) ) != -1 ) {
	switch ( opt ) {
	    case 'r':   /* reduce points ratio */
		ratio = atof ( optarg );
		if ( ratio <= 0.0 ) {
		    ratio = 0.01;
		}
		rdpt = G_TRUE;
	    break;

	    case 'b':	/* create bound and bound info */
		cbnd = G_TRUE;
	    break;

	    case 't':	/* create station table */
		ctbl = G_TRUE;
	    break;

	    case 'l':	/* list fields */
		lfld = G_TRUE;
	    break;

	    case 'd':	/* dump the shape file */
		dump = G_TRUE;
	    break;

	    case 'h':	/* display help */
	    default:
		ip_help ( prognm, &pagflg, &ier, strlen(prognm) );
		exit ( 0 );
	    break;
	}
    }

    if ( optind == argc || argc - optind > MAXSHP ) {
	ip_help ( prognm, &pagflg, &ier, strlen(prognm) );
	exit ( -1 );
    }

/*======================== Read the shape file ========================*/

    /*
     * Loop over input files.
     */
    currec = NULL;
    numrec = 0;
    numfil = argc - optind;
    for ( nf = 0; nf < numfil; nf++ ) { 
        /*
         * Get the input names and open them.
         */
	if ( debug_flag ) {
	    printf ( "File %d: %s\n", nf, argv[optind+nf] );
	}
        strcpy ( dbfnam[nf], argv[optind+nf] );
        strcat ( dbfnam[nf], ".dbf" );
        strcpy ( shxnam[nf], argv[optind+nf] );
        strcat ( shxnam[nf], ".shx" );
        strcpy ( shpnam[nf], argv[optind+nf] );
        strcat ( shpnam[nf], ".shp" );

        cfl_inqr ( dbfnam[nf], NULL, &flen, dbfnam[nf], &ier );
        dbffp = cfl_ropn ( dbfnam[nf], NULL, &ier );
        if ( ier != 0 ) {
            fprintf ( stderr, "File %s does not exist.\n", dbfnam[nf] );
	    exit ( -1 );
        }

        cfl_inqr ( shxnam[nf], NULL, &flen, shxnam[nf], &ier );
        shxfp = cfl_ropn ( shxnam[nf], NULL, &ier );
        if ( ier != 0 ) {
            fprintf ( stderr, "File %s does not exist.\n", shxnam[nf] );
	    exit ( -1 );
        }

        cfl_inqr ( shpnam[nf], NULL, &flen, shpnam[nf], &ier );
        shpfp = cfl_ropn ( shpnam[nf], NULL, &ier );
        if ( ier != 0 ) {
            fprintf ( stderr, "File %s does not exist.\n", shpnam[nf] );
	    exit ( -1 );
        }

        /*
         * Detect platform endian.
         */
        cfl_read ( shpfp, INTEGER_SIZE, (unsigned char *)&file_code,
	           &nbin, &ier );
        if ( file_code == 9994 ) {
            mch_endian = BIG;
        } else {
            mch_endian = LITTLE;
        }

        /*
         * Read database header.
         */
        shp_rdbh ( dbffp, &dbfhdr, &ier );
	numrec += dbfhdr.nrec;
        if ( lfld == G_TRUE ) {
            for ( ifld = 0; ifld < dbfhdr.nfld; ifld++ ) {
                printf ( "Field: %s\n", dbfhdr.dbflds[ifld].name );
            }
        } else {
            /*
             * Read shape record and construct an internal list.
             */
            for ( rec = 0; rec < dbfhdr.nrec; rec++ ) {
	        /*
	         * Read record field data. 
	         */
                shp_rdbf ( dbffp, rec, &dbfhdr, &ier );

                /*
	         * Read record index.
	         */
	        shp_rshx ( shxfp, rec, &shxrec, &ier );

                /*
	         * Read record data.
	         */
	        shp_rshp ( shpfp, &dbfhdr, &shxrec,
		           &newrec, &ier );

		/*
		 * Add the new record on record list.
		 */
	        newrec->prvrec = currec;
	        if ( currec != NULL ) {
	            currec->nxtrec = newrec;
	        } else {
	            reclst = newrec;
	        }
	        currec = newrec;
            }
	}

        /*
         * Close up opened files.
         */
        cfl_clos ( dbffp, &ier );
        cfl_clos ( shxfp, &ier );
        cfl_clos ( shpfp, &ier );
    }

    if ( lfld == G_TRUE ) {
        shp_mfreeall ( );
	exit ( 0 );
    }

/*======================== Read the shape exception table =============*/

    _shpException = NULL;
    _numShpException = 0;
    shp_rdException ( &ier );

/*======================= Starting Post-Process =======================*/

    /*
     * Dump shapefile records.
     */
    if ( dump == G_TRUE ) {
	for ( rec = 0, currec = reclst; rec < numrec; 
	      rec++, currec = currec->nxtrec ) {

	    shp_wfld ( stdout, currec, &ier );
	    shp_wrec ( stdout, currec, &ier );
        }

        shp_mfreeall ( );
	exit ( 0 );
    }

    /*
     * Split record part if it has more than MAXOUT number of points
     * or it crosses the international dateline.
     */
    shp_splt ( reclst, numrec, &ier );

    /*
     * Detect the map type.
     */
    shp_mtyp ( dbfnam, numfil, &ier );
    if ( maptyp == 0 ) {
        fprintf ( stderr, "Unknown Map Type.\n" );
	exit ( -1 );
    }

    /*
     * Delete not used records.
     */
    shp_drec ( &reclst, &numrec, &ier );

    /*
     * Combine records that have the same key.
     */
    shp_cmbn ( reclst, &numrec, &ier );

    /*
     * Compute record centroid.
     */
    mode = 1;
    iunit = itype = 1;
    xsize = ysize = 1.0F;
    strcpy ( device, "GN" );
    strcpy ( filnam, "SHPCV" );
    /*
     * Use North STR projection.
     */
    strcpy ( proj, "STR" );
    angle1 = 90.0F;
    angle2 = -90.0F;
    angle3 = 0.0F;
    lllat = -15.0F;
    lllon = -135.0F;
    urlat = -15.0F;
    urlon = -135.0F;
    ginitp ( &mode, &istat, &ier );
    gsdeva ( device, &iunit, filnam, &itype, &xsize, &ysize, &ier,
             strlen(device), strlen(filnam) );
    gsmprj ( proj, &angle1, &angle2, &angle3,
             &lllat, &lllon, &urlat, &urlon, &ier, strlen(proj) );

    for ( currec = reclst, rec = 0; rec < numrec;
          rec++, currec = currec->nxtrec ) {

        /*
         * Get the shape fips code, check if there is a match in
         * Shape Exception Table. If yes, get manually set 
         * clon&clat value from the table directly.
         */
        if ( sscanf(currec->fields[3].data, "%lu", &shp_fips) == 1 ) {

          exception_found = FALSE;

	  for ( ii = 0; ii < _numShpException; ii++ ) {
            if ( _shpException[ii].fips == shp_fips ) {
	      currec->cenlat = _shpException[ii].clat;
	      currec->cenlon = _shpException[ii].clon;
              exception_found = TRUE;
	      break;
            }
          }

          if ( exception_found ) continue;

        }

        /*
         * Get the shape id, check if there is a match in
         * Shape Exception Table. If yes, get manually set 
         * clon&clat value from the table directly.
         */
        exception_found = FALSE;

	for ( ii = 0; ii < _numShpException; ii++ ) {
	  if ( strcasecmp(currec->fields[0].data, _shpException[ii].id) == 0 ) {
	      currec->cenlat = _shpException[ii].clat;
	      currec->cenlon = _shpException[ii].clon;
              exception_found = TRUE;
	      break;
            }
	}

        if ( exception_found ) continue;

        /*
         * Calculate clon&clat mathematically from an algorithm.
         */
        shp_gctr ( currec, &clon, &clat, &ier );
	if ( ier == 0 ) {
	    currec->cenlat = clat;
	    currec->cenlon = clon;
	} else {
	    currec->cenlat = RMISSD;
	    currec->cenlon = RMISSD;
	}

    } /* the end of for ( currec ... */

    /*
     * Reduce number of points.
     */
    if ( rdpt == G_TRUE ) {
        for ( rec = 0, currec = reclst; rec < numrec;
              rec++, currec = currec->nxtrec ) {

	    for ( prt = 0, curprt = currec->shpart; 
	          prt < currec->numprt;
		  prt++, curprt = curprt->nxtprt ) {

                shp_thin ( curprt, ratio, &ier );
	    }
	}
    }

    /*
     * Create station table.
     */
    if ( ctbl == G_TRUE ) {
        shp_ctbl ( reclst, numrec, &ier );
    }

    /*
     * Create bound and bound info.
     */
    if ( cbnd == G_TRUE ) {
        shp_cbnd ( reclst, numrec, &ier );
    }

    /*
     * Clean up.
     */
    shp_mfreeall ( );

    return 0;
}

/*=====================================================================*/

void shp_rdException ( int *iret )
/************************************************************************
 * shp_rdException							*
 *									*
 * This function reads the shape exception table.			*
 *									*
 * void shp_rdException ( iret )					*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret	int		Return value				*
 *                               -1 - Unable to open table      	*
 *				 -2 - Information missing		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		07/07	created					*
 ***********************************************************************/
{
    int		num, ilat, ilon, idx, ier;
    char	tagstr1[128], tagstr2[128], tagstr3[128], tagstr4[128];
    char	tagstr5[128], tagstr6[128], tagstr7[128], buff[256];
    FILE    	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Open the shape exception table. If not found, return an error.
     */
    fp = cfl_tbop("shpexception.tbl", "stns", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }
  
    /*
     * Count # of records on the table.
     */
    cfl_tbnr (fp, &_numShpException, &ier);

    /*
     * Allocate space for _shpException.
     */
    _shpException = (ShpException_t*) malloc( sizeof(ShpException_t) * 
                           _numShpException );

    /*
     *  Scan table line-by-line.
     */
    idx = 0;
    rewind (fp);
    while ( !feof(fp) )  {

	cfl_trln(fp, sizeof(buff), buff, &ier);
	if ( ier != 0 ) continue;

	num = sscanf (buff, "%s %s %s %s %s %s %s", tagstr1, tagstr2,
		      tagstr3, tagstr4, tagstr5, tagstr6, tagstr7);
	if ( num != 7 ) continue; 

	if ( sscanf (tagstr6, "%d",  &ilat) == 1  &&
	     sscanf (tagstr7, "%d",  &ilon) == 1     ) {

	     strcpy ( _shpException[idx].id, tagstr1);
	     _shpException[idx].fips = 0;
	     sscanf (tagstr2, "%lu", &(_shpException[idx].fips));
	     _shpException[idx].clat = ilat * 0.01;
	     _shpException[idx].clon = ilon * 0.01;
	     idx++;
        }
    } /* the end of while (... */

    cfl_clos(fp, &ier);

}

/*=====================================================================*/


