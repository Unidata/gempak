#include "geminc.h"
#include "gemprm.h"
#define SHP_GLOBAL
#include "shpprm.h"

/************************************************************************
 * shpssf.c                                                             *
 *                                                                      *
 * This module contains the main program of shpssf.  The program shpssf	*
 * converts map file from the Shapefile format to the Sequential	*
 * Standard Format (SSF).						*
 *                                                                      *
 * CONTENTS:                                                            *
 *      main()           main program of shpssf.                        *
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc, char *argv[] )
/************************************************************************
 * main									*
 *                                                                      *
 * Main program of shpssf.						*
 *                                                                      *
 * int main ( argc, argv )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      argc            int_record      Number of arguments		*
 *	**argv		char		arguments array			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 * R. Tian/SAIC          2/05           Modified shp_mtyp               *
 * T. Piper/SAIC	01/06	Call ip_help if inputs incorrect	*
 * S. Jacobs/NCEP	 3/10	Fixed wording of error messages		*
 * S. Jacobs/NCEP        3/11   Added debug_flag and print statements   *
 * S. Jacobs/NCEP	 6/14	Added more debug output			*
 ***********************************************************************/
{
    dbf_header dbfhdr;
    shx_record shxrec;
    shp_record *reclst, *currec, *newrec;
    shp_part   *prtlst, *curprt, *oneprt;
    char dbfnam[MAXSHP][LLPATH], shxnam[MAXSHP][LLPATH],
	 shpnam[MAXSHP][LLPATH];
    char tpname[LLPATH], hiname[LLPATH], mename[LLPATH], loname[LLPATH];
    char prognm[7];
    FILE *dbffp, *shxfp, *shpfp;
    int nf, opt, rec, prt, file_code, nbin, ier;
    int tres, hres, mres, lres;
    int numrec, numprt, pagflg, numfil;
    long flen;
    float tol, ratio;

/*---------------------------------------------------------------------*/
    tres = G_FALSE;
    hres = G_FALSE;
    mres = G_FALSE;
    lres = G_FALSE;
    pagflg = G_FALSE;
    strcpy ( prognm, "shpssf" );
    tol = 0.005;

    debug_flag = G_TRUE;

/*=================== Parse command line arguments ====================*/

    while ( ( opt = getopt ( argc, argv, "s:t:i:m:l:a:h" ) ) != -1 ) {
        switch ( opt ) {
	    case 's':   /* strip overlapped map line tolerance */
		tol = atof ( optarg );
		if ( tol < 0.0 ) {
		    tol = 0.005;
		}
	   break;

	    case 't':   /* top resolution SSF map file name */
		strcpy ( tpname, optarg );
		tres = G_TRUE;
	    break;

	    case 'i':   /* high resolution SSF map file name */
		strcpy ( hiname, optarg );
		hres = G_TRUE;
	    break;

	    case 'm':   /* medium resolution SSF map file name */
		strcpy ( mename, optarg );
		mres = G_TRUE;
	    break;

	    case 'l':   /* low resolution SSF map file name */
		strcpy ( loname, optarg );
		lres = G_TRUE;
	    break;

	    case 'a':   /* all resolution SSF map file name */
		strcpy ( tpname, "tp" );
		strcat ( tpname, optarg );
		strcpy ( hiname, "hi" );
		strcat ( hiname, optarg );
		strcpy ( mename, "me" );
		strcat ( mename, optarg );
		strcpy ( loname, "lo" );
		strcat ( loname, optarg );
		tres = G_TRUE;
		hres = G_TRUE;
		mres = G_TRUE;
		lres = G_TRUE;
	    break;

	    case 'h':	/* display help */
	    default:
		ip_help ( prognm, &pagflg, &ier, strlen(prognm) );
		exit (0);
	    break;
	}
    }

    if ( optind == argc || argc - optind > MAXSHP ) {
	ip_help ( prognm, &pagflg, &ier, strlen(prognm) );
	exit ( -1 );
    }

/*======================= Starting Pre-Process ========================*/

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

        /*
         * Read shape record and construct an internal list.
         */
	if ( debug_flag ) {
	    printf ( "Total number of records = %d\n", dbfhdr.nrec );
	}
        for ( rec = 0; rec < dbfhdr.nrec; rec++ ) {
	    if ( debug_flag ) {
		printf ( "Processing record %d\n", rec );
	    }
	    /*
	     * Read record field data.
	     */
	    if ( debug_flag ) {
		printf ( "Reading record field data\n" );
	    }
            shp_rdbf ( dbffp, rec, &dbfhdr, &ier );

            /*
	     * Read record index.
	     */
	    if ( debug_flag ) {
		printf ( "Reading record index\n" );
	    }
	    shp_rshx ( shxfp, rec, &shxrec, &ier );

            /*
	     * Read record data.
	     */
	    if ( debug_flag ) {
		printf ( "Reading record data\n" );
	    }
	    shp_rshp ( shpfp, &dbfhdr, &shxrec, &newrec, &ier );

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

        /*
         * Close opened files.
         */
        cfl_clos ( dbffp, &ier );
        cfl_clos ( shxfp, &ier );
        cfl_clos ( shpfp, &ier );
    }

/*======================= Starting Post-Process =======================*/

    /*
     * Split record apart if it has more than MAXOUT number of points
     * or if it crosses the international dateline.
     */
    shp_splt ( reclst, numrec, &ier );

    /*
     * Detect map type.
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
     * Contruct a part list from record list just for map processing.
     */
    prtlst = NULL;
    numprt = 0;
    for ( currec = reclst, rec = 0; rec < numrec;
          rec++, currec = currec->nxtrec ) {
        for ( curprt = currec->shpart, prt = 0;
	      prt < currec->numprt;
	      prt++, curprt = curprt->nxtprt ) {
            if ( prtlst == NULL ) {
                prtlst = curprt;
            } else {
                oneprt->nxtprt = curprt;
                curprt->prvprt = oneprt;
            }
            oneprt = curprt;
	    oneprt->prtflg = PRT_OUT;
	    numprt++;
        }
    }

    /*
     * Strip overlapped map lines.
     */
    shp_strip ( &prtlst, &numprt, tol, &ier );
    shp_join ( &prtlst, &numprt, &ier );

    /*
     * Create top resolution SSF map file.
     */
    if ( tres == G_TRUE ) {
        ratio = 0.005;
	shp_cmap ( prtlst, tpname, ratio, &ier );
    }

    /*
     * Create high resolution SSF map file.
     */
    if ( hres == G_TRUE ) {
        ratio = 0.01;
	shp_cmap ( prtlst, hiname, ratio, &ier );
    }

    /*
     * Create medium resolution SSF map file.
     */
    if ( mres == G_TRUE ) {
        ratio = 0.09;
	shp_cmap ( prtlst, mename, ratio, &ier );
    }

    /*
     * Create low resolution SSF map file.
     */
    if ( lres == G_TRUE ) {
        ratio = 0.4;
	shp_cmap ( prtlst, loname, ratio, &ier );
    }

    /*
     * Clean up memory.
     */
    shp_mfreeall ( );

    return 0;
}
