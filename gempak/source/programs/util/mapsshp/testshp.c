#include "geminc.h"
#include "gemprm.h"
#define SHP_GLOBAL
#include "shpprm.h"

/*
 * private function
 */
void shp_build ( shp_record **shprecp, int *numrec, int *iret );

/************************************************************************
 * testshp.c                                                            *
 *                                                                      *
 * This module contains the main program of testshp.			*
 *                                                                      *
 * CONTENTS:                                                            *
 *      main()           main program of testshp.                       *
 *      shp_build()      build an internal list.                        *
 ***********************************************************************/

/*=====================================================================*/

int main ( void )
/************************************************************************
 * main                                                                 *
 *                                                                      *
 * This program tests the "SHP" functions.                       	*
 *                                                                      *
 * main ( )                                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 6/04                                           *
 * R. Tian/SAIC		 2/05		Modified SHP_MTYP		*
 ***********************************************************************/
{
    shp_record *shprec, *currec;
    shp_part *prtlst, *curprt, *oneprt;
    float filt, cenlat, cenlon;
    int numrec, numprt, rec, prt, ier;
    char select[LLSCRN];
    char reckey[20], header[80], subhdr[80], hdrnam[80];
    char filnms[MAXSHP][LLPATH];
    int cont, numsub, numfil;
    float tol;
    int tltpts1, tltpts2;
    unsigned char byte4[4], byte2[2];
    int ii, ival[4];

    int mode, istat, iunit, itype;
    char device[8], proj[8], filnam[20];
    float xsize, ysize, angle1, angle2, angle3, lllat, lllon, urlat, urlon;
/*---------------------------------------------------------------------*/
    cont = G_TRUE;
    maptyp = 0;

    while ( cont ) {
        printf ( "\n\n" );
        printf ( "   1 = SHP_SPLT   2 = SHP_THIN   3 = SHP_GKEY\n" );
        printf ( "   4 = SHP_CMBN   5 = SHP_BHDR   6 = SHP_CBND\n" );
        printf ( "   7 = SHP_GCTR   8 = SHP_MTYP   9 = SHP_DREC\n" );
        printf ( "  10 = SHP_CTBL  11 = SHP_STRIP 12 = SHP_JOIN\n" );
        printf ( "  13 = SHP_CMAP  14 = SHP_XXXX  15 = SHP_XXXX\n" );
        printf ( "  20 = SHP_GET_LLONG      21 = SHP_GET_LSHORT\n" );
        printf ( "  22 = SHP_GET_BLONG      23 = SHP_GET_BSHORT\n" );
        printf ( "  30 = Build internal data list" );
        printf ( "\n" );
        printf ( "Select a subroutine number or type EXIT: " );
        scanf ( " %s", select );
        switch ( select[0] ) {
            case 'e':
            case 'E':
                    cont = G_FALSE;
            default:
                    numsub = atoi ( select );
                    break;
        }

/*---------------------------------------------------------------------*/
        if ( numsub == 1 ) {
            shp_splt ( shprec, numrec, &ier );
	    currec = shprec;
	    while ( currec ) {
                shp_wfld ( stdout, currec, &ier );
	        shp_wrec ( stdout, currec, &ier );

		currec = currec->nxtrec;
            }
        }
/*---------------------------------------------------------------------*/
        if ( numsub == 2 ) {
            printf ( "Enter a filter factor:\n" );
            scanf ( " %f", &filt );
	    tltpts1 = 0;
	    currec = shprec;
	    while ( currec ) {
	        curprt = currec->shpart;
		while ( curprt ) {
		    tltpts1 += curprt->numpts;
		    curprt = curprt->nxtprt;
		}
	        currec = currec->nxtrec;
	    }

	    currec = shprec;
	    while ( currec ) {
		curprt = currec->shpart;
		while ( curprt ) {
                    shp_thin ( curprt, filt, &ier );

		    curprt = curprt->nxtprt;
		}
                shp_wfld ( stdout, currec, &ier );
	        shp_wrec ( stdout, currec, &ier );
		
		currec = currec->nxtrec;
            }

	    tltpts2 = 0;
	    currec = shprec;
	    while ( currec ) {
	        curprt = currec->shpart;
		while ( curprt ) {
		    tltpts2 += curprt->numpts;
		    curprt = curprt->nxtprt;
		}
	        currec = currec->nxtrec;
	    }

	    printf ( "Number of points reduced from %d to %d, ratio: %%%5.2f\n",
	              tltpts1, tltpts2, (tltpts1-tltpts2)*100.0/tltpts1 );
        }
/*---------------------------------------------------------------------*/
        if ( numsub == 3 ) {
	    if ( maptyp == 0 ) {
	        printf ( "SHP_MTYP has to be run first.\n" );
		continue;
	    }
	    currec = shprec;
	    while ( currec ) {
		shp_gkey ( currec, reckey, &ier );
		printf ( "Key: %s\n", reckey );

		currec = currec->nxtrec;
            }
        }
/*---------------------------------------------------------------------*/
        if ( numsub == 4 ) {
	    if ( maptyp == 0 ) {
	        printf ( "SHP_MTYP has to be run first.\n" );
		continue;
	    }
            shp_cmbn ( shprec, &numrec, &ier );
            for ( rec = 0, currec = shprec; rec < numrec;
                  rec++, currec = currec->nxtrec ) {

                shp_wfld ( stdout, currec, &ier );
	        shp_wrec ( stdout, currec, &ier );
            }
        }
/*---------------------------------------------------------------------*/
        if ( numsub == 5 ) {
	    if ( maptyp == 0 ) {
	        printf ( "SHP_MTYP has to be run first.\n" );
		continue;
	    }
            for ( rec = 0, currec = shprec; rec < numrec; 
                  rec++, currec = currec->nxtrec ) {

		shp_gctr ( currec, &cenlon, &cenlat, &ier );
		currec->cenlat = cenlat;
		currec->cenlon = cenlon;
		shp_bhdr ( currec, header, subhdr, hdrnam, 
		           &cenlat, &cenlon, &ier );
		if ( ier == 1 ) {
		    continue;
		}
		printf ( "Header: %s\n", header );
		printf ( "Subhdr: %s\n", subhdr );
            }
        }
/*---------------------------------------------------------------------*/
        if ( numsub == 6 ) {
	    if ( maptyp == 0 ) {
	        printf ( "SHP_MTYP has to be run first.\n" );
		continue;
	    }
            shp_cbnd ( shprec, numrec, &ier );
        }
/*---------------------------------------------------------------------*/
        if ( numsub == 7 ) {
            for ( rec = 0, currec = shprec; rec < numrec; 
                  rec++, currec = currec->nxtrec ) {

                shp_gctr ( currec, &cenlon, &cenlat, &ier );
		if ( ier == 0 ) {
		    printf ( "rec: %d\tcenlat: %f\tcenlon: %f\n",
		              rec, cenlat, cenlon );
	        } else {
		    printf ( "rec: %d\tcenlat: %f\tcenlon: %f\n",
		              rec, -9999.0, -9999.0 );
		}
	    }
        }
/*---------------------------------------------------------------------*/
        if ( numsub == 8 ) {
	    printf ( " Enter the number of input files:\n" );
	    scanf ( " %d", &numfil );
	    for ( ii = 0; ii < numfil; ii++ ) {
	        printf ( " Enter a file name:\n" );
	        scanf ( " %s", filnms[ii] );
	    }
	    shp_mtyp ( filnms, numfil, &ier );
	    switch ( maptyp ) {
	        case MAP_CNTY:
		    printf ( "County map.\n" );
		break;

	        case MAP_MARI:
		    printf ( "Marine map.\n" );
		break;

	        case MAP_MZCN:
		    printf ( "Combined county marine map.\n" );
		break;

	        case MAP_ZONE:
		    printf ( "Public Forcast Zone map.\n" );
		break;

	        case MAP_IEDG:
		    printf ( "Ice Edge.\n" );
		break;

	        case MAP_FIRE:
		    printf ( "Fire Weather Zone.\n" );
		break;

	        case MAP_RFCR:
		    printf ( "River Forecast Center.\n" );
		break;

	        case MAP_CWAB:
		    printf ( "County Warning Area.\n" );
		break;
	    }
        }
/*---------------------------------------------------------------------*/
        if ( numsub == 9 ) {
	    if ( maptyp == 0 ) {
	        printf ( "SHP_MTYP has to be run first.\n" );
		continue;
	    }
	    shp_drec ( &shprec, &numrec, &ier );
	    currec = shprec;
	    while ( currec ) {
                shp_wfld ( stdout, currec, &ier );
	        shp_wrec ( stdout, currec, &ier );

		currec = currec->nxtrec;
            }
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 10 ) {
	    if ( maptyp == 0 ) {
	        printf ( "SHP_MTYP has to be run first.\n" );
		continue;
	    }
	    shp_ctbl ( shprec, numrec, &ier );
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 11 ) {
	    tol = 0.005;
	    shp_strip ( &prtlst, &numprt, tol, &ier );
	    for ( curprt = prtlst, prt = 0; prt < numprt;
	          prt++, curprt = curprt->nxtprt ) {
	        shp_wprt ( stdout, curprt, &ier );
            }
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 12 ) {
	    shp_join ( &prtlst, &numprt, &ier );
	    curprt = prtlst;
	    while ( curprt ) {
	        shp_wprt ( stdout, curprt, &ier );
		curprt = curprt->nxtprt;
            }
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 13 ) {
	    printf ( " Enter a file name:\n" );
	    scanf ( " %s", filnam );
            printf ( "Enter a filter factor:\n" );
            scanf ( " %f", &filt );
	    shp_cmap ( prtlst, filnam, filt, &ier );
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 20 ) {
	    printf ( " Enter 4 bytes seperated by space in little"
	             " endian:\n" );
	    scanf ( " %d %d %d %d", &ival[0], &ival[1], 
	                            &ival[2], &ival[3] );
	    for ( ii = 0; ii < 4; ii++ ) {
	        byte4[ii] = ival[ii];
	    }
	    printf ( "Integer value: %d\n", (int)shp_get_llong(byte4) );
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 21 ) {
	    printf ( " Enter 2 bytes seperated by space in little"
	             " endian:\n" );
	    scanf ( " %d %d",&ival[0], &ival[1]);
	    for ( ii = 0; ii < 2; ii++ ) {
	        byte2[ii] = ival[ii];
	    }
	    printf ( "Integer value: %d\n", shp_get_lshort(byte2) );
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 22 ) {
	    printf ( " Enter 4 bytes seperated by space in big"
	             " endian:\n" );
	    scanf ( " %d %d %d %d", &ival[0], &ival[1], 
	                            &ival[2], &ival[3] );
	    for ( ii = 0; ii < 4; ii++ ) {
	        byte4[ii] = ival[ii];
	    }
	    printf ( "Integer value: %d\n", (int)shp_get_blong(byte4) );
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 23 ) {
	    printf ( " Enter 2 bytes seperated by space in big"
	             " endian:\n" );
	    scanf ( " %d %d",&ival[0], &ival[1]);
	    for ( ii = 0; ii < 2; ii++ ) {
	        byte2[ii] = ival[ii];
	    }
	    printf ( "Integer value: %d\n", shp_get_bshort(byte2) );
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 30 ) {
    /*
     * Build a record list from the test data file.
     */
    shp_build ( &shprec, &numrec, &ier );
    prtlst = NULL;
    numprt = 0;
    for ( currec = shprec, rec = 0; rec < numrec;
          rec++, currec = currec->nxtrec ) {
	for ( curprt = currec->shpart, prt = 0; prt < currec->numprt;
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
     * Init GEMPLT and set DEVICE and PROJ. Use North STR projection.
     */
    mode = 1;
    iunit = itype = 1;
    xsize = ysize = 1.0F;
    strcpy ( device, "GN" );
    strcpy ( filnam, "SHPCV" );
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

	}
/*---------------------------------------------------------------------*/
    }

    shp_mfreeall ();
    return 0;
}

void shp_build ( shp_record **shprecp, int *numrec, int *iret )
/************************************************************************
 * shp_build                                                            *
 *                                                                      *
 * This function builds an internal list.				*
 *                                                                      *
 * shp_build ( shprecp, numrec, iret )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      **shprecp       shp_record      Shape record list               *
 *      *numrec         int             Total number of records         *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 ***********************************************************************/
{
    shp_record *shprec, *onerec, *currec;
    shp_part *oneprt, *curprt;
    char fname[LLPATH], buffer[2048], *cp;
    FILE *fp;
    long flen;
    int numpts, rempts, fldlen, newrec, pidx, ier, i;
    float maxlat, minlat, maxlon, minlon, ptx[4], pty[4];
/*---------------------------------------------------------------------*/
    *iret = 0;

    printf ( "Enter the test data file name for testing:\n" );
    scanf ( " %s", fname );
    cfl_inqr ( fname, NULL, &flen, fname, &ier );
    fp = cfl_ropn ( fname, NULL, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s does not exist.\n", fname );
	exit ( -1 );
    }

    newrec = G_TRUE;
    *numrec = 0;
    currec = NULL;
    while ( fgets ( buffer, sizeof(buffer), fp ) ) {
        if ( buffer[0] == 'F' ) {
	    /*
	     * Fields
	     */
	    if ( newrec ) {
	        onerec = shp_mnew ( SHP_RECSZ );
		onerec->prvrec = currec;
		onerec->nxtrec = NULL;
		if ( currec != NULL ) {
		    currec->nxtrec = onerec;
		} else {
		    shprec = onerec;
		}
		currec = onerec;
		*numrec += 1;

		currec->numfld = 0;
		curprt = NULL;
	        newrec = G_FALSE;
	    }

	    cp = strchr ( buffer, ':' );
	    fldlen = cp - buffer - 6;
	    strncpy ( currec->fields[currec->numfld].name,
	              buffer+6, fldlen );
	    currec->fields[currec->numfld].name[fldlen+1] = '\0';
	    strcpy ( currec->fields[currec->numfld].data, cp+3 );
	    cp = strchr ( currec->fields[currec->numfld].data, '\n' );
	    if ( cp != NULL ) {
	        *cp = '\0';
	    }
	    currec->numfld++;
	} else {
	    /*
	     * Data
	     */
	    newrec = G_TRUE;

	    if ( strstr ( buffer, "              " ) ) { /* 14 blanks */
	        oneprt = shp_mnew ( SHP_PRTSZ );
		oneprt->prvprt = curprt;
		if ( curprt != NULL ) {
		    curprt->nxtprt = oneprt;
		} else {
		    currec->shpart = oneprt;
		}
		curprt = oneprt;
		currec->numprt++;

		sscanf ( buffer,
		    "%d              %f%f%f%f%f%f\n",
		    &numpts, &maxlat, &minlat, &maxlon, &minlon, 
		    &pty[0], &ptx[0] );
		curprt->numpts = numpts / 2;
		curprt->ptx = shp_mnew ( oneprt->numpts * sizeof(float) );
		curprt->pty = shp_mnew ( oneprt->numpts * sizeof(float) );
		pidx = 0;

		curprt->ptx[pidx] = ptx[0]; 
		curprt->pty[pidx] = pty[0];
		pidx++;
	    } else {
		sscanf ( buffer, 
		    "%f%f%f%f%f%f%f%f\n",
		    &pty[0], &ptx[0], &pty[1], &ptx[1],
		    &pty[2], &ptx[2], &pty[3], &ptx[3] );

		if ( numpts / 2 - pidx >=4 ) {
		    for ( i = 0; i < 4; i++ ) {
		        curprt->ptx[pidx] = ptx[i];
			curprt->pty[pidx] = pty[i];
			pidx++;
		    }
		} else {
		    rempts = numpts / 2 - pidx;
		    for ( i = 0; i < rempts; i++ ) {
		        curprt->ptx[pidx] = ptx[i];
			curprt->pty[pidx] = pty[i];
			pidx++;
		    }
		}
	    }
	}
    }

    *shprecp = shprec;
    cfl_clos ( fp, &ier );
}
