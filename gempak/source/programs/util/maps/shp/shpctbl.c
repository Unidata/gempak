#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

/*
 * private function
 */
static void abbreviate ( char *instr, int reqlen, char *outstr, 
                         int *iret );
static void tbl_cnty ( shp_record *shprec, int numrec, int *iret );
static void tbl_mari ( shp_record *shprec, int numrec, int *iret );
static void tbl_mzcn ( shp_record *shprec, int numrec, int *iret );
static void tbl_zone ( shp_record *shprec, int numrec, int *iret );
static void tbl_rfcb ( shp_record *shprec, int numrec, int *iret );
static void tbl_usst ( shp_record *shprec, int numrec, int *iret );
static void tbl_npsa ( shp_record *shprec, int numrec, int *iret );

/************************************************************************
 * shp_ctbl.c								*
 *									*
 * This module creates station table files				*
 *									*
 * CONTENTS:								*
 *  abbreviate()		abbreviate a full name			*
 *  shp_ctbl()			create station table files		*
 *  tbl_cnty()			create county table			*
 *  tbl_mari()			create marine table			*
 *  tbl_zone()			create public forecast zones table	*
 *  tbl_rfcb()			create river forecast center basin tbl	*
 *  tbl_usst()			create us state and territories tbl	*
 *  tbl_npsa()			create predictive service areas tbl	*
 ***********************************************************************/

/*=====================================================================*/

void shp_ctbl ( shp_record *shprec, int numrec, int *iret )
/************************************************************************
 * shp_ctbl                                                             *
 *                                                                      *
 * This function creates the station table files.                	*
 *                                                                      *
 * shp_ctbl ( shprec, numrec, iret )                           		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *shprec         shp_record  	Shape record list		*
 *	numrec		int		Total number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 * R. Tian/SAIC          2/05           Added MAP_FIRE			*
 * R. Tian/SAIC          4/05           Added MAP_RFCB			*
 * R. Tian/SAIC          6/05           Added MAP_USSC			*
 * S. Jacobs/NCEP	 4/10		Added MAP_NPSA			*
 ***********************************************************************/
{
    int ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    switch ( maptyp ) {
        case MAP_CNTY: 
	    /*
	     * County
	     */
	    tbl_cnty ( shprec, numrec, &ier );
        break;

        case MAP_MARI: 
	    /*
	     * Marine
	     */
	    tbl_mari ( shprec, numrec, &ier );
        break;

        case MAP_MZCN: 
	    /*
	     * Combined marine-county
	     */
	    tbl_mzcn ( shprec, numrec, &ier );
        break;

        case MAP_ZONE: 
        case MAP_FIRE: 
	    /*
	     * Public Forecast Zone and Fire Forecast Zone.
	     */
	    tbl_zone ( shprec, numrec, &ier );
        break;

        case MAP_RFCB: 
	    /*
	     * River Forecast Center Basin.
	     */
	    tbl_rfcb ( shprec, numrec, &ier );
        break;

        case MAP_USST: 
	    /*
	     * US State and Territories.
	     */
	    tbl_usst ( shprec, numrec, &ier );
        break;

        case MAP_NPSA: 
	    /*
	     * Predictive Service Areas
	     */
	    tbl_npsa ( shprec, numrec, &ier );
        break;
    }
}

static void abbreviate ( char *instr, int reqlen, char *outstr, 
                                                              int *iret )
/************************************************************************
 * abbreviate                                                           *
 *                                                                      *
 * This function abbreviates the long name.				*
 *                                                                      *
 * abbreviate ( instr, reqlen, outstr, iret )           		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*instr		char		Input string			*
 *	reqlen		int		Required output string length	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*outstr		char		Output string			*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 ***********************************************************************/
{
    char buffer1[1000], *bf1p;
    char buffer2[1000], *bf2p;
    char *tmp;
    int ii;
/*---------------------------------------------------------------------*/
    *iret = 0;

    if ( strlen(instr) > sizeof(buffer1) ) {
        *iret = -1;
	return;
    }

    strcpy ( buffer1, instr );
    bf1p = buffer1;
    bf2p = buffer2;

    for ( ii = 0; ii < NUMABBR * 2; ii += 2 ) {
        /*
         * Replace all occureance.
	*/
        do {
            cst_rpst ( bf1p, abbrev_tbl[ii+1], abbrev_tbl[ii], 
	               bf2p, iret );
	    tmp = bf1p;
	    bf1p = bf2p;
	    bf2p = tmp;
        } while ( strcmp ( bf1p, bf2p ) );

        if ( strlen ( bf1p ) < (size_t)reqlen ) {
            break;
        }
    }

    strcpy ( outstr, bf1p );
}

static void tbl_cnty ( shp_record *shprec, int numrec, int *iret )
/************************************************************************
 * tbl_cnty                                                             *
 *                                                                      *
 * This function creates the county station table files.               	*
 *                                                                      *
 * tbl_cnty ( shprec, numrec, iret )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *shprec         shp_record  	Shape record list		*
 *	numrec		int		Total number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 ***********************************************************************/
{
    shp_record *currec;
    char ctynam[25], shrnam[33], cwanam[10], stabbr[3];
    int ifips, ilat, ilon;
    int irec, jfld, pos, len, ier;
    float shplat, shplon;
    char *ctytbl = COUNTYTBL, *namtbl = COUNTYNAMTBL;
    FILE *tblfp, *namfp;
/*---------------------------------------------------------------------*/
    *iret = 0;

    tblfp = cfl_wopn ( ctytbl, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", ctytbl );
        exit ( -1 );
    }
    namfp = cfl_wopn ( namtbl, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", namtbl );
        exit ( -1 );
    }

    for ( currec = shprec, irec = 0; irec < numrec;
        irec++, currec = currec->nxtrec ) {
        for ( jfld = 0; jfld < currec->numfld; jfld++ ) {
            if ( strncmp ( currec->fields[jfld].name, "COUNTYNAME",
                strlen("COUNTYNAME") ) == 0 ) {
                strcpy ( ctynam, currec->fields[jfld].data );
		cst_lstr ( ctynam, &len, &ier );
		ctynam[len] = '\0';
		cst_rspc ( ctynam, &ier );
		abbreviate ( ctynam, 32, shrnam, &ier );

		/*
		 * Remove "City_of_" from county name
		 */
	        cst_rmst ( shrnam, "City_of_", &pos, shrnam, &ier );

		/*
		 * Remove "Lower_Keys_in_" from county name
		 */
	        cst_rmst ( shrnam, "Lower_Keys_in_", &pos, shrnam, &ier );

		/*
		 * Remove "Outer_Banks_" from county name
		 */
	        cst_rmst ( shrnam, "Outer_Banks_", &pos, shrnam, &ier );

		/*
		 * Remove "Mainland_" from county name
		 */
	        cst_rmst ( shrnam, "Mainland_", &pos, shrnam, &ier );
            } else if ( strncmp ( currec->fields[jfld].name, "STATE",
	        strlen ( "STATE" ) ) == 0 ) {
	        strcpy ( stabbr, currec->fields[jfld].data );
	    } else if ( strncmp ( currec->fields[jfld].name, "CWA",
	        strlen ( "CWA" ) ) == 0 ) {
	        strcpy ( cwanam, currec->fields[jfld].data );
		cst_lstr ( cwanam, &len, &ier );
		cwanam[len] = '\0';
	    } else if ( strncmp ( currec->fields[jfld].name, "FIPS",
                strlen("FIPS") ) == 0 ) {
                ifips = atoi ( currec->fields[jfld].data );
	    } else if ( strncmp ( currec->fields[jfld].name, "LAT",
                strlen("LAT") ) == 0 ) {
                shplat = atof ( currec->fields[jfld].data );
	    } else if ( strncmp ( currec->fields[jfld].name, "LON",
                strlen("LON") ) == 0 ) {
                shplon = atof ( currec->fields[jfld].data );
            }
	}
	if ( strlen ( shrnam ) == (size_t)0 || strlen ( stabbr ) == (size_t)0 ) {
	    continue;
	}

	if ( ERMISS ( currec->cenlat ) || ERMISS ( currec->cenlon ) ) {
	    ilat = (int)(ROUNDUP(shplat)*100.);
	    ilon = (int)(ROUNDUP(shplon)*100.);
	} else {
	    ilat = (int)(ROUNDUP(currec->cenlat)*100.);
	    ilon = (int)(ROUNDUP(currec->cenlon)*100.);
	}

        fprintf ( tblfp, 
          "%2.2sC%3.3d   %6d %-32.32s %2.2s US %5d %6d %5d %2d %-3.3s\n",
          stabbr, ifips%1000, ifips, shrnam, stabbr,
          ilat, ilon, 0, 0, cwanam );
        fprintf ( namfp, 
	  "%-8.8s %6d %-32.32s %2.2s US %5d %6d %5d %2d %-3.3s\n",
	  shrnam, ifips, shrnam, stabbr, ilat, ilon, 0, 0, cwanam );
    }

    cfl_clos ( tblfp, &ier );
    cfl_clos ( namfp, &ier );
}

static void tbl_mari ( shp_record *shprec, int numrec, int *iret )
/************************************************************************
 * tbl_mari                                                             *
 *                                                                      *
 * This function creates the marine station table files.               	*
 *                                                                      *
 * tbl_mari ( shprec, numrec, iret )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *shprec         shp_record  	Shape record list		*
 *	numrec		int		Total number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 * S. Gilbert/NCEP      11/13       Restrict fulnam to 250 chars        *
 ***********************************************************************/
{
    shp_record *currec;
    /*
     * The array size is from the Costal Marine Zones attributes
     * specification.
     */
    char id[7], fulnam[251], shrnam[33], wfonam[4], stabbr[3];
    int ifips, istno, izone, ilat, ilon, fullen=250;
    int irec, jfld, len, ier;
    char *tblnam = MARINETBL;
    float shplat, shplon;
    FILE *tblfp;
/*---------------------------------------------------------------------*/
    *iret = 0;

    tblfp = cfl_wopn ( tblnam, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", tblnam );
        exit ( -1 );
    }

    for ( currec = shprec, irec = 0; irec < numrec;
        irec++, currec = currec->nxtrec ) {
        for ( jfld = 0; jfld < currec->numfld; jfld++ ) {
            if ( strncmp ( currec->fields[jfld].name, "ID",
                strlen("ID") ) == 0 ) {
		        strncpy ( id, currec->fields[jfld].data, 6 );
		        id[6] = '\0';
                strncpy ( stabbr, currec->fields[jfld].data, 2 );
                stabbr[2] = '\0';
                izone = atoi ( currec->fields[jfld].data + 3 );
                for ( istno = 0; istno < NUMSTNO; istno++ ) {
                    if ( strncmp ( stateno[istno], stabbr, 2 ) == 0 ) {
                        break;
                    }
                }
                ifips = istno * 10000 + izone * 10;
            } else if ( strncmp ( currec->fields[jfld].name, "WFO",
                strlen ( "WFO" ) ) == 0 ) {
                strncpy ( wfonam, currec->fields[jfld].data, 3 );
		        wfonam[3] = '\0';
            } else if ( strncmp ( currec->fields[jfld].name, "NAME",
                strlen ( "NAME" ) ) == 0 ) {
        		if (strlen(currec->fields[jfld].data) <= fullen )
                    strcpy ( fulnam, currec->fields[jfld].data );
        		else {
        			strncpy ( fulnam, currec->fields[jfld].data, fullen );
        		    fulnam[fullen]='\0';
                }
		        cst_lstr ( fulnam, &len, &ier );
		        fulnam[len] = '\0';
                cst_rspc ( fulnam, &ier );
		        abbreviate ( fulnam, 32, shrnam, &ier );
	        } else if ( strncmp ( currec->fields[jfld].name, "LAT",
                strlen("LAT") ) == 0 ) {
                shplat = atof ( currec->fields[jfld].data );
	        } else if ( strncmp ( currec->fields[jfld].name, "LON",
                strlen("LON") ) == 0 ) {
                shplon = atof ( currec->fields[jfld].data );
            }
        }

	if ( ERMISS ( currec->cenlat ) || 
	    ERMISS ( currec->cenlon ) ) {
	    ilat = (int)(ROUNDUP(shplat)*100.);
	    ilon = (int)(ROUNDUP(shplon)*100.);
	} else {
	    ilat = (int)(ROUNDUP(currec->cenlat)*100.);
	    ilon = (int)(ROUNDUP(currec->cenlon)*100.);
	}

        fprintf ( tblfp,
            "%6.6s   %6d %-32.32s -- US %5d %6d %5d %2d %-3.3s\n",
            id, ifips, shrnam, ilat, ilon, 0, 0, wfonam );
    }

    cfl_clos ( tblfp, &ier );
}

static void tbl_mzcn ( shp_record *shprec, int numrec, int *iret )
/************************************************************************
 * tbl_mzcn                                                             *
 *                                                                      *
 * This function creates the combined county and marine station tables.	*
 *                                                                      *
 * tbl_mzcn ( shprec, numrec, iret )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *shprec         shp_record  	Shape record list		*
 *	numrec		int		Total number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   Initial coding                  	*
 * S. Jacobs/NCEP	 3/11	Changed header keys for determining	*
 * 				the type of file - county or marine	*
 * X.Guo/CWS             9/11   Increased fulname size and chnaged      *
 *                              SHAPE_LENG to COUNTYNAME                *
 ***********************************************************************/
{
    shp_record *currec;
    /*
     * The array size is from the Costal Marine Zones and AWIPS Counties
     * attributes specification.
     */
    char fulnam[500], shrnam[33], cwanam[10], stabbr[3];
    int ifips, istno, izone, ilat, ilon;
    int irec, jfld, kfld, pos, len, ier;
    char *stntbl = MZCNTYTBL, *namtbl = MZCNTYNAMTBL; 
    float shplat, shplon;
    FILE *tblfp, *namfp;
/*---------------------------------------------------------------------*/
    *iret = 0;

    tblfp = cfl_wopn ( stntbl, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", stntbl );
        exit ( -1 );
    }
    namfp = cfl_wopn ( namtbl, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", namtbl );
        exit ( -1 );
    }

    for ( currec = shprec, irec = 0; irec < numrec;
        irec++, currec = currec->nxtrec ) {

        for ( jfld = 0; jfld < currec->numfld; jfld++ ) {
            if ( strncmp ( currec->fields[jfld].name, "COUNTYNAME",
                strlen("COUNTYNAME") ) == 0 ) {
                /*
                 * County.
                 */
                for ( kfld = 0; kfld < currec->numfld; kfld++ ) {
                    if ( strncmp ( currec->fields[kfld].name, 
		        "COUNTYNAME", strlen("COUNTYNAME") ) == 0 ) {
                        strcpy ( fulnam, currec->fields[kfld].data );
			cst_lstr ( fulnam, &len, &ier );
			fulnam[len] = '\0';
                        cst_rspc ( fulnam, &ier );
                        abbreviate ( fulnam, 32, shrnam, &ier );

                        /*
                         * Remove "City_of_" from county name
                         */
                        cst_rmst ( shrnam, "City_of_", &pos,
                                   shrnam, &ier );

                        /*
                         * Remove "Lower_Keys_in_" from county name
                         */
                        cst_rmst ( shrnam, "Lower_Keys_in_", &pos,
                                   shrnam, &ier );

                        /*
                         * Remove "Outer_Banks_" from county name
                         */
                        cst_rmst ( shrnam, "Outer_Banks_", &pos,
                                   shrnam, &ier );

                        /*
                         * Remove "Mainland_" from county name
                         */
                        cst_rmst ( shrnam, "Mainland_", &pos,
                                   shrnam, &ier );
                    } else if ( strncmp ( currec->fields[kfld].name, 
		        "STATE", strlen ( "STATE" ) ) == 0 ) {
                        strcpy ( stabbr, currec->fields[kfld].data );
                    } else if ( strncmp ( currec->fields[kfld].name, 
		        "CWA", strlen ( "CWA" ) ) == 0 ) {
                        strcpy ( cwanam, currec->fields[kfld].data );
			cst_lstr ( cwanam, &len, &ier );
			cwanam[len] = '\0';
                    } else if ( strncmp ( currec->fields[kfld].name, 
		        "FIPS", strlen("FIPS") ) == 0 ) {
                        ifips = atoi ( currec->fields[kfld].data );
	            } else if ( strncmp ( currec->fields[kfld].name, 
		        "LAT", strlen("LAT") ) == 0 ) {
                        shplat = atof ( currec->fields[kfld].data );
	            } else if ( strncmp ( currec->fields[kfld].name, 
		        "LON", strlen("LON") ) == 0 ) {
                        shplon = atof ( currec->fields[kfld].data );
                    }
                }
                if ( strlen ( shrnam ) == (size_t)0 || strlen ( stabbr ) == (size_t)0 ) {
                    continue;
                }
	        if ( ERMISS ( currec->cenlat ) || 
		     ERMISS ( currec->cenlon ) ) {
	            ilat = (int)(ROUNDUP(shplat)*100.);
	            ilon = (int)(ROUNDUP(shplon)*100.);
	        } else {
	            ilat = (int)(ROUNDUP(currec->cenlat)*100.);
	            ilon = (int)(ROUNDUP(currec->cenlon)*100.);
	        }

                fprintf ( tblfp,
            "%2.2sC%3.3d   %6d %-32.32s %2.2s US %5d %6d %5d %2d %-3.3s\n",
                stabbr, ifips%1000, ifips, shrnam, stabbr,
                 ilat, ilon, 0, 0, cwanam );
            } else if ( strncmp ( currec->fields[jfld].name, "NAME",
                strlen("NAME") ) == 0 ) {
                /*
                 * Marine.
                 */
                for ( kfld = 0; kfld < currec->numfld; kfld++ ) {
                    if ( strncmp ( currec->fields[kfld].name, "ID",
                        strlen("ID") ) == 0 ) {
                        strcpy ( shrnam, currec->fields[kfld].data );
                        strncpy ( stabbr, currec->fields[kfld].data, 2 );
                        stabbr[2] = '\0';
                        izone = atoi ( currec->fields[kfld].data + 3 );
                        for ( istno = 0; istno < NUMSTNO; istno++ ) {
                            if ( strncmp ( stateno[istno], stabbr, 2 ) 
			        == 0 ) {
                                break;
                            }
                        }
                        ifips = istno * 10000 + izone * 10;
                    } else if ( strncmp ( currec->fields[kfld].name, 
		        "WFO", strlen ( "WFO" ) ) == 0 ) {
                        strcpy ( cwanam, currec->fields[kfld].data );
                    } else if ( strncmp ( currec->fields[kfld].name, 
		        "NAME", strlen ( "NAME" ) ) == 0 ) {
                        strcpy ( fulnam, currec->fields[kfld].data );
			cst_lstr ( fulnam, &len, &ier );
			fulnam[len] = '\0';
                        cst_rspc ( fulnam, &ier );
	            } else if ( strncmp ( currec->fields[kfld].name, 
		        "LAT", strlen("LAT") ) == 0 ) {
                        shplat = atof ( currec->fields[kfld].data );
	            } else if ( strncmp ( currec->fields[kfld].name, 
		        "LON", strlen("LON") ) == 0 ) {
                        shplon = atof ( currec->fields[kfld].data );
                    }
                } 

	        if ( ERMISS ( currec->cenlat ) || 
		     ERMISS ( currec->cenlon ) ) {
	            ilat = (int)(ROUNDUP(shplat)*100.);
	            ilon = (int)(ROUNDUP(shplon)*100.);
	        } else {
	            ilat = (int)(ROUNDUP(currec->cenlat)*100.);
	            ilon = (int)(ROUNDUP(currec->cenlon)*100.);
	        }

                fprintf ( tblfp,
                 "%6.6s   %6d %-32.32s %2.2s US %5d %6d %5d %2d %-3.3s\n",
                 shrnam, ifips, shrnam, stabbr, ilat, ilon, 0, 0, cwanam );
                fprintf (namfp, "%s %s\n", shrnam, fulnam );
            }
        }
    }

    cfl_clos ( tblfp, &ier );
    cfl_clos ( namfp, &ier );
}

static void tbl_zone ( shp_record *shprec, int numrec, int *iret )
/************************************************************************
 * tbl_zone                                                             *
 *                                                                      *
 * This function creates the public forecast zone and fire weather zone	*
 * station table files.							*
 *                                                                      *
 * tbl_zone ( shprec, numrec, iret )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *shprec         shp_record  	Shape record list		*
 *	numrec		int		Total number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 ***********************************************************************/
{
    shp_record *currec;
    /*
     * The array size is from the Public Forecast Zones attributes 
     * specification.
     */
    char zonenm[255], shrnam[33], cwanam[4], stabbr[3];
    int ifips, izone, istno, iclat, iclon;
    int rec, ifld, len, ier;
    float shplat, shplon;
    char *tblnam = PFZTBL;
    FILE *tblfp;
/*---------------------------------------------------------------------*/
    *iret = 0;

    tblfp = cfl_wopn ( tblnam, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", tblnam );
        exit ( -1 );
    }

    for ( currec = shprec, rec = 0; rec < numrec;
        rec++, currec = currec->nxtrec ) {

        for ( ifld = 0; ifld < currec->numfld; ifld++ ) {
            if ( strcmp ( currec->fields[ifld].name, "STATE" ) == 0 ) {
	        strcpy ( stabbr, currec->fields[ifld].data );
            } else if ( strncmp ( currec->fields[ifld].name, "ZONE",
	        strlen ( "ZONE" ) ) == 0 ) {
		izone = strtol ( currec->fields[ifld].data, NULL, 10 );
	    } else if ( strncmp ( currec->fields[ifld].name, "CWA",
	        strlen ( "CWA" ) ) == 0 ) {
	        strncpy ( cwanam, currec->fields[ifld].data, 3 );
	        cwanam[3] = '\0';
	    } else if ( strncmp ( currec->fields[ifld].name, "LAT",
                strlen("LAT") ) == 0 ) {
                shplat = atof ( currec->fields[ifld].data );
	    } else if ( strncmp ( currec->fields[ifld].name, "LON",
                strlen("LON") ) == 0 ) {
                shplon = atof ( currec->fields[ifld].data );
            } else if ( strncmp ( currec->fields[ifld].name, "NAME",
	        strlen("NAME") ) == 0 ) {
		strcpy ( zonenm, currec->fields[ifld].data );
		cst_lstr ( zonenm, &len, &ier );
		zonenm[len] = '\0';
		cst_rspc ( zonenm, &ier );
		abbreviate ( zonenm, 32, shrnam, &ier );
	    }
	}

        for ( istno = 0; istno < NUMSTNO; istno++ ) {
            if ( strncmp ( stateno[istno], stabbr, 2 ) == 0 ) {
                break;
            }
        }
        ifips = istno * 10000 + izone * 10;
        iclat = (int)(ROUNDUP(shplat)*100.);
        iclon = (int)(ROUNDUP(shplon)*100.);
        if ( iclat == 0 && iclon == 0 ) {
            iclat = (int)(ROUNDUP(currec->cenlat)*100.);
            iclon = (int)(ROUNDUP(currec->cenlon)*100.);
        }

        fprintf ( tblfp, 
          "%2.2sZ%3.3d   %6d %-32.32s %2.2s US %5d %6d %5d %2d %-3.3s\n",
          stabbr, izone, ifips, shrnam, stabbr, iclat, iclon, 0, 0, cwanam );
    }

    cfl_clos ( tblfp, &ier );
}

static void tbl_rfcb ( shp_record *shprec, int numrec, int *iret )
/************************************************************************
 * tbl_rfcb                                                             *
 *                                                                      *
 * This function creates the river forecast center basin table file.	* 
 *                                                                      *
 * tbl_rfcb ( shprec, numrec, iret )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *shprec         shp_record  	Shape record list		*
 *	numrec		int		Total number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          4/04   	Initial coding                  *
 ***********************************************************************/
{
    shp_record *currec;
    /*
     * The array size is from the River Forecast Center Basin
     * specification.
     */
    char basid[9], basnam[65], shrnam[33], cwanam[13], rfcid[6];
    int iclat, iclon;
    int rec, ifld, len, ier;
    float shplat, shplon;
    char *tblnam = RIVERBASINTBL;
    FILE *tblfp;
/*---------------------------------------------------------------------*/
    *iret = 0;

    tblfp = cfl_wopn ( tblnam, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", tblnam );
        exit ( -1 );
    }

    for ( currec = shprec, rec = 0; rec < numrec;
        rec++, currec = currec->nxtrec ) {

        for ( ifld = 0; ifld < currec->numfld; ifld++ ) {
            if ( strncmp ( currec->fields[ifld].name, "ID",
	        strlen ( "ID" ) ) == 0 ) {
	        strcpy ( basid, currec->fields[ifld].data );
		cst_lstr ( basid, &len, &ier );
		basid[len] = '\0';
            } else if ( strncmp ( currec->fields[ifld].name, "NAME",
	        strlen ( "NAME" ) ) == 0 ) {
		strcpy ( basnam, currec->fields[ifld].data );
		cst_lstr ( basnam, &len, &ier );
		basnam[len] = '\0';
		cst_rspc ( basnam, &ier );
		abbreviate ( basnam, 32, shrnam, &ier );
	    } else if ( strncmp ( currec->fields[ifld].name, "CWA",
	        strlen ( "CWA" ) ) == 0 ) {
	        strcpy ( cwanam, currec->fields[ifld].data );
		cst_lstr ( cwanam, &len, &ier );
	        cwanam[len] = '\0';
	    } else if ( strncmp ( currec->fields[ifld].name, "LAT",
                strlen("LAT") ) == 0 ) {
                shplat = atof ( currec->fields[ifld].data );
	    } else if ( strncmp ( currec->fields[ifld].name, "LON",
                strlen("LON") ) == 0 ) {
                shplon = atof ( currec->fields[ifld].data );
            } else if ( strncmp ( currec->fields[ifld].name, "RFC",
	        strlen("RFC") ) == 0 ) {
		strcpy ( rfcid, currec->fields[ifld].data );
		cst_lstr ( rfcid, &len, &ier );
		rfcid[len] = '\0';
	    }
	}

        iclat = (int)(ROUNDUP(shplat)*100.);
        iclon = (int)(ROUNDUP(shplon)*100.);
        if ( iclat == 0 && iclon == 0 ) {
            iclat = (int)(ROUNDUP(currec->cenlat)*100.);
            iclon = (int)(ROUNDUP(currec->cenlon)*100.);
        }

        fprintf ( tblfp, 
          "%-6.6s   %6s %-32.32s -- US %5d %6d %5d %2d %-3.3s\n",
          basid, rfcid, shrnam, iclat, iclon, 0, 0, cwanam );
    }

    cfl_clos ( tblfp, &ier );
}

static void tbl_usst ( shp_record *shprec, int numrec, int *iret )
/************************************************************************
 * tbl_usst                                                             *
 *                                                                      *
 * This function creates the us state and territories table files.	*
 *                                                                      *
 * tbl_usst ( shprec, numrec, iret )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *shprec         shp_record  	Shape record list		*
 *	numrec		int		Total number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          6/04   	Initial coding                  *
 ***********************************************************************/
{
    shp_record *currec;
    /*
     * The array size is from the US State and Territories
     * specification.
     */
    char stabbr[3], stname[25];
    int rec, ifld, len, ier;
    char *tblnam = STATETBL;
    FILE *tblfp;
/*---------------------------------------------------------------------*/
    *iret = 0;

    tblfp = cfl_wopn ( tblnam, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", tblnam );
        exit ( -1 );
    }

    for ( currec = shprec, rec = 0; rec < numrec;
        rec++, currec = currec->nxtrec ) {

        for ( ifld = 0; ifld < currec->numfld; ifld++ ) {
            if ( strncmp ( currec->fields[ifld].name, "STATE",
	        strlen("STATE") ) == 0 ) {
		strncpy ( stabbr, currec->fields[ifld].data, 2 );
		stabbr[2] = '\0';
            } else if ( strncmp ( currec->fields[ifld].name, "NAME",
	        strlen("NAME") ) == 0 ) {
		strcpy ( stname, currec->fields[ifld].data );
		cst_lstr ( stname, &len, &ier );
		stname[len] = '\0';
	    }
	}

        fprintf ( tblfp, "%2.2s %s\n", stabbr, stname );
    }

    cfl_clos ( tblfp, &ier );
}

static void tbl_npsa ( shp_record *shprec, int numrec, int *iret )
/************************************************************************
 * tbl_npsa                                                             *
 *                                                                      *
 * This function creates the Predictive Service Areas table files.	*
 *                                                                      *
 * tbl_npsa ( shprec, numrec, iret )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *shprec         shp_record  	Shape record list		*
 *	numrec		int		Total number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	 4/10   	Initial coding                  *
 * X. Guo/CWS            4/12           Increase variables(code,shrnam) *
 *                                      size                            *
 ***********************************************************************/
{
    shp_record *currec;
    /*
     * The array size is from the specification.
     */
    char code[20], name[81], shrnam[52];
    int rec, ifld, len, ier;
    float clat, clon;
    int iclat, iclon;
    char *tblnam = NPSATBL;
    FILE *tblfp;
/*---------------------------------------------------------------------*/
    *iret = 0;

    tblfp = cfl_wopn ( tblnam, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", tblnam );
        exit ( -1 );
    }

    for ( currec = shprec, rec = 0; rec < numrec;
        rec++, currec = currec->nxtrec ) {

        for ( ifld = 0; ifld < currec->numfld; ifld++ ) {
            if ( strncmp ( currec->fields[ifld].name, "NAT_CODE",
	        strlen("NAT_CODE") ) == 0 ) {
		strcpy ( code, currec->fields[ifld].data );
		cst_lstr ( code, &len, &ier );
		code[len] = '\0';
            } else if ( strncmp ( currec->fields[ifld].name, "PSA_NAME",
	        strlen("PSA_NAME") ) == 0 ) {
		strcpy ( name, currec->fields[ifld].data );
		cst_lstr ( name, &len, &ier );
		name[len] = '\0';
		cst_rspc ( name, &ier );
		abbreviate ( name, 50, shrnam, &ier );
	    }
	}

	if ( strlen(shrnam) == 0 ) {
	    strcpy ( shrnam, code );
	}

	shp_gctr ( currec, &clon, &clat, &ier );
        iclat = (int)(ROUNDUP(clat)*100.);
        iclon = (int)(ROUNDUP(clon)*100.);

        fprintf ( tblfp, 
          "%-8.8s %6.6d %-32.32s -- US %5d %6d %5d %2d\n",
          code, 999999, shrnam, iclat, iclon, 0, 0 );
    }

    cfl_clos ( tblfp, &ier );
}
