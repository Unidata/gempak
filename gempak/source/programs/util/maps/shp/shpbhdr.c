#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

/*
 * private function
 */
static void bnd_cnty ( shp_record *onerec, char *header, char *subhdr,
                 char *hdrnam, float *cenlat, float *cenlon, int *iret );
static void bnd_mari ( shp_record *onerec, char *header, char *subhdr,
                 char *hdrnam, float *cenlat, float *cenlon, int *iret );
static void bnd_mzcn ( shp_record *onerec, char *header, char *subhdr,
                 char *hdrnam, float *cenlat, float *cenlon, int *iret );
static void bnd_zone ( shp_record *onerec, char *header, char *subhdr,
                 char *hdrnam, float *cenlat, float *cenlon, int *iret );
static void bnd_iedg ( shp_record *onerec, char *header, char *subhdr,
                 char *hdrnam, float *cenlat, float *cenlon, int *iret );
static void bnd_rfcr ( shp_record *onerec, char *header, char *subhdr,
                 char *hdrnam, float *cenlat, float *cenlon, int *iret );
static void bnd_cwab ( shp_record *onerec, char *header, char *subhdr,
                 char *hdrnam, float *cenlat, float *cenlon, int *iret );
static void bnd_tmzn ( shp_record *onerec, char *header, char *subhdr,
                 char *hdrnam, float *cenlat, float *cenlon, int *iret );
static void bnd_usst ( shp_record *onerec, char *header, char *subhdr,
                 char *hdrnam, float *cenlat, float *cenlon, int *iret );
static void bnd_npsa ( shp_record *onerec, char *header, char *subhdr,
                 char *hdrnam, float *cenlat, float *cenlon, int *iret );

                                                                                
/************************************************************************
 * shp_bhdr.c                                                           *
 *                                                                      *
 * This module generates bound header and sub-header lines.             *
 *                                                                      *
 * CONTENTS:                                                            *
 *  shp_bhdr()                  generate bound header and sub-header    *
 *  bnd_cnty()			generate county bound header/sub-header	*
 *  bnd_mari()			generate marine bound header/sub-header	*
 *  bnd_mzcn()			generate marine/county header/sub-header*
 *  bnd_zone()			generate zone header/sub-header		*
 *  bnd_iedg()			generate ice edge header/sub-header	*
 *  bnd_rfcr()			generate rfc header/sub-header		*
 *  bnd_cwab()			generate cwa header/sub-header		*
 *  bnd_tmzn()			generate time zone header/sub-header	*
 *  bnd_usst()			generate us state header/sub-header	*
 ***********************************************************************/
                                                                                
/*=====================================================================*/
                                                                                
void shp_bhdr ( shp_record *onerec, char *header, char *subhdr, 
                char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * shp_bhdr                                                             *
 *                                                                      *
 * This function generates the bound header and sub-header lines.       *
 *                                                                      *
 * shp_bhdr ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret ) 	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record  	One shape record		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 * R. Tian/SAIC          2/05           Added MAP_FIRE/RECR/CWAB        *
 * R. Tian/SAIC          6/05           Added MAP_TMZN/USST		*
 * S. Jacobs/NCEP	 4/10		Added MAP_NPSA			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    switch ( maptyp ) {
        case MAP_CNTY:
            /*
             * County
             */
            bnd_cnty ( onerec, header, subhdr, hdrnam,
                       cenlat, cenlon, iret );
        break;
                                                                                
        case MAP_MARI:
            /*
             * Marine
             */
            bnd_mari ( onerec, header, subhdr, hdrnam,
                       cenlat, cenlon, iret );
        break;

        case MAP_MZCN:
            /*
             * Combined marine-county.
             */
            bnd_mzcn ( onerec, header, subhdr, hdrnam,
                       cenlat, cenlon, iret );
        break;

        case MAP_ZONE:
        case MAP_FIRE:
            /*
             * Public Forecast Zones and Fire Weather Zone.
             */
            bnd_zone ( onerec, header, subhdr, hdrnam,
                       cenlat, cenlon, iret );
        break;

        case MAP_IEDG:
            /*
             * Ice edge
             */
            bnd_iedg ( onerec, header, subhdr, hdrnam,
                       cenlat, cenlon, iret );
        break;

        case MAP_RFCR:
            /*
             * River Forecast Center Regions.
             */
            bnd_rfcr ( onerec, header, subhdr, hdrnam,
                       cenlat, cenlon, iret );
        break;

        case MAP_CWAB:
            /*
             * County Warning Area.
             */
            bnd_cwab ( onerec, header, subhdr, hdrnam,
                       cenlat, cenlon, iret );
        break;

        case MAP_TMZN:
            /*
             * Time Zone.
             */
            bnd_tmzn ( onerec, header, subhdr, hdrnam,
                       cenlat, cenlon, iret );
        break;

        case MAP_USST:
            /*
             * US State and Territories.
             */
            bnd_usst ( onerec, header, subhdr, hdrnam,
                       cenlat, cenlon, iret );
        break;

        case MAP_NPSA:
            /*
             * Predictive Service Areas
             */
            bnd_npsa ( onerec, header, subhdr, hdrnam,
                       cenlat, cenlon, iret );
        break;
    }
}

static void bnd_cnty ( shp_record *onerec, char *header, char *subhdr,
                  char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * bnd_cnty                                                             *
 *                                                                      *
 * This function generates the county bound header and sub-header lines *
 *                                                                      *
 * bnd_cnty ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record      One shape record               	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                        1 = Incomplete record         *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 ***********************************************************************/
{
    char ctynam[25], cwanam[10], fearea[3], tmzone[3], stabbr[3];
    int ifips, iclat, iclon, nparts;
    float shplat, shplon;
    int ifld, pos, len, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    nparts = onerec->numprt;
    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
        if ( strncmp ( onerec->fields[ifld].name, "COUNTYNAME", 
	    strlen("COUNTYNAME") ) == 0 ) {
	    strcpy ( ctynam, onerec->fields[ifld].data );
	    cst_lstr ( ctynam, &len, &ier );
	    ctynam[len] = '\0';
            cst_rspc ( ctynam, &ier );

            /*
             * Remove "Lower_Keys_in_" from county name
             */
            cst_rmst ( ctynam, "Lower_Keys_in_", &pos, ctynam, &ier );
                                                                                
            /*
             * Remove "Outer_Banks_" from county name
             */
            cst_rmst ( ctynam, "Outer_Banks_", &pos, ctynam, &ier );
                                                                                
            /*
             * Remove "Mainland_" from county name
             */
            cst_rmst ( ctynam, "Mainland_", &pos, ctynam, &ier );

	} else if ( strncmp ( onerec->fields[ifld].name, "FIPS", 
            strlen("FIPS") ) == 0 ) {
	    ifips = atoi ( onerec->fields[ifld].data );
        } else if ( strncmp ( onerec->fields[ifld].name, "CWA",
	    strlen ( "CWA" ) ) == 0 ) {
	    strcpy ( cwanam, onerec->fields[ifld].data );
	    cst_lstr ( cwanam, &len, &ier );
	    cwanam[len] = '\0';
        } else if ( strncmp ( onerec->fields[ifld].name, "FE_AREA",
	    strlen ( "FE_AREA" ) ) == 0 ) {
	    strcpy ( fearea, onerec->fields[ifld].data );
        } else if ( strncmp ( onerec->fields[ifld].name, "TIME_ZONE",
	    strlen ( "TIME_ZONE" ) ) == 0 ) {
	    strcpy ( tmzone, onerec->fields[ifld].data );
        } else if ( strncmp ( onerec->fields[ifld].name, "STATE",
	    strlen ( "STATE" ) ) == 0 ) {
	    strcpy ( stabbr, onerec->fields[ifld].data );
        } else if ( strncmp ( onerec->fields[ifld].name, "LAT",
            strlen("LAT") ) == 0 ) {
            shplat = atof ( onerec->fields[ifld].data );
        } else if ( strncmp ( onerec->fields[ifld].name, "LON",
            strlen("LON") ) == 0 ) {
            shplon = atof ( onerec->fields[ifld].data );
        }
    }

    if ( strlen ( ctynam ) == (size_t)0 || strlen ( stabbr ) == (size_t)0 ) {
        *iret = 1;
        return;
    }
	    
    if ( ERMISS ( onerec->cenlat ) || ERMISS ( onerec->cenlon ) ) {
        iclat = (int)(ROUNDUP(shplat)*100.);
        iclon = (int)(ROUNDUP(shplon)*100.);
        *cenlat = shplat;
        *cenlon = shplon;
    } else {
        iclat = (int)(ROUNDUP(onerec->cenlat)*100.);
        iclon = (int)(ROUNDUP(onerec->cenlon)*100.);
        *cenlat = onerec->cenlat;
        *cenlon = onerec->cenlon;
    }

    sprintf ( header, "B%5.5d %-32.32s %5d %6d %3d %s",
              ifips, ctynam, iclat, iclon, nparts, cwanam );
    sprintf ( subhdr, "<FIPS>%d<STATE>%s<TIME_ZONE>%s<FE_AREA>%s",
              ifips, stabbr, tmzone, fearea );
    sprintf ( hdrnam, "%s", ctynam );
}

static void bnd_mari ( shp_record *onerec, char *header, char *subhdr,
                  char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * bnd_mari                                                             *
 *                                                                      *
 * This function generates the marine bound header and sub-header lines *
 *                                                                      *
 * bnd_mari ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record      One shape record               	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                        1 = Incomplete record         *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 ***********************************************************************/
{
    char mrznam[4], stabbr[3], wfonam[4], fearea[3];
    int ifips, izone, istno, iclat, iclon, nparts;
    float shplat, shplon;
    int i;
/*---------------------------------------------------------------------*/
    *iret = 0;

    nparts = onerec->numprt;
    for ( i = 0; i < onerec->numfld; i++ ) {
        if ( strncmp ( onerec->fields[i].name, "ID", 
	    strlen("ID") ) == 0 ) {
	    strncpy ( mrznam, onerec->fields[i].data, 3 );
	    strncpy ( stabbr, onerec->fields[i].data, 2 );
	    mrznam[3] = '\0';
	    stabbr[2] = '\0';
	    izone = atoi ( onerec->fields[i].data + 3 );
	    for ( istno = 0; istno < NUMSTNO; istno++ ) {
	        if ( strncmp ( stateno[istno], stabbr, 2 ) == 0 ) {
		    break;
		}
	    }
	} else if ( strncmp ( onerec->fields[i].name, "LAT", 
            strlen("LAT") ) == 0 ) {
	    shplat = atof ( onerec->fields[i].data );
        } else if ( strncmp ( onerec->fields[i].name, "LON",
	    strlen ( "LON" ) ) == 0 ) {
	    shplon = atof ( onerec->fields[i].data );
        } else if ( strncmp ( onerec->fields[i].name, "WFO",
	    strlen ( "WFO" ) ) == 0 ) {
	    strncpy ( wfonam, onerec->fields[i].data, 3 );
	    wfonam[3] = '\0';
        }
    }

    if ( ERMISS ( onerec->cenlat ) || ERMISS ( onerec->cenlon ) ) {
        iclat = (int)(ROUNDUP(shplat)*100.);
        iclon = (int)(ROUNDUP(shplon)*100.);
        *cenlat = shplat;
        *cenlon = shplon;
    } else {
        iclat = (int)(ROUNDUP(onerec->cenlat)*100.);
        iclon = (int)(ROUNDUP(onerec->cenlon)*100.);
        *cenlat = onerec->cenlat;
        *cenlon = onerec->cenlon;
    }
    ifips = istno * 10000 + izone * 10;
    fearea[0] = '\0';

    sprintf ( header, "B%5.5d %-32.32s %5d %6d %3d %s",
              izone, mrznam, iclat, iclon, nparts, mrznam );
    sprintf ( subhdr, "<FIPS>%d<WFO>%s<ID>%s<FE_AREA>%s",
              ifips, wfonam, mrznam, fearea );
    sprintf ( hdrnam, "%s", mrznam );
}

static void bnd_mzcn ( shp_record *onerec, char *header, char *subhdr,
                  char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * bnd_mzcn                                                             *
 *                                                                      *
 * This function generates the marine bound header and sub-header lines *
 *                                                                      *
 * bnd_mzcn ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record      One shape record               	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                        1 = Incomplete record         *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 ***********************************************************************/
{
    char ctynam[25], cwanam[10], stabbr[3];
    int ifips, izone, istno, iclat, iclon, nparts;
    float shplat, shplon;
    int i, pos, len, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    nparts = onerec->numprt;
    for ( i = 0; i < onerec->numfld; i++ ) {
        if ( strncmp ( onerec->fields[i].name, "COUNTYNAME", 
	    strlen("COUNTYNAME") ) == 0 ) {
	    strcpy ( ctynam, onerec->fields[i].data );
	    cst_lstr ( ctynam, &len, &ier );
	    ctynam[len] = '\0';
            cst_rspc ( ctynam, &ier );

            /*
             * Remove "Lower_Keys_in_" from county name
             */
            cst_rmst ( ctynam, "Lower_Keys_in_", &pos, ctynam, &ier );
                                                                                
            /*
             * Remove "Outer_Banks_" from county name
             */
            cst_rmst ( ctynam, "Outer_Banks_", &pos, ctynam, &ier );
                                                                                
            /*
             * Remove "Mainland_" from county name
             */
            cst_rmst ( ctynam, "Mainland_", &pos, ctynam, &ier );

	} else if ( strncmp ( onerec->fields[i].name, "FIPS", 
            strlen("FIPS") ) == 0 ) {
	    izone = ifips = atoi ( onerec->fields[i].data );
        } else if ( strncmp ( onerec->fields[i].name, "CWA",
	    strlen ( "CWA" ) ) == 0 ) {
	    strcpy ( cwanam, onerec->fields[i].data );
	    cst_lstr ( cwanam, &len, &ier );
	    cwanam[len] = '\0';
        } else if ( strncmp ( onerec->fields[i].name, "ID", 
	    strlen("ID") ) == 0 ) {
	    strcpy ( ctynam, onerec->fields[i].data );
	    strncpy ( stabbr, onerec->fields[i].data, 2 );
	    stabbr[2] = '\0';
	    izone = atoi ( onerec->fields[i].data + 3 );
	    for ( istno = 0; istno < NUMSTNO; istno++ ) {
	        if ( strncmp ( stateno[istno], stabbr, 2 ) == 0 ) {
		    break;
		}
	    }
            ifips = istno * 10000 + izone * 10;
        } else if ( strncmp ( onerec->fields[i].name, "WFO",
	    strlen ( "WFO" ) ) == 0 ) {
	    strcpy ( cwanam, onerec->fields[i].data );
        } else if ( strncmp ( onerec->fields[i].name, "LAT",
            strlen("LAT") ) == 0 ) {
            shplat = atof ( onerec->fields[i].data );
        } else if ( strncmp ( onerec->fields[i].name, "LON",
            strlen("LON") ) == 0 ) {
            shplon = atof ( onerec->fields[i].data );
        }
    }

    if ( ERMISS ( onerec->cenlat ) || ERMISS ( onerec->cenlon ) ) {
        iclat = (int)(ROUNDUP(shplat)*100.);
        iclon = (int)(ROUNDUP(shplon)*100.);
        *cenlat = shplat;
        *cenlon = shplon;
    } else {
        iclat = (int)(ROUNDUP(onerec->cenlat)*100.);
        iclon = (int)(ROUNDUP(onerec->cenlon)*100.);
        *cenlat = onerec->cenlat;
        *cenlon = onerec->cenlon;
    }

    sprintf ( header, "B%5.5d %-32.32s %5d %6d %3d %s",
              izone, ctynam, iclat, iclon, nparts, cwanam );
    sprintf ( subhdr, "<FIPS>%d", ifips );
    sprintf ( hdrnam, "%s", ctynam );
}

static void bnd_zone ( shp_record *onerec, char *header, char *subhdr,
                  char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * bnd_zone                                                             *
 *                                                                      *
 * This function generates the public forecast zone and fire weather	*
 * zone bound header and sub-header lines.				*
 *                                                                      *
 * bnd_zone ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record      One shape record               	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                        1 = Incomplete record         *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 ***********************************************************************/
{
    char stabbr[3], cwanam[4];
    int ifips, izone, istno, iclat, iclon, nparts;
    float shplat, shplon;
    int i;
/*---------------------------------------------------------------------*/
    *iret = 0;

    nparts = onerec->numprt;
    for ( i = 0; i < onerec->numfld; i++ ) {
        if ( strcmp ( onerec->fields[i].name, "STATE" ) == 0 ) {
	    strcpy ( stabbr, onerec->fields[i].data );
	} else if ( strncmp ( onerec->fields[i].name, "ZONE",
	    strlen("ZONE") ) == 0 ) {
	    izone = strtol ( onerec->fields[i].data, NULL, 10 );
        } else if ( strncmp ( onerec->fields[i].name, "CWA",
	    strlen ( "CWA" ) ) == 0 ) {
	    strncpy ( cwanam, onerec->fields[i].data, 3 );
	    cwanam[3] = '\0';
	} else if ( strncmp ( onerec->fields[i].name, "LAT", 
            strlen("LAT") ) == 0 ) {
	    shplat = atof ( onerec->fields[i].data );
        } else if ( strncmp ( onerec->fields[i].name, "LON",
	    strlen ( "LON" ) ) == 0 ) {
	    shplon = atof ( onerec->fields[i].data );
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
        iclat = (int)(ROUNDUP(onerec->cenlat)*100.);
        iclon = (int)(ROUNDUP(onerec->cenlon)*100.);
        *cenlat = onerec->cenlat;
        *cenlon = onerec->cenlon;
    } else {
        *cenlat = shplat;
        *cenlon = shplon;
    }

    sprintf ( header, "B%5.5d %-32.32s %5d %6d %3d %s",
              izone, cwanam, iclat, iclon, nparts, cwanam );
    sprintf ( subhdr, "<FIPS>%d<STATE>%s<WFO>%s",
              ifips, stabbr, cwanam );
    sprintf ( hdrnam, "%s", cwanam );
}

static void bnd_iedg ( shp_record *onerec, char *header, char *subhdr,
                  char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * bnd_iedg                                                             *
 *                                                                      *
 * This function generates the ice edge bound header and sub-header	*
 * lines 								*
 *                                                                      *
 * bnd_iedg ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record      One shape record               	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                        1 = Incomplete record         *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 ***********************************************************************/
{
    /*
     * The array size is from the Ice Edge attributes specification.
     */
    char icecod[120], icenam[15];
    int iclat, iclon, nparts, ifld, len, ier;
    static int seqnum = 0;
/*---------------------------------------------------------------------*/
    *iret = 0;

    seqnum++;
    nparts = onerec->numprt;
    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
        if ( strncmp ( onerec->fields[ifld].name, "ICECODE",
	    strlen("ICECODE") ) == 0 ) {
	    strcpy ( icecod, onerec->fields[ifld].data );
	    cst_lstr ( icecod, &len, &ier );
	    icecod[len] = '\0';
        }
    }

    sprintf ( icenam, "ICE_EDGE_%5.5d", seqnum );
    iclat = (int)(ROUNDUP(onerec->cenlat)*100.);
    iclon = (int)(ROUNDUP(onerec->cenlon)*100.);
    *cenlat = onerec->cenlat;
    *cenlon = onerec->cenlon;

    sprintf ( header, "B%5.5d %-32.32s %5d %6d %3d %s",
              seqnum, icenam, iclat, iclon, nparts, icenam );
    sprintf ( subhdr, "<NUM>%d<ICECODE>%s", seqnum, icecod );
    sprintf ( hdrnam, "%s", icenam );
}

static void bnd_rfcr ( shp_record *onerec, char *header, char *subhdr,
                  char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * bnd_rfcr                                                             *
 *                                                                      *
 * This function generates the river forecast center regions bound	*
 * header and sub-header lines.						*
 *                                                                      *
 * bnd_rfcr ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record      One shape record               	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                        1 = Incomplete record         *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          2/05           Initial coding                  *
 ***********************************************************************/
{
    /*
     * The array size is from the river forecast center regions
     * attributes specification.
     */
    char basnid[6], rfcnam[19];
    int iclat, iclon, nparts, ifld, len, ier;
    static int seqnum = 0;
/*---------------------------------------------------------------------*/
    *iret = 0;

    seqnum++;
    nparts = onerec->numprt;
    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
        if ( strncmp ( onerec->fields[ifld].name, "BASIN_ID",
	    strlen("BASIN_ID") ) == 0 ) {
	    strncpy ( basnid, onerec->fields[ifld].data, 5 );
	    basnid[5] = '\0';
	    cst_lstr ( basnid, &len, &ier );
	    basnid[len] = '\0';
        } else if ( strncmp ( onerec->fields[ifld].name, "RFC_NAME",
	    strlen("RFC_NAME") ) == 0 ) {
	    strncpy ( rfcnam, onerec->fields[ifld].data, 18 );
	    rfcnam[18] = '\0';
	    cst_lstr ( rfcnam, &len, &ier );
	    rfcnam[len] = '\0';
	    cst_rspc ( rfcnam, &ier );
        }
    }

    iclat = (int)(ROUNDUP(onerec->cenlat)*100.);
    iclon = (int)(ROUNDUP(onerec->cenlon)*100.);
    *cenlat = onerec->cenlat;
    *cenlon = onerec->cenlon;

    sprintf ( header, "B%5.5d %-32.32s %5d %6d %3d %s",
              seqnum, basnid, iclat, iclon, nparts, basnid );
    sprintf ( subhdr, "<RFC>%s<RFC_NAME>%s", basnid, rfcnam );
    sprintf ( hdrnam, "%s", basnid );
}

static void bnd_cwab ( shp_record *onerec, char *header, char *subhdr,
                  char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * bnd_cwab                                                             *
 *                                                                      *
 * This function generates the county warning area boundaries bound	*
 * header and sub-header lines.						*
 *                                                                      *
 * bnd_cwab ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record      One shape record               	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                        1 = Incomplete record         *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          2/05           Initial coding                  *
 ***********************************************************************/
{
    /*
     * The array size is from the county warning area boundaries
     * attributes specification.
     */
    char wfonam[4];
    int iclat, iclon, nparts, ifld;
    static int seqnum = 0;
/*---------------------------------------------------------------------*/
    *iret = 0;

    seqnum++;
    nparts = onerec->numprt;
    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
        if ( strncmp ( onerec->fields[ifld].name, "WFO",
	    strlen("WFO") ) == 0 ) {
	    strncpy ( wfonam, onerec->fields[ifld].data, 3 );
	    wfonam[3] = '\0';
        }
    }

    iclat = (int)(ROUNDUP(onerec->cenlat)*100.);
    iclon = (int)(ROUNDUP(onerec->cenlon)*100.);
    *cenlat = onerec->cenlat;
    *cenlon = onerec->cenlon;

    sprintf ( header, "B%5.5d %-32.32s %5d %6d %3d %s",
              seqnum, wfonam, iclat, iclon, nparts, wfonam );
    sprintf ( subhdr, "<WFO>%s", wfonam );
    sprintf ( hdrnam, "%s", wfonam );
}

static void bnd_tmzn ( shp_record *onerec, char *header, char *subhdr,
                  char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * bnd_tmzn                                                             *
 *                                                                      *
 * This function generates the time zone boundaries bound header and	*
 * sub-header lines.							*
 *                                                                      *
 * bnd_tmzn ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record      One shape record               	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                        1 = Incomplete record         *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          6/05           Initial coding                  *
 ***********************************************************************/
{
    char tzname[80], symbol[80], gmt_offset[80], gmt_dst_offset[80];
    int iclat, iclon, nparts, ifld;
    int len, ier;
    static int seqnum = 0;
/*---------------------------------------------------------------------*/
    *iret = 0;

    seqnum++;
    nparts = onerec->numprt;
    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
        if ( strncmp ( onerec->fields[ifld].name, "TIMEZONE",
	    strlen("TIMEZONE") ) == 0 ) {
	    strcpy ( tzname, onerec->fields[ifld].data );
	    cst_rmbl ( tzname, tzname, &len, &ier );
        } else if ( strncmp ( onerec->fields[ifld].name, "SYMBOL",
	    strlen("SYMBOL") ) == 0 ) {
	    strcpy ( symbol, onerec->fields[ifld].data );
	    cst_rmbl ( symbol, symbol, &len, &ier );
        } else if ( strncmp ( onerec->fields[ifld].name, "GMT_OFFSET",
	    strlen("GMT_OFFSET") ) == 0 ) {
	    strcpy ( gmt_offset, onerec->fields[ifld].data );
	    cst_rmbl ( gmt_offset, gmt_offset, &len, &ier );
	} else if ( strncmp ( onerec->fields[ifld].name, "GMT_DST_OF",
	    strlen("GMT_DST_OF") ) == 0 ) {
	    strcpy ( gmt_dst_offset, onerec->fields[ifld].data );
	    cst_rmbl ( gmt_dst_offset, gmt_dst_offset, &len, &ier );
	}
    }

    iclat = (int)(ROUNDUP(onerec->cenlat)*100.);
    iclon = (int)(ROUNDUP(onerec->cenlon)*100.);
    *cenlat = onerec->cenlat;
    *cenlon = onerec->cenlon;

    sprintf ( header, "B%5.5d %-32.32s %5d %6d %3d %s",
              seqnum, tzname, iclat, iclon, nparts, symbol );
    sprintf ( subhdr, "<TIME_ZONE>%s<GMT_OFFSET>%s<GMT_DST_OFFSET>%s",
              tzname, gmt_offset, gmt_dst_offset );
    sprintf ( hdrnam, "%s", tzname );
}

static void bnd_usst ( shp_record *onerec, char *header, char *subhdr,
                  char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * bnd_usst                                                             *
 *                                                                      *
 * This function generates the us state boundaries bound header and	*
 * sub-header lines.							*
 *                                                                      *
 * bnd_usst ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record      One shape record               	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                        1 = Incomplete record         *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          6/05           Initial coding                  *
 ***********************************************************************/
{
    /*
     * The array size is from the us state and territories
     * attributes specification.
     */
    char stabbr[3], stname[25];
    int ifips, iclat, iclon, nparts, ifld;
    float shplat, shplon;
    int len, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    nparts = onerec->numprt;
    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
        if ( strncmp ( onerec->fields[ifld].name, "STATE",
	    strlen("STATE") ) == 0 ) {
	    strncpy ( stabbr, onerec->fields[ifld].data, 2 );
	    stabbr[2] = '\0';
        } else if ( strncmp ( onerec->fields[ifld].name, "NAME",
	    strlen("NAME") ) == 0 ) {
	    strcpy ( stname, onerec->fields[ifld].data );
	    cst_lstr ( stname, &len, &ier );
	    stname[len] = '\0';
            cst_rspc ( stname, &ier );
        } else if ( strncmp ( onerec->fields[ifld].name, "FIPS",
	    strlen("FIPS") ) == 0 ) {
	    ifips = atoi ( onerec->fields[ifld].data );
        } else if ( strncmp ( onerec->fields[ifld].name, "LAT",
            strlen("LAT") ) == 0 ) {
            shplat = atof ( onerec->fields[ifld].data );
        } else if ( strncmp ( onerec->fields[ifld].name, "LON",
            strlen("LON") ) == 0 ) {
            shplon = atof ( onerec->fields[ifld].data );
        }
    }

    if ( ERMISS ( onerec->cenlat ) || ERMISS ( onerec->cenlon ) ) {
        iclat = (int)(ROUNDUP(shplat)*100.);
        iclon = (int)(ROUNDUP(shplon)*100.);
        *cenlat = shplat;
        *cenlon = shplon;
    } else {
        iclat = (int)(ROUNDUP(onerec->cenlat)*100.);
        iclon = (int)(ROUNDUP(onerec->cenlon)*100.);
        *cenlat = onerec->cenlat;
        *cenlon = onerec->cenlon;
    }

    sprintf ( header, "B%5.5d %-32.32s %5d %6d %3d",
              ifips, stname, iclat, iclon, nparts );
    sprintf ( subhdr, "<FIPS>%d<STATE>%s", ifips, stabbr );
    sprintf ( hdrnam, "%s", stname );
}

static void bnd_npsa ( shp_record *onerec, char *header, char *subhdr,
                  char *hdrnam, float *cenlat, float *cenlon, int *iret )
/************************************************************************
 * bnd_npsa                                                             *
 *                                                                      *
 * This function generates the predictive service areas boundaries	*
 * bound header and sub-header lines.					*
 *                                                                      *
 * bnd_npsa ( onerec, header, subhdr, hdrnam, cenlat, cenlon, iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record      One shape record               	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*header		char		Bound header line		*
 *	*subhdr		char		Bound sub-header line		*
 *	*hdrnam		char		Bound header name    		*
 *	*cenlat		float		Center latitude			*
 *	*cenlon		float 		Center longitude		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                        1 = Incomplete record         *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP          4/10           Initial coding                *
 * X. Guo/CWS              4/12           Increase variables(code) size *
 ***********************************************************************/
{
    /*
     * The array size is from the us state and territories
     * attributes specification.
     */
    char code[20], name[81], gacc[81];
    int iclat, iclon, nparts, ifld;
    float shplat, shplon;
    int len, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    nparts = onerec->numprt;
    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
        if ( strncmp ( onerec->fields[ifld].name, "NAT_CODE",
	    strlen("NAT_CODE") ) == 0 ) {
	    strcpy ( code, onerec->fields[ifld].data );
	    cst_lstr ( code, &len, &ier );
	    code[len] = '\0';
        } else if ( strncmp ( onerec->fields[ifld].name, "PSA_NAME",
	    strlen("PSA_NAME") ) == 0 ) {
	    strcpy ( name, onerec->fields[ifld].data );
	    cst_lstr ( name, &len, &ier );
	    name[len] = '\0';
            cst_rspc ( name, &ier );
        } else if ( strncmp ( onerec->fields[ifld].name, "GACC",
	    strlen("GACC") ) == 0 ) {
	    strcpy ( gacc, onerec->fields[ifld].data );
	    cst_lstr ( gacc, &len, &ier );
	    gacc[len] = '\0';
            cst_rspc ( gacc, &ier );
        }
    }

    shp_gctr ( onerec, &shplon, &shplat, &ier );
    iclat = (int)(ROUNDUP(shplat)*100.);
    iclon = (int)(ROUNDUP(shplon)*100.);
    *cenlat = shplat;
    *cenlon = shplon;

    sprintf ( header, "B%5.5d %-32.32s %5d %6d %3d",
              0, name, iclat, iclon, nparts );
    sprintf ( subhdr, "<AREA>%s<NAT_CODE>%s<GACC>%s", name, code, gacc );
    sprintf ( hdrnam, "%s", name );
}
