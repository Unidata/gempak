#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

/*
 * private function
 */
static void getname ( char *bndnam, char *infnam, int *iret);

/************************************************************************
 * shp_cbnd.c                                                           *
 *                                                                      *
 * This module contains functions to create bound and bound info.       *
 *                                                                      *
 * CONTENTS:                                                            *
 *  getname()			determine bound and bound info names	*
 *  shp_cbnd()                  create bound and bound info             *
 ***********************************************************************/
                                                                                
/*=====================================================================*/

void shp_cbnd ( shp_record *shprec, int numrec, int *iret )
/************************************************************************
 * shp_cbnd                                                             *
 *                                                                      *
 * This function creates the bound and bound info files.                *
 *                                                                      *
 * shp_cbnd ( shprec, numrec, iret )                            	*
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
    shp_part *curprt;
    char bndnam[LLPATH], infnam[LLPATH];
    char header[80], subhdr[80], hdrnam[80];
    float cenlat, cenlon, maxlat, minlat, maxlon, minlon;
    float rmaxlat, rminlat, rmaxlon, rminlon;
    FILE *bndfp, *inffp;
    int tltbnd, maxpts;
    long lpos;
    int irec, jprt, kpts, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    getname ( bndnam, infnam, &ier );
    if ( ier != 0 ) {
        *iret = -1;
	return;
    }

    bndfp = cfl_wopn ( bndnam, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", bndnam );
	exit ( -1 );
    }
    inffp = cfl_wopn ( infnam, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", infnam );
	exit ( -1 );
    }

    /*
     * Compute min/max.
     */
    tltbnd = 0;
    maxpts = 0;
    for ( currec = shprec, irec = 0; irec < numrec;
        irec++, currec = currec->nxtrec ) {

	tltbnd++;
        rmaxlat = -90.0F;
        rminlat = 90.0F;
        rmaxlon = -180.0F;
        rminlon = 180.0F;

	for ( curprt = currec->shpart, jprt = 0; jprt < currec->numprt;
	    jprt++, curprt = curprt->nxtprt ) {

	    maxpts = curprt->numpts > maxpts ? curprt->numpts : maxpts;
            maxlat = -90.0F;
            minlat = 90.0F;
            maxlon = -180.0F;
            minlon = 180.0F;
            for ( kpts = 0; kpts < curprt->numpts; kpts++ ) {
                maxlon = curprt->ptx[kpts] > maxlon ? 
		    curprt->ptx[kpts] : maxlon;
                minlon = curprt->ptx[kpts] < minlon ? 
		    curprt->ptx[kpts] : minlon;
                maxlat = curprt->pty[kpts] > maxlat ? 
		    curprt->pty[kpts] : maxlat;
                minlat = curprt->pty[kpts] < minlat ? 
		    curprt->pty[kpts] : minlat;
            }
	    curprt->maxlat = maxlat;
	    curprt->minlat = minlat;
	    curprt->maxlon = maxlon;
	    curprt->minlon = minlon;

	    rmaxlat = curprt->maxlat > rmaxlat ? curprt->maxlat : rmaxlat;
	    rminlat = curprt->minlat < rminlat ? curprt->minlat : rminlat;
	    rmaxlon = curprt->maxlon > rmaxlon ? curprt->maxlon : rmaxlon;
	    rminlon = curprt->minlon < rminlon ? curprt->minlon : rminlon;
        }
	currec->maxlat = rmaxlat;
	currec->minlat = rminlat;
	currec->maxlon = rmaxlon;
	currec->minlon = rminlon;
    }

    /*
     * Write the bound and bound info files.
     */
    fprintf ( inffp, "!\n!    BOUNDARIES FILENAME \n%s\n!\n", bndnam );
    fprintf ( inffp, "!    TOTAL NUMBER OF BOUNDS\n%d\n!\n", tltbnd );
    fprintf ( inffp, "!    MAX NUMBER OF POINTS per BOUND\n%d\n!\n", maxpts );
    fprintf ( inffp, "!    BOUNDARY STRUCTURE INFORMATION\n!\n" );

    for ( currec = shprec, irec = 0; irec < numrec;
        irec++, currec = currec->nxtrec ) {

	/*
	 * Get bound header and sub-header lines.
	 */
        shp_bhdr ( currec, header, subhdr, hdrnam, &cenlat, &cenlon, &ier );
	/*
	 * Write the bound header and sub-header lines for this record.
	 */
	cfl_wher ( bndfp, &lpos, &ier );
	fprintf ( bndfp, "%s\n", header );
	fprintf ( bndfp, "%s\n", subhdr );

        fprintf ( inffp, 
	          "!\n%-s %-12ld %-.2f %-.2f %-.2f %-.2f %-.2f %-.2f %-5d\n",
		  hdrnam, lpos, cenlat, cenlon,
		  currec->minlat, currec->minlon,
		  currec->maxlat, currec->maxlon, currec->numprt );
        fprintf ( inffp, "%s\n", subhdr );

	/*
	 * Write each part of this record.
	 */
	for ( curprt = currec->shpart, jprt = 0; jprt < currec->numprt;
	    jprt++, curprt = curprt->nxtprt ) {

	    cfl_wher ( bndfp, &lpos, &ier );

	    shp_wprt ( bndfp, curprt, &ier );
            fprintf ( inffp, 
	              "\t%-12ld %-.2f %-.2f %-.2f %-.2f %-8d \n",
		      lpos, curprt->minlat, curprt->minlon,
		      curprt->maxlat, curprt->maxlon, curprt->numpts );
        }
    }
    
    cfl_clos ( bndfp, &ier );
    cfl_clos ( inffp, &ier );
}

static void getname ( char *bndnam, char *infnam, int *iret )
/************************************************************************
 * getname                                                              *
 *                                                                      *
 * This function determines the bound and bound info file names.        *
 *                                                                      *
 * getname ( bndnam, infnam, iret )                             	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *	*bndnam		char		Bound file name			*
 *	*infnam		char		Bound info file name		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 * R. Tian/SAIC          2/05           Added MAP_FIRE/RECR/CWAB        *
 * m.gamazaychikov/SAIC  6/08           Added MAP_OFSH/CNPR/UNDF        *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;

    switch ( maptyp ) {
        case MAP_CNTY:
	    /*
	     * County
	     */
	    strcpy ( bndnam, COUNTYBNDS );
	    strcpy ( infnam, COUNTYBNDS_INFO );
	break;

        case MAP_MARI:
	    /*
	     * Marine
	     */
	    strcpy ( bndnam, MZBNDS );
	    strcpy ( infnam, MZBNDS_INFO );
	break;

        case MAP_MZCN:
	    /*
	     * Combined County and Marine
	     */
	    strcpy ( bndnam, MZCNTYBNDS );
	    strcpy ( infnam, MZCNTYBNDS_INFO );
	break;

        case MAP_ZONE:
	    /*
	     * Public Forecast Zones
	     */
	    strcpy ( bndnam, PFZBNDS );
	    strcpy ( infnam, PFZBNDS_INFO );
	break;

        case MAP_IEDG:
	    /*
	     * Ice Edge
	     */
	    strcpy ( bndnam, ICEEDGEBND );
	    strcpy ( infnam, ICEEDGEBND_INFO );
	break;

        case MAP_FIRE:
	    /*
	     * Fire Weather Zone.
	     */
	    strcpy ( bndnam, FIREBND );
	    strcpy ( infnam, FIREBND_INFO );
	break;

        case MAP_RFCR:
	    /*
	     * River Forecast Center Regions.
	     */
	    strcpy ( bndnam, RFCBND );
	    strcpy ( infnam, RFCBND_INFO );
	break;

        case MAP_CWAB:
	    /*
	     * County Warning Area.
	     */
	    strcpy ( bndnam, CWABND );
	    strcpy ( infnam, CWABND_INFO );
	break;

        case MAP_TMZN:
	    /*
	     * Time Zone.
	     */
	    strcpy ( bndnam, TZBNDS );
	    strcpy ( infnam, TZBNDS_INFO );
	break;

        case MAP_USST:
	    /*
	     * US State and Territories.
	     */
	    strcpy ( bndnam, STBNDS );
	    strcpy ( infnam, STBNDS_INFO );
	break;

        case MAP_OFSH:
	    /*
	     * Offshore Marine Zones.
	     */
	    strcpy ( bndnam, OSMZBNDS );
	    strcpy ( infnam, OSMZBNDS_INFO );
	break;

        case MAP_CNPR:
	    /*
	     * Canadian Provinces.
	     */
	    strcpy ( bndnam, CNPRBNDS );
	    strcpy ( infnam, CNPRBNDS_INFO );
	break;

        case MAP_NPSA:
	    /*
	     * Predictive Service Areas
	     */
	    strcpy ( bndnam, NPSABNDS );
	    strcpy ( infnam, NPSABNDS_INFO );
	break;

        case MAP_UNDF:
	    /*
	     * UNDEF map types.
	     */
	    strcpy ( bndnam, UNDFBNDS );
	    strcpy ( infnam, UNDFBNDS_INFO );
	break;

	default:
	    *iret = -1;
	break;
    }
}
