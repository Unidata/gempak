#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

#define  CNAM_LEN  256

extern SpcInfo_t      newinfo;
extern SpcInfo_t      spcinfo;

void vfwsev ( int *iret )
/************************************************************************
 * vfwsev                                                               *
 *                                                                      *
 * This program opens, creates and closes the Weather Watch SEV text    *
 * product file.							*
 *                                                                      *
 * vfwsev ( iret )                                             		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          8/99   Created                                 *
 * A. Hardy/GSC          9/99   Changed truncation method for lats/lons *
 * M. Li/GSC		10/99	Added a '\r' to the end of each line	*
 * A. Hardy/GSC         11/99   Separated ind. cities from counties     *
 * A. Hardy/GSC         11/99   Add 1200 am time check; del. unused var.*
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 * A. Hardy/GSC		 3/00   Added effective time function - vfeftm  *
 * A. Hardy/GSC		 3/00   Added spcinfo.sssnum calculation        *
 * A. Hardy/GSC		 5/00   Added 'PARISHES';cfl_opn -> cfl_wopn:use*
 *				AWIPS/WMO header ids.;removed 'NNNN'    * 
 * A. Hardy/GSC		 6/00   Cleaned up ind. cities storage/display  *
 * A. Hardy/GSC		12/00   Removed '&' from iret			*
 * A. Hardy/GSC          5/01   Cleaned up several fprintf, removed     *
 *				unused variable 'newtm'			*
 * A. Hardy/SAIC	10/01   Added check for old/new WMO header flag;*
 *                              removed '#' from watch number		*
 * R. Tian/SAIC		06/02	Modified to meet the SPC requirement	*
 * R. Tian/SAIC		06/02	Added 'NWS' before 'STORM PRED...'	*
 * S. Jacobs/NCEP	 3/03	Added extra check for num of counties	*
 * G. Grosshans/SPC	 4/03	Removed leading space on county lines   *
 *  				in first column and add "#" back to the *
 *				watch number 				*
 * G. Grosshans/SPC	 5/03	Removed cst_rmst call to remove '.'     *
 *				within county names (e.g. ST. CLAIR will*
 *				now be"ST. CLAIR" instead of "ST CLAIR" *
 * A. Hardy/NCEP	11/03	Modified to use wbc/utl libraried; check*
 *				if watch is 'TEST'			*
 * A. Hardy/NCEP	 1/04   Added VTEC parameters to WBC_DCTY	*
 * A. Hardy/NCEP	 3/04	Added wbc_mzhv and  ctb_mzgnm		*
 * G. Grosshans/SPC	 4/04	Per Schaefer, removed MIDNIGHT & NOON	*
 *				from SEV. WWA redefine can not handle	*
 *				MIDNIGHT/NOON				*
 * A. Hardy/NCEP	 6/04	Commented out addition of marine zones  *
 * A. Hardy/NCEP	 7/04	Check for no explicit mz, but have 'CW'	*
 * H. Zeng/SAIC		06/06	modified calling sequence for utl_ivet	*
 ***********************************************************************/
{
    FILE    *ifpsev;
    char    ifname[256];
    char    iampm[3], vampm[3], eampm[3], chmon[4], chdwk[4], lclzn[4];
    char    **ugc_arr, **cnam_arr, **st_arr, **ind_arr, cntystr[10000];
    char    cday[3], chour[5];
    char    prdcod[2], actn[4], offid[5], phen[3], sigcd[2], etn[5];
    int     ier, inewtm, vnewtm, enewtm, datwk, time;
    int	    itmarr[5], vtime[5], etime[5], iarr[5], earr[5];
    int     varr[5], ii, leng, num, ugcln, len, vtecln, icnt;
    int     imk;
    Boolean hvmz;
/*-------------------------------------------------------------------*/
    *iret = 0;
    ier   = 0;
    vtecln = 0;

   /*
    *  Create output file for appending.
    */
    sprintf ( ifname, "WW%04d.SEV", spcinfo.wnum );
    ifpsev = cfl_wopn ( ifname, &ier );

    spcinfo.sssnum = spcinfo.wnum % 10;

   /*
    * Set up header information lines.
    */
    fprintf ( ifpsev, "WWUS50 KWNS %02d%s\n", spcinfo.itime.day,
              spcinfo.itime.hour);
    fprintf ( ifpsev, "SEV%d\n\n", spcinfo.sssnum);

   /*
    * Getting all of the information to create the the time line string.
    */

    time  = atoi(spcinfo.itime.hour);
    itmarr[0] = spcinfo.itime.year;
    itmarr[1] = spcinfo.itime.month;
    itmarr[2] = spcinfo.itime.day;
    itmarr[3] = time / 100;
    itmarr[4] = time % 100;

    time     = atoi(spcinfo.vtime.hour);
    vtime[0] = spcinfo.vtime.year;
    vtime[1] = spcinfo.vtime.month;
    vtime[2] = spcinfo.vtime.day;
    vtime[3] = time / 100;
    vtime[4] = time % 100;

    time     = atoi(spcinfo.etime.hour);
    etime[0] = spcinfo.etime.year;
    etime[1] = spcinfo.etime.month;
    etime[2] = spcinfo.etime.day;
    etime[3] = time / 100;
    etime[4] = time % 100;

    strcpy ( lclzn, spcinfo.timzone ); 
    utl_ivet ( lclzn, itmarr, vtime, etime, iarr, &inewtm,
               iampm, chmon, chdwk, varr, &vnewtm, vampm,
               earr, &enewtm, eampm, &datwk, &ier );

   /*
    * Set up general time of day string.
    */

    leng = sizeof ( spcinfo.genday );
    utl_gtod ( vnewtm, enewtm, earr[4], vampm, eampm, datwk, leng,
               spcinfo.genday, &ier); 

   /*
    * Check if watch has been issued as a 'TEST'.
    */ 

    if ( strcmp (spcinfo.status, "TEST") == 0 ) {
        fprintf ( ifpsev, ". TEST %s WATCH #%d HAS BEEN ISSUED",
              spcinfo.wtype, spcinfo.wnum );
    }
    else {
        fprintf ( ifpsev, ". %s WATCH #%d HAS BEEN ISSUED",
              spcinfo.wtype, spcinfo.wnum );
    }

    fprintf ( ifpsev, " BY THE NWS STORM PREDICTION CENTER \n\n");

    fprintf ( ifpsev, "EFFECTIVE THIS%s",spcinfo.genday);
        fprintf ( ifpsev, " UNTIL %d%02d %s %s.\n", enewtm, earr[4], eampm, 
	                     spcinfo.timzone );

    fprintf ( ifpsev, "$$ \n\n");

   /*
    * Read the marine zone table if coastal waters was found.
    */

    wbc_mzhv ( spcinfo.states, &hvmz, &ier );

   /*
    * Do not add marine zones into the SEV. Find the last county listing
    * and use that number for the total counties.
    */

    imk = G_FALSE;
    icnt = spcinfo.total;
    if ( hvmz ) {
        ii = 0;
        while (  ( ii < icnt) ) {
 	    if ( ( !imk ) && ( spcinfo.cnty[ii].ugc[2] == 'Z') ) { 
		num = ii;
		imk = G_TRUE;
	    }
	    ii++;
	}
       /* If 'CW' is listed in states string but no marine zones
	* exist, because marine zones are turned off, set the 
	* number of counties to the stored total.
        */
	if ( !imk ) {
            num = spcinfo.total;
	}
    }
    else {
        num = spcinfo.total;
    }

   /*
    * Set up county watch section. 
    */

    ugcln = 0;
    ugc_arr = (char **)malloc(num * sizeof(char *));
    cnam_arr = (char **)malloc(num * sizeof(char *));
    st_arr = (char **)malloc(num * sizeof(char *));
    ind_arr = (char **)malloc(num * sizeof(char *));

    for ( ii = 0; ii < num; ii++ ) {

        leng = strlen( spcinfo.cnty[ii].ugc );
	ugc_arr[ii] = (char *)malloc((leng+1) * sizeof(char));
	cst_ncpy( ugc_arr[ii], spcinfo.cnty[ii].ugc, leng, &ier );

	cnam_arr[ii] = (char *)malloc((CNAM_LEN+1) * sizeof(char));
	strcpy( cnam_arr[ii], spcinfo.cnty[ii].cname );

       /*
        * If have marine zones, then find the full name of the zone.
        * Check for 'Z' in the id array. If it is a county station id,
	* do not check name.
	*
	* Comment out the addtion of marine zones.
	*

	if ( hvmz ) {
	    if ( ugc_arr[ii][2] == 'Z') {
                ctb_mzgnm ( cnam_arr[ii], cnam_arr[ii], &ier );
		if ( ier < 0 ) {
		    printf("Do not have %s full name.\n", ugc_arr[ii]);
		}
	    }
	}
	*/

	leng = strlen( spcinfo.cnty[ii].state );
	st_arr[ii] = (char *)malloc((leng+1) * sizeof(char));
	strcpy( st_arr[ii], spcinfo.cnty[ii].state );

	leng = 33;
	ind_arr[ii] = (char *)malloc((leng+1) * sizeof(char));
        strcpy( ind_arr[ii], spcinfo.cnty[ii].indnam );
    }

    len = 4;
    leng = sizeof ( cntystr );
    cntystr[0] = '\0';
    cst_inch ( spcinfo.etime.day, cday, &ier ); 
    cst_ncpy ( chour, spcinfo.etime.hour, len, &ier );

    strcpy ( prdcod, " " );
    strcpy ( actn, " " );
    strcpy ( offid, " " );
    strcpy ( phen, " " );
    strcpy ( sigcd, " " );
    strcpy ( etn, " " );
    for ( ii = 0; ii < 5; ii++ ) {
	vtime[ii] = 0;
	etime[ii] = 0;
    }
    wbc_dcty ( ugc_arr, cnam_arr, st_arr, &num, cday, 
                chour, &leng, &ugcln, &vtecln, prdcod, 
		actn, offid, phen, sigcd, etn, vtime, 
		etime, ind_arr, cntystr, &ier );

    fprintf ( ifpsev, "%s", cntystr );

   /*
    *  Close output file.
    */

    cfl_clos ( ifpsev, &ier );

    for( ii = 0; ii < num; ii++ ) {
        free( ugc_arr[ii] );
	free( cnam_arr[ii] );
	free( st_arr[ii] );
	free( ind_arr[ii] );
    }

    if ( ugc_arr ) {
	free( (char **) ugc_arr );
	free( (char **) cnam_arr );
	free( (char **) st_arr );
	free( (char **) ind_arr );
    }

}
