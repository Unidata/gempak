#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

#define  CNAM_LEN  256

extern SpcInfo_t      spcinfo;

void vfwoui ( int *iret )
/************************************************************************
 * vfwoui                                                               *
 *                                                                      *
 * This program opens, creates and closes the  WOU - inital ( Watch     *
 * Outline Update ) text product file.					*
 *                                                                      *
 * vfwoui ( iret )                                             		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC		 3/00   Modified from VFWWCL			*
 * A. Hardy/GSC		 5/00   Changed cfl_aopn to cfl_wopn; Use	*
 *				AWIPS/WFO header ids.;removed 'NNNN'	*
 * A. Hardy/GSC		 5/00   Split up multiple WFO IDs.		*
 * A. Hardy/GSC		 6/00   Cleaned up ind. cities saving/display   *
 * A. Hardy/GSC		12/00   Removed '&' from iret and ti_dayw       *
 * A. Hardy/GSC		 5/01   Removed parameter 'vmin' from vfgtod,	*
 *				removed 'indy' variable			*
 * A. Hardy/SAIC	10/01   Removed '#' from watch numbers		*
 * R. Tian/SAIC		06/02	Modified to meet the SPC requirement	*
 * R. Tian/SAIC		06/02	Removed '\r', added FIPS for ind. cities*
 * G. Grosshans/SPC	08/02	Removed list of participating WFOs      *
 * 				from the beginning of the product	*
 * S. Jacobs/NCEP	 9/02	Format spacing fixes			*
 * S. Jacobs/NCEP	11/02	Removed check for total num of counties	*
 * 				to output the end time			*
 * A. Hardy/NCEP	 3/03   Added 'TEST' to header 			*
 * S. Jacobs/NCEP	 3/03	Added extra check for num of counties	*
 * S. Jacobs/NCEP	 3/03	Changed "REMAINS" to "IS"		*
 * A. Hardy/NCEP	 5/03   Removed leading space in front of names;*
 *				made UGC length match WOUPDT		*
 * A. Hardy/NCEP	 5/03	Added 'BULLETIN...' headline		*
 * A. Hardy/NCEP	10/03	Modified to use wbc/utl libraries  	*
 * A. Hardy/NCEP        11/03   Added watch status check for 'TEST'     *
 * A. Hardy/NCEP	 1/04   Added VTEC parameters to WBC_DCTY	*
 * A. Hardy/NCEP	 3/04	Added marine zones name check		*
 * G. Grosshans/SPC      4/04   Per Schaefer, removed MIDNIGHT & NOON   *
 *                              from WOU.                               *
 * A. Hardy/NCEP	 6/04	Removed ':' from time and 'EFFECT' line *
 * A. Hardy/NCEP	 8/04	Added code: 'TEST...' for MND if desired*
 * M. Li/SAIC           10/04   Replaced ctb_rdwou with ctb_rdprf	*
 * T. Piper/SAIC	01/06	Added wrapping of Attention line	*
 * J. Wu/SAIC		04/06	Added parameter in cst_wrap 		*
 * H. Zeng/SAIC		06/06   modified calling sequence for utl_ivet	*
 ***********************************************************************/
{
    FILE    *ifpwou;
    int     ier, etime[5], enewtm, time, inewtm;
    int     vnewtm, earr[5], itmarr[5], iarr[5], vtime[5], varr[5];
    int     ii, datwk, num, lend, leng, ugcln, vtecln;
    char    attnstr[200], wfostns[180], cntystr[10000];
    char    **aryptr, **cnam_arr, **ind_arr, **st_arr, **ugc_arr;
    char    actn[4], cday[3], chour[5], etn[5], offid[5], phen[3],
	     prdcod[2], sigcd[2], ifname[256], lclzn[4];
    char    chdwk[4], chmon[4], dirsym[160], eampm[3], iampm[3],
	    tag[25], tblnam[72], value[120], vampm[3];
    Boolean hvmz;
    const int line_len = 66;
/*-------------------------------------------------------------------*/
    *iret = 0;
    hvmz = False;

    ier = 0;
    enewtm = 0;

   /*
    *  Create output file for appending.
    */

    sprintf ( ifname, "WW%04d.WOU", spcinfo.wnum );
    ifpwou = cfl_wopn ( ifname, &ier );

   /*
    * storing the issue, valid and ending time.
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
    * Create the 'ATTN' line for WFO offices.
    */

    aryptr = (char **)malloc(spcinfo.total * sizeof(char *));
    for ( ii = 0; ii < spcinfo.total; ii++ ) {
        lend = strlen( spcinfo.cnty[ii].wfo );
	aryptr[ii] = (char *)malloc((lend+1) * sizeof(char));
	strcpy( aryptr[ii], spcinfo.cnty[ii].wfo );
    }

    utl_wfos ( aryptr, spcinfo.total, wfostns, &ier );

    for( ii = 0; ii < spcinfo.total; ii++ )
        free( aryptr[ii] );
    if ( aryptr )
	free( (char **) aryptr );

   /*
    * Set up the header information.
    */

    spcinfo.sssnum = spcinfo.wnum % 10;

    fprintf ( ifpwou, "WOUS64 KWNS %02d%s\n", spcinfo.vtime.day,spcinfo.vtime.hour);
    fprintf ( ifpwou, "WOU%d\n", spcinfo.sssnum);

    fprintf ( ifpwou, "\n");

    fprintf ( ifpwou, "BULLETIN - IMMEDIATE BROADCAST REQUESTED\n");
   /*
    * Uncomment the 3 lines below if 'TEST...mnd...TEST'is the desired format. 
    */
    if ( strcmp(spcinfo.status, "TEST") == 0 ) {
       fprintf ( ifpwou, "TEST...");
    }
    if ( strcmp (spcinfo.wtype, "SEVERE THUNDERSTORM") == 0 ) {
        fprintf ( ifpwou, "%s WATCH OUTLINE UPDATE FOR WS ",
                           spcinfo.wtype  );
    }
    else if ( strcmp (spcinfo.wtype, "TORNADO") == 0 ) {
        fprintf ( ifpwou, "%s WATCH OUTLINE UPDATE FOR WT ", 
                           spcinfo.wtype );
    }

   /*
    * Check if watch has been issued as a 'TEST'.
    */

    if ( strcmp(spcinfo.status, "TEST") == 0 ) {
        fprintf ( ifpwou, "%d...TEST\n", spcinfo.wnum );
    }
    else {
        fprintf ( ifpwou, "%d\n", spcinfo.wnum );
    }

    fprintf ( ifpwou, "NWS STORM PREDICTION CENTER NORMAN OK\n");
        fprintf ( ifpwou, "%d%02d %s %s %s %s %d %d\n\n", vnewtm, varr[4], 
	       vampm, spcinfo.timzone, chdwk, chmon, iarr[2], iarr[0]);

   /*
    * Set up effective time of day information section.
    */

    if ( strcmp (spcinfo.wtype, "SEVERE THUNDERSTORM") == 0 ) {
        fprintf ( ifpwou, "%s WATCH %d IS IN EFFECT UNTIL ",
              spcinfo.wtype, spcinfo.wnum );
            fprintf ( ifpwou, "%d%02d %s %s", enewtm, earr[4], eampm,
                  spcinfo.timzone );
        fprintf ( ifpwou, "\nFOR THE FOLLOWING LOCATIONS\n\n");

    }
    if ( strcmp (spcinfo.wtype, "TORNADO") == 0 ) {
        fprintf ( ifpwou, "%s WATCH %d IS IN EFFECT UNTIL ",
              spcinfo.wtype, spcinfo.wnum );
            fprintf ( ifpwou, "%d%02d %s %s", enewtm, earr[4], eampm,
                  spcinfo.timzone );
        fprintf ( ifpwou, " FOR THE\n FOLLOWING LOCATIONS\n\n");
    }

   /*
    * Read the marine zone table if coastal waters was found.
    */

   wbc_mzhv ( spcinfo.states, &hvmz, &ier );

   /*
    * Set up county watch section. 
    */

    ugcln  = 1;
    num = spcinfo.total;
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
	*/
	if ( hvmz ) {
	    if ( ugc_arr[ii][2] == 'Z') {
                ctb_mzgnm ( cnam_arr[ii], cnam_arr[ii], &ier );
		if ( ier < 0 ) {
		    printf("Do not have %s full name.\n", 
		       ugc_arr[ii]);
		}
	    }
	}

	leng = strlen( spcinfo.cnty[ii].state );
	st_arr[ii] = (char *)malloc((leng+1) * sizeof(char));
	strcpy( st_arr[ii], spcinfo.cnty[ii].state );

	leng = 33;
	ind_arr[ii] = (char *)malloc((leng+1) * sizeof(char));
        strcpy( ind_arr[ii], spcinfo.cnty[ii].indnam );
    }
   /*
    * Set values for VTEC line.
    */

    strcpy ( tblnam, "woudef.tbl" );
    strcpy ( dirsym, "txtprd" );
    strcpy ( tag, "INIT_WOU_USE_VTEC");
    ctb_rdprf ( tblnam, dirsym, tag, value, &ier );
    cst_numb ( value, &vtecln, &ier);

    if ( strcmp(spcinfo.status, "TEST") == 0 ) {
        strcpy ( prdcod, "T");
    }
    else {
        strcpy ( tag, "INIT_WOU_PROD_CODE");
        ctb_rdprf ( tblnam, dirsym, tag, value, &ier );
        strcpy ( prdcod, value);
    }

    if ( strcmp (spcinfo.wtype, "SEVERE THUNDERSTORM") == 0 ) {
        strcpy ( phen, "SV" );
    }
    else if ( strcmp (spcinfo.wtype, "TORNADO") == 0 ) {
        strcpy ( phen, "TO" );
    }
    sprintf ( etn, "%04d", spcinfo.wnum );

    strcpy ( tag, "INIT_WOU_ACTION");
    ctb_rdprf ( tblnam, dirsym, tag, value, &ier );
    strcpy ( actn, value);

    strcpy ( tag, "INIT_WOU_OFFID");
    ctb_rdprf ( tblnam, dirsym, tag, value, &ier );
    strcpy ( offid, value );

    strcpy ( tag, "INIT_WOU_SIGCD");
    ctb_rdprf ( tblnam, dirsym, tag, value, &ier );
    strcpy ( sigcd, value );

    lend = 4;
    leng = sizeof ( cntystr );
    cst_inch ( spcinfo.etime.day, cday, &ier );
    cst_ncpy ( chour, spcinfo.etime.hour, lend, &ier );
    wbc_dcty ( ugc_arr, cnam_arr, st_arr, &num, cday, 
               chour, &leng, &ugcln, &vtecln, prdcod, 
	       actn, offid, phen, sigcd, etn, vtime, 
	       etime, ind_arr, cntystr, &ier );

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
    fprintf ( ifpwou, "%s", cntystr );

   /*
    * Attention line for WFO offices.
    */
    sprintf ( attnstr, "ATTN...WFO...%s\n\n", wfostns);
    cst_wrap( attnstr, "...", &line_len, "\n", (char *)NULL, attnstr, &ier );
    fprintf ( ifpwou, "%s\n\n", attnstr);

   /*
    *  Close output file.
    */

    cfl_clos ( ifpwou, &ier );
}
