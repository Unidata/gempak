#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

#define CNAM_LEN    256

extern SpcInfo_t      newinfo;
extern SpcInfo_t      spcinfo;

void vfwwcl ( char *preid, char *filnam, char *outstr, int *iret )
/************************************************************************
 * vfwwcl                                                               *
 *                                                                      *
 * This program opens, creates and closes the Weather Watch WCL (Watch  *
 * County Coordingation List ) text product file.			*
 *                                                                      *
 * vfwwcl ( preid, filnam, outstr, iret )                      		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	*preid		char		Preliminary ID			*
 *									*
 * Output parameters:                                                   *
 *	*filnam		char		Output file name		*
 *	*outstr		char		Product contents		*
 *      *iret		int		Return Code                     *
 *					 -2 = hit max length for cntystr*
 *					 -1 = invalid input arguments	*
 *					  0 = normal return		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC		 3/00   Modified from VFWSEV			*
 * A. Hardy/GSC		 3/00   Corrected top three header lines        *
 * A. Hardy/GSC		 5/00   Added PARISHES;cfl_aopn -> cfl_wopn; Use*
 *				AWIPS/WFO header ids.; remove 'NNNN'    *
 * A. Hardy/GSC		 5/00   Split apart multiple WFO IDs.           *
 * A. Hardy/GSC		 6/00   Cleaned up ind. cities storage/display  *
 * A. Hardy/GSC		12/00   Removed '&' from iret and ti_dayw       *
 * A. Hardy/GSC		 5/01   Removed parameter 'vmin' from vfgtod,   *
 *				cleaned up several fprintfs		*
 * A. Hardy/SAIC	10/01   Removed '#' from watch number		*
 * R. Tian/SAIC		06/02	Modified to meet the SPC requirement	*
 * E. Safford/SAIC	06/02	use ier not iret in cst_nocc()		*
 * S. Jacobs/NCEP	 9/02	Added UGC list; Format fixes		*
 * S. Jacobs/NCEP	11/02	Removed check for total num of counties	*
 * 				to output the end time			*
 * S. Jacobs/NCEP	 3/03	Added extra check for num of counties	*
 * S. Jacobs/NCEP	 4/03	Changed NWUS62 to NWUS64		*
 * A. Hardy/NCEP	 6/03   Added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 * A. Hardy/NCEP	 3/04	Reworked to usc wbc* library		*
 * A. Hardy/NCEP	 5/04	Changed 'ugcln' from 0 -> 1		* 
 * T. Piper/SAIC	12/05	Added cst_wrap of attention string	*
 * F. J. Yen/NCEP	 2/06	Increased size of cntystr; checked ier	*
 * J. Wu/SAIC		04/06	Added parameter in cst_wrap 		*
 ***********************************************************************/
{
    int     ier, lend, ii, itype = 1;
    int     iyear, imon, iday, ihour, imin, isec, julian;
    int     vtime[5], etime[5], ugcln, leng, len, num, vtecln;
    char    attnstr[200], wfostns[180], cntystr[12000];
    char    **aryptr, **cnam_arr, **ind_arr, **st_arr, **ugc_arr;
    char    actn[4], cday[3], chour[5], etn[5], offid[5], phen[3],
	    prdcod[2], sigcd[2], systime[7], tmzn[4];
    Boolean hvmz;
    const int line_len = 66;
/*-------------------------------------------------------------------*/
    *iret = 0;

   /*
    * Validate the input args.
    */
    if (preid == NULL || filnam == NULL || outstr == NULL ) {
	*iret = -1;
	return;
    }
    
    ier = 0;

   /*
    *  Create output file for appending.
    */
    sprintf ( filnam, "KWNSWCL%c", preid[0] );

   /*
    * Get UTC system time.
    */
    css_date ( &itype, &iyear, &imon, &iday, &ihour, &imin, &isec, 
               &julian, tmzn, &ier );
    sprintf(systime, "%02d%02d%02d", iday, ihour, imin);

   /*
    * Set up first three lines of information.
    */
    sprintf ( outstr, "NWUS64 KWNS %s\n", systime);
    sprintf ( &outstr[strlen(outstr)], "WCL%c  \n\n", preid[0]);

   /*
    * Set up effective time of day information section.
    */
    sprintf ( &outstr[strlen(outstr)], ".%s WATCH %c\n"
               "COORDINATION COUNTY LIST FROM THE NWS STORM PREDICTION CENTER\n"
               "EFFECTIVE UNTIL %s UTC.\n", 
               spcinfo.wtype, preid[0], spcinfo.etime.hour );
    sprintf ( &outstr[strlen(outstr)], "%s", "\n\n");


   /*
    * Read the marine zone table if coastal waters was found.
    */

    wbc_mzhv ( spcinfo.states, &hvmz, &ier );

   /*
    * Set up county watch section.
    */

    ugcln = 1;
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
	strcpy( cnam_arr[ii], spcinfo.cnty[ii].cname);

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

    len = 4;
    vtecln = 0;
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
    if ( ier == -1 ) {
	*iret = -2;
    }
    strcat ( outstr, cntystr );

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
     * Attention line for WFO offices.
     */

    sprintf ( attnstr, "ATTN...WFO...%s\n\n", wfostns);
    cst_wrap( attnstr, "...", &line_len, "\n", (char *)NULL, attnstr, &ier );
    sprintf ( &outstr[strlen(outstr)], attnstr );
}
