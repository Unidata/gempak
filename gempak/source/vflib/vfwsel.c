#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

#define  EOL  "\n"
#define  LINE_LEN   66

extern 	SpcInfo_t      newinfo;
extern 	SpcInfo_t      spcinfo;

void vfwsel ( char strin[], int *iret )
/************************************************************************
 * vfwsel                                                               *
 *                                                                      *
 * This program opens, creates and closes the Weather Watch SEL text    *
 * product file.					                *
 *                                                                      *
 * vfwsel ( strin, iret )                                      		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	strin[]		char		Continuing Watches String	*
 *									*
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          8/99   Created                                 *
 * A. Hardy/GSC          9/99   Changed truncation method for lats/lons *
 * M. Li/GSC		10/99	Added two more arguments to clo_cmpwds	*
 * M. Li/GSC		10/99	Modified output format			*
 * A. Hardy/GSC         11/99   Added ongoing/replacement statements;   *
 *                              Cleaned up line lengths                 *
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 * A. Hardy/GSC		 3/00   Removed \r\n after lightning.Fixed lens.*
 * A. Hardy/GSC		 3/00   Used cst_wrap to even line lengths      *
 * A. Hardy/GSC		 5/00   Changed cfl_aopn to cfl_wopn; Use	*
 *				AWIPS/WMO header ids.;removed 'NNNN'	*
 * A. Hardy/GSC		 5/00   Added check for continue watch nos.     *
 * A. Hardy/GSC		10/00   Added ck for '0' dist. anchor points    *
 * A. Hardy/GSC		12/00   Removed '&' from iret and ti_dayw       *
 * A. Hardy/GSC		 5/01   Removed parameter 'vmin' from vfgtod	*
 * A. Hardy/SAIC	10/01   Added check for old/new WMO header flag *
 * A. Hardy/SAIC	12/01   Removed unused varible 'i'		*
 * R. Tian/SAIC		06/02	Modified to meet the SPC requirement	*
 * R. Tian/SAIC		06/02	Added 'NWS' before 'STORM PRED...'	*
 * R. Tian/SAIC         04/03   Corrected if(iret == 0) syntax error    *
 * A. Hardy/NCEP	 4/03   Fixed replacement time;removed '...'    *
 *                              fixed line length with 'EFFECTIVE' line *
 * G. Grosshans/SPC	 5/03   Add 'OCCASIONALLY' statement;fixed typo *
 * A. Hardy/NCEP	 5/03	Change hail size to string for decimals *
 * A. Hardy/NCEP	11/03	Modified to use wbc/utl libraries; and  *
 *				to check if 'TEST' watch		*
 * G. Grosshans/SPC	 3/04	Removed extra EOLs before and after	*
 *				OTHER WATCH section			*
 * A. Hardy/NCEP        10/04   Corrected replace. wtch ck 1000->10000  *
 * G. Grosshans/SPC	10/04	Added ADD_WATCH_APPROX check		*
 * A. Hardy/NCEP         3/05   Added irmzn to wbc_dsts call seq.	*
 * G. Grosshans/SPC	10/05	Added LATLON_SAW_FORMAT tag to turn	*
 *				off lat-lon info at bottom of product.	*
 * T. Piper/SAIC	12/05	Updated for cst_wrap CSC		*
 * J. Wu/SAIC		04/06	Added parameter in cst_wrap 		*
 * H. Zeng/SAIC		06/06	Removed MIDNIGHT processing(fix wch#281)*
 * S. Guan/NCEP         02/22   Add cst_wrap before output SEL          * 
 ***********************************************************************/
{
    FILE    *ifpsel;
    char    ifname[256], chmon[4], chdwk[4], iampm[3], vampm[3];
    char    blank[2]={' '}, words1[16], words2[16], *arrch; 
    char    cname1[32], cname2[32], eampm[3];
    char    chnum[5], lclzn[4], avnstr[500], constr[500], perstr[180];
    char    tmpsel[500], holdsel[200], selstr[100], newname[132],
            efen[12], efst[12], stzstr[500], sttstr[500], hdlstr[180],
	    stlst[256], hailstr[4];
    char    prefs_tag[20];
    int     ier, time, datwk, inewtm, vnewtm, enewtm; 
    int     itmarr[5], iarr[5],etime[5], earr[5], vtime[5], varr[5]; 
    int     j, ipos, iin, iout, ireplace, newnum, irplen;
    int     len, leng, lenc, lenp, lena, len1, len2, irmzn;
    Boolean useln;
/*-------------------------------------------------------------------*/
    ier = 0;
    inewtm = 0;
    vnewtm = 0;
    enewtm = 0;
    ireplace = 0;
   /*
    *  Create output file for appending.
    */

    sprintf ( ifname, "WW%04d.SEL", spcinfo.wnum );
    ifpsel = cfl_wopn ( ifname, &ier );

   /*
    * Set up state zones and state name information.
    */

    irmzn = 1;
    len1 = sizeof ( stzstr );
    len2 = sizeof ( sttstr );
    strcpy (stlst, spcinfo.states);
    wbc_dsts ( stlst, &len1, &irmzn, stzstr, sttstr, &ier );

    spcinfo.sssnum = spcinfo.wnum % 10;
    fprintf ( ifpsel, "WWUS20 KWNS %02d%s\n", spcinfo.itime.day,
                                           spcinfo.itime.hour);
    fprintf( ifpsel, "SEL%d\n", spcinfo.sssnum);
    fprintf ( ifpsel, "%cSPC WW %02d%s\n", CHRS, spcinfo.itime.day,
                                       spcinfo.itime.hour);
    len = 59;
    cst_wrap ( stzstr, "-", &len, EOL, (char *)NULL, stzstr, &ier );
    fprintf ( ifpsel, "%s-%02d%s-\n\n",
                      stzstr, spcinfo.etime.day, spcinfo.etime.hour );
   /*
    * Set up urgent section.
    */

    hdlstr[0] = '\0';
    len = sizeof ( hdlstr );
    wbc_dhdl ( spcinfo.wtype, spcinfo.status, &(spcinfo.wnum), len, hdlstr, &ier );
    strcat (hdlstr, EOL );
    fprintf ( ifpsel, hdlstr );

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
  
    fprintf ( ifpsel, "%d%02d %s %s %s %s %d %d\n\n", inewtm, iarr[4], 
	      iampm, spcinfo.timzone, chdwk, chmon, iarr[2], iarr[0]);
   
			 
   /*
    * Set up general area description section.
    * Check status of watch.
    */

    if ( strcmp(spcinfo.status, "TEST") == 0 ) {
        fprintf ( ifpsel, "THE NWS STORM PREDICTION CENTER HAS ISSUED ");
        fprintf ( ifpsel, "A...TEST...\n ");
    }
    else {
        fprintf ( ifpsel, "THE NWS STORM PREDICTION CENTER HAS ISSUED A\n");
    }
    fprintf ( ifpsel, "%s WATCH FOR PORTIONS OF \n", spcinfo.wtype );
    fprintf ( ifpsel, sttstr);
    fprintf ( ifpsel, "\n\n");

   /*
    * Get the general time of day string.
    */

    leng = sizeof ( spcinfo.genday );
    utl_gtod ( vnewtm, enewtm, earr[4], vampm, eampm, datwk, leng, 
             spcinfo.genday, &ier);

   /*
    * Set up effective time of day string.
    */

    len1 = sizeof ( efst );
    len2 = sizeof ( efen );
    wbc_defl ( vnewtm, varr[4], vampm, enewtm, earr[4], eampm,
               spcinfo.timzone, len1, len2, efst, efen, &ier );

    len = LINE_LEN;
    tmpsel[0] = '\0';
    sprintf ( tmpsel, "EFFECTIVE THIS%s FROM %s UNTIL %s.",
              spcinfo.genday, efst, efen );

    strcat (tmpsel, EOL );
    strcat (tmpsel, EOL );
    len = LINE_LEN;
    cst_wrap ( tmpsel, blank, &len, EOL, (char *)NULL, tmpsel, &ier );
    fprintf ( ifpsel, tmpsel);

   /*
    * Add some phases if the watch is a 'PDS'
    */

    if ( strcmp ( spcinfo.pdsn,"PDS" ) == 0 )
        fprintf ( ifpsel, "...THIS IS A PARTICULARLY DANGEROUS SITUATION...\n\n"); 

   /*
    * Change hail size to string.
    */

    if ( ( (int) (spcinfo.hailsz * 10.0F) % 2) == 0 ) {
	sprintf( hailstr, "%1.*f", 0, spcinfo.hailsz );
    }
    else {
	sprintf( hailstr, "%3.*f", 1, spcinfo.hailsz );
    }

   /*
    * Set up hail, wind gusts and lightning section.
    */

    if ( strcmp(spcinfo.wtype,"TORNADO") == 0 ) {
        if ( strcmp(spcinfo.pdsn, "PDS") == 0 ) {
            tmpsel[0] = '\0'; 
            holdsel[0] = '\0'; 
            sprintf ( tmpsel, "DESTRUCTIVE TORNADOES...");
            if ( G_DIFF(spcinfo.hailsz, 0.0F) ){ 
	        sprintf ( holdsel, "THUNDERSTORM WIND GUSTS TO %d MPH", spcinfo.maxmph );
		strcat(tmpsel, holdsel);
	    }
            else if ( (spcinfo.hailsz > 0.0F ) && (spcinfo.hailsz <= 1.0F ) ) {
                sprintf ( holdsel, "LARGE HAIL TO %s INCH ", hailstr );
		strcat(tmpsel, holdsel);
	        sprintf ( holdsel, "IN DIAMETER... THUNDERSTORM WIND GUSTS TO %d MPH", spcinfo.maxmph );
		strcat(tmpsel, holdsel);
	    }
            else if ( spcinfo.hailsz > 1.0F ){
                sprintf ( holdsel, "LARGE HAIL TO %s INCHES ", hailstr );
		strcat(tmpsel, holdsel);
	        sprintf ( holdsel, "IN DIAMETER... THUNDERSTORM WIND GUSTS TO %d MPH", spcinfo.maxmph );
		strcat(tmpsel, holdsel);
	    }
            sprintf ( holdsel,  "...AND DANGEROUS LIGHTNING ARE POSSIBLE IN THESE AREAS.");
	    strcat(tmpsel, holdsel);
            strcat (tmpsel, EOL );
            strcat (tmpsel, EOL );
            len = LINE_LEN;
            cst_wrap ( tmpsel, blank, &len, EOL, (char *)NULL, tmpsel, &ier );
            fprintf ( ifpsel, tmpsel);
	}
        else {
            tmpsel[0] = '\0'; 
            holdsel[0] = '\0'; 
            sprintf ( tmpsel, "TORNADOES...");

            if ( G_DIFF(spcinfo.hailsz,0.0F) ) {
                sprintf ( holdsel, "THUNDERSTORM WIND GUSTS TO %d MPH...AND DANGEROUS LIGHTNING ", 
                          spcinfo.maxmph);
		strcat(tmpsel, holdsel);
	    }
            
            else if ( ( spcinfo.hailsz > 0.0F ) && ( spcinfo.hailsz <= 1.0F ) ) {
                sprintf ( holdsel, "HAIL TO %s INCH IN DIAMETER...THUNDERSTORM WIND ",
                          hailstr);
		strcat(tmpsel, holdsel);
                sprintf ( holdsel, "GUSTS TO %d MPH...AND DANGEROUS LIGHTNING ", spcinfo.maxmph);
		strcat(tmpsel, holdsel);
            }      
            else {
                sprintf ( holdsel, "HAIL TO %s INCHES IN DIAMETER...THUNDERSTORM WIND ",
                          hailstr);
		strcat(tmpsel, holdsel);
                sprintf ( holdsel, "GUSTS TO %d MPH...AND DANGEROUS LIGHTNING ", spcinfo.maxmph);
		strcat(tmpsel, holdsel);
            }
            sprintf ( holdsel, "ARE POSSIBLE IN THESE AREAS.");
	    strcat(tmpsel, holdsel);
            strcat (tmpsel, EOL );
            strcat (tmpsel, EOL );
            len = LINE_LEN;
            cst_wrap ( tmpsel, blank, &len, EOL, (char *)NULL, tmpsel, &ier );
            fprintf ( ifpsel, tmpsel);
        }
    }

    if ( strcmp(spcinfo.wtype,"SEVERE THUNDERSTORM") == 0 ) {
        if ( strcmp(spcinfo.pdsn, "PDS") == 0 ) {
            tmpsel[0] = '\0'; 
            holdsel[0] = '\0'; 
            sprintf ( tmpsel, "EXTREMELY DAMAGING THUNDERSTORM WIND GUSTS TO %d ", spcinfo.maxmph );
            if ( ( spcinfo.hailsz > 0.0F ) && ( spcinfo.hailsz <= 1.0F ) )  {
                sprintf ( holdsel, "MPH...LARGE HAIL TO %s INCH IN DIAMETER...", hailstr);
		strcat(tmpsel, holdsel);
	    }
            if ( spcinfo.hailsz > 1.0F ) { 
                sprintf ( holdsel, "MPH...LARGE HAIL TO %s INCHES IN DIAMETER...", hailstr);
		strcat(tmpsel, holdsel);
	    }
            sprintf ( holdsel, "AND DANGEROUS LIGHTNING ARE POSSIBLE IN THESE AREAS.");
	    strcat(tmpsel, holdsel);
            strcat (tmpsel, EOL );
            strcat (tmpsel, EOL );
            len = LINE_LEN;
            cst_wrap ( tmpsel, blank, &len, EOL, (char *)NULL, tmpsel, &ier );
            fprintf ( ifpsel, tmpsel);

        }
        else {
            tmpsel[0] = '\0'; 
            holdsel[0] = '\0'; 
            if ( G_DIFF(spcinfo.hailsz,0.0F) ) {
                sprintf ( tmpsel, "THUNDERSTORM WIND GUSTS TO %d MPH...",spcinfo.maxmph);
		strcat(tmpsel, holdsel);
	    }
            else if ( ( spcinfo.hailsz > 0.0F ) && (spcinfo.hailsz <= 1.0F ) ) {
                sprintf ( holdsel, "HAIL TO %s INCH IN DIAMETER...THUNDERSTORM WIND ",
                          hailstr);
		strcat(tmpsel, holdsel);
                sprintf ( holdsel, "GUSTS TO %d MPH...", spcinfo.maxmph);
		strcat(tmpsel, holdsel);
            }      
            else if ( spcinfo.hailsz > 1.0F ) {
                sprintf ( holdsel, "HAIL TO %s INCHES IN DIAMETER...THUNDERSTORM WIND ",
                          hailstr);
		strcat(tmpsel, holdsel);
                sprintf ( holdsel, "GUSTS TO %d MPH...", spcinfo.maxmph);
		strcat(tmpsel, holdsel);
            }        
            sprintf ( holdsel, "AND DANGEROUS LIGHTNING ARE POSSIBLE IN THESE AREAS.");
	    strcat(tmpsel, holdsel);
            strcat (tmpsel, EOL );
            strcat (tmpsel, EOL );
            len = LINE_LEN;
            cst_wrap ( tmpsel, blank, &len, EOL, (char *)NULL, tmpsel, &ier );
            fprintf ( ifpsel, tmpsel);
        }
    }

   /*
    * Set watch area section.
    */

    clo_cmpwds ( spcinfo.ancrpt.dirct1, &iin, words1, &iout, &ier );
    clo_cmpwds ( spcinfo.ancrpt.dirct2, &iin, words2, &iout, &ier );

    utl_gname ( spcinfo.ancrpt.stn1, spcinfo.ancrpt.stnnam1, 
		spcinfo.ancrpt.stateid1, &ier );
    utl_gname ( spcinfo.ancrpt.stn2, spcinfo.ancrpt.stnnam2, 
		spcinfo.ancrpt.stateid2, &ier );

    tb_idst ( spcinfo.ancrpt.stateid1, spcinfo.ancrpt.statnm1, &ier,
              strlen ( spcinfo.ancrpt.stateid1), sizeof(spcinfo.ancrpt.statnm1));
    tb_idst ( spcinfo.ancrpt.stateid2, spcinfo.ancrpt.statnm2, &ier,
              strlen ( spcinfo.ancrpt.stateid2), sizeof(spcinfo.ancrpt.statnm2));

    cst_rnan ( spcinfo.ancrpt.stnnam1, cname1, &ier );
    cst_rmst ( cname1, ".", &ipos, cname1, &ier );
    cst_rnan ( spcinfo.ancrpt.stnnam2, cname2, &ier );
    cst_rmst ( cname2, ".", &ipos, cname2, &ier );

   /*
    * Set up the watch area lines.
    */

    tmpsel[0] = '\0'; 
    holdsel[0] = '\0'; 

   /*
    * Check status of watch and also check if the APPROXIMATE verbage 
    * needs to be included in the text.
    */
    strcpy (prefs_tag, "ADD_WATCH_APPROX");
    ctb_pfbool (prefs_tag, &useln, &ier );
    if ( useln == TRUE ) {
       if (strcmp (spcinfo.status, "TEST") == 0 ) {
           sprintf ( tmpsel, "THE TEST %s WATCH AREA IS APPROXIMATELY ALONG AND %d STATUTE MILES ", 
                     spcinfo.wtype, spcinfo.ancatt.dist);
       }
       else {
           sprintf ( tmpsel, "THE %s WATCH AREA IS APPROXIMATELY ALONG AND %d STATUTE MILES ", 
                     spcinfo.wtype, spcinfo.ancatt.dist);
            }
    }
    else {
        if (strcmp (spcinfo.status, "TEST") == 0 ) {
            sprintf ( tmpsel, "THE TEST %s WATCH AREA IS ALONG AND %d STATUTE MILES ", 
                  spcinfo.wtype, spcinfo.ancatt.dist);
        }
        else {
            sprintf ( tmpsel, "THE %s WATCH AREA IS ALONG AND %d STATUTE MILES ", 
                  spcinfo.wtype, spcinfo.ancatt.dist);
        }
    }

    if ( (spcinfo.ancrpt.dist1 > 0 ) && ( spcinfo.ancrpt.dist2 > 0 ) ){
        sprintf ( holdsel, "%s OF A LINE FROM %d MILES %s OF %s %s",
                  spcinfo.ancatt.dirc, spcinfo.ancrpt.dist1, words1,
                  cname1, spcinfo.ancrpt.statnm1);
        strcat(tmpsel, holdsel);
        sprintf ( holdsel, " TO %d MILES %s OF %s %s.",
                  spcinfo.ancrpt.dist2, words2, cname2, 
		  spcinfo.ancrpt.statnm2);
    }
    else if ( (spcinfo.ancrpt.dist1 <= 0 ) && ( spcinfo.ancrpt.dist2 > 0 ) ){
        sprintf ( holdsel, "%s OF A LINE FROM %s %s",
                  spcinfo.ancatt.dirc, cname1, spcinfo.ancrpt.statnm1);
        strcat(tmpsel, holdsel);
        sprintf ( holdsel, " TO %d MILES %s OF %s %s.",
                  spcinfo.ancrpt.dist2, words2, cname2, 
		  spcinfo.ancrpt.statnm2);
    }
    else if ( (spcinfo.ancrpt.dist1 > 0 ) && ( spcinfo.ancrpt.dist2 <= 0 ) ){
        sprintf ( holdsel, "%s OF A LINE FROM %d MILES %s OF %s %s",
                  spcinfo.ancatt.dirc, spcinfo.ancrpt.dist1, words1,
                  cname1, spcinfo.ancrpt.statnm1);
        strcat(tmpsel, holdsel);
        sprintf ( holdsel, " TO %s %s.", cname2, spcinfo.ancrpt.statnm2);
    }
    else if ( (spcinfo.ancrpt.dist1 <= 0 ) && ( spcinfo.ancrpt.dist2 <= 0 ) ){
        sprintf ( holdsel, "%s OF A LINE FROM %s %s",
                  spcinfo.ancatt.dirc, cname1, spcinfo.ancrpt.statnm1);
        strcat(tmpsel, holdsel);
        sprintf ( holdsel, " TO %s %s.", cname2, spcinfo.ancrpt.statnm2);
    }
    strcat(tmpsel, holdsel);
    if ( useln == TRUE ) {
        sprintf (holdsel, "  FOR A COMPLETE DEPICTION OF THE WATCH SEE THE ");
        strcat(tmpsel, holdsel);
	sprintf (holdsel, "ASSOCIATED WATCH OUTLINE UPDATE (WOUS64 KWNS WOU%d).", spcinfo.sssnum);
        strcat(tmpsel, holdsel);
    }

    strcat (tmpsel, EOL );
    strcat (tmpsel, EOL );
    len = LINE_LEN;
    cst_wrap ( tmpsel, blank, &len, EOL, (char *)NULL, tmpsel, &ier );
    fprintf ( ifpsel, tmpsel);

   /*
    * Set up watch explanation section.
    */

    tmpsel[0] = '\0'; 
    lenc = sizeof ( constr );
    lenp = sizeof ( perstr );
    wbc_dcon ( spcinfo.wtype, lenc, lenp, constr, perstr, &ier );

    strcat ( tmpsel, "REMEMBER..."); 

    strcat ( tmpsel, constr );
    strcat ( tmpsel, perstr );

    if ( strcmp(spcinfo.wtype,"SEVERE THUNDERSTORM") == 0 ){
      strcat ( tmpsel, " SEVERE THUNDERSTORMS CAN AND OCCASIONALLY DO ");
      strcat ( tmpsel, "PRODUCE TORNADOES.");
    }

    strcat (tmpsel, EOL );
    len = LINE_LEN;
    cst_wrap ( tmpsel, blank, &len, EOL, (char *)NULL, tmpsel, &ier );
    fprintf ( ifpsel, tmpsel);
    
   /*
    * Check for ongoing and replacement watches.
    */

    tmpsel[0] = '\0'; 
    holdsel[0] = '\0'; 
    if ( strcmp ( spcinfo.replcnm[0], "NONE") != 0 ) {
	strcat (tmpsel, EOL);
        strcat ( tmpsel, "OTHER WATCH INFORMATION...");

       /*
        *  If there are replacement watches listed, retrieve the type
        *  for each replacement watch.
        */

        if ( strcmp ( spcinfo.replcnm[0], "NONE") != 0 ) {
            strcpy ( newname, "ww");
            strcat ( newname, spcinfo.replcnm[0] );
            strcat ( newname, ".txt" );
            strcpy ( newinfo.file_info.filnam,newname);
            vfrptxt (newname, iret);
            if ( *iret == 0 ){
                sprintf ( holdsel, "THIS %s WATCH REPLACES %s WATCH NUMBER %d.",
	                  spcinfo.wtype, newinfo.wtype, atoi(spcinfo.replcnm[0]) ); 
                strcat (tmpsel, holdsel );
            }
        }

       /*
        *  If there are more that 1 replacement watches listed,
        *  proceed with the loop.
        */
        if ( spcinfo.wwrepnm >= 1 ){
            for(j = 1; j <= spcinfo.wwrepnm; j++) {
                strcpy ( newname, "ww");
                strcat ( newname, spcinfo.replcnm[j] );
                strcat ( newname, ".txt" );
                strcpy ( newinfo.file_info.filnam,newname);
                vfrptxt (newname, iret);
                if ( *iret == 0 ){
	            sprintf ( holdsel, "..%s WATCH NUMBER %d.",
	                   newinfo.wtype, atoi(spcinfo.replcnm[j]) );
                    strcat (tmpsel, holdsel );
                }
	    }
        }

        sprintf ( holdsel, " WATCH NUMBER ");
        strcat (tmpsel, holdsel );
        for(j = 0; j <= spcinfo.wwrepnm; j++) {
            strcpy ( newname, "ww");
            strcat ( newname, spcinfo.replcnm[j] );
            strcat ( newname, ".txt" );
            strcpy ( newinfo.file_info.filnam,newname);
            vfrptxt (newname, iret);
            if ( *iret == 0 ){
                sprintf ( holdsel, "%d ", atoi(spcinfo.replcnm[j]) ) ; 
                strcat (tmpsel, holdsel );
	    }
	}

       /*
        * Set cancellation time with the replacement watch's valid time.
        */

        strcat ( tmpsel, "WILL NOT BE IN EFFECT AFTER " );
        if ( (vnewtm == 12) && (varr[4] ==0) && (strcmp(vampm, "AM") == 0) ) {
            sprintf ( holdsel, "MIDNIGHT %s. ", spcinfo.timzone );
            strcat (tmpsel, holdsel );
	}
        else if ( (vnewtm == 12) && (varr[4] ==0) && (strcmp(vampm, "PM") == 0) ) {
            sprintf ( holdsel, "NOON %s. ", spcinfo.timzone );
            strcat (tmpsel, holdsel );
	}
        else {
            sprintf ( holdsel, "%d%02d %s %s. ", vnewtm, varr[4], vampm, 
	              spcinfo.timzone );
            strcat (tmpsel, holdsel );
	}
	ireplace = 1;
    }

    cst_lstr ( strin, &irplen, iret );
    strcpy ( selstr, strin);
    if ( irplen > 0 ) {
        holdsel[0] = '\0'; 
        if ( ireplace != 1 ) {
            strcat (tmpsel, EOL);
	    strcat( tmpsel, "OTHER WATCH INFORMATION...");
	}
        strcat ( tmpsel, "CONTINUE...");
        arrch = strtok( selstr, " " );
	while ( arrch != NULL ) {
	    newnum = atoi(arrch);
	    if ( (newnum > 0 ) && ( newnum < 10000 ) ) {
	        strcat ( tmpsel,"WW ");
	        cst_inch ( newnum, chnum, iret);
	        strcat ( tmpsel, chnum);
	        strcat ( tmpsel, "...");
	    }
	    arrch = strtok( NULL," " );
	}
    strcat (tmpsel, EOL);
    }

    if (( ireplace == 1 ) && ( irplen < 1 )) {
    strcat (tmpsel, EOL);
    }
    cst_wrap ( tmpsel, blank, &len, EOL, (char *)NULL, tmpsel, &ier );
    fprintf ( ifpsel, tmpsel);

   /*
    * Check status of watch.
    */

    if (strcmp (spcinfo.status, "TEST") == 0 ) {
        fprintf ( ifpsel, "\nDISCUSSION...THIS IS A TEST...");
        fprintf ( ifpsel, "THIS IS A TEST...\n\n");
    }
    else {
        fprintf ( ifpsel, "\nDISCUSSION...\n\n");
    }

   /*
    * Create the aviation string.
    */

    lena = sizeof ( avnstr );
    wbc_davn ( spcinfo.wtype, &(spcinfo.hailsz), &(spcinfo.maxgust),
               &(spcinfo.maxtops), &(spcinfo.motion.deg), 
	       &(spcinfo.motion.speed), lena, avnstr, &ier );

    strcat (avnstr, EOL );
    strcat (avnstr, EOL );
    strcat (avnstr, EOL );
    len = LINE_LEN;
    cst_wrap ( avnstr, blank, &len, EOL, (char *)NULL, avnstr, &ier );
    fprintf ( ifpsel, avnstr);

   /*
    * Print out forecaster's name.
    */

    fprintf ( ifpsel, "...%s\n\n",spcinfo.frcstr);

   /*
    *  Get the value for LATLON_SAW_FORMAT flag from the prefs.tbl
    *  If FALSE then its before February 2006 and the less
    *  precise lat-lon data is retained in the SEL.  When this
    *  is TRUE then more precise lat-lon data will be located 
    *  in the SAW product.
    */
    strcpy (prefs_tag, "LATLON_SAW_FORMAT");
    ctb_pfbool (prefs_tag, &useln, &ier );
    if ( useln == FALSE ) {

        /* 
         * Converting hundreths into minutes.
         */

         utl_tomin ( &(spcinfo.wcpnt1.lat),  &(spcinfo.wcpnt1.lon), 
       		&(spcinfo.wcpnt1.newlat),  &(spcinfo.wcpnt1.newlon), iret );
         utl_tomin ( &(spcinfo.wcpnt2.lat),  &(spcinfo.wcpnt2.lon), 
   		&(spcinfo.wcpnt2.newlat),  &(spcinfo.wcpnt2.newlon), iret );
         utl_tomin ( &(spcinfo.wcpnt3.lat),  &(spcinfo.wcpnt3.lon), 
    		&(spcinfo.wcpnt3.newlat),  &(spcinfo.wcpnt3.newlon), iret );
         utl_tomin ( &(spcinfo.wcpnt4.lat),  &(spcinfo.wcpnt4.lon), 
   		&(spcinfo.wcpnt4.newlat),  &(spcinfo.wcpnt4.newlon), iret );

       /*
        *  The statement below will truncate the minutes at the 10's position.
        *  No rounding takes place.
        */

        fprintf ( ifpsel, ";%d,%04d %d,%04d %d,%04d %d,%04d; \n\n",
            (int)(spcinfo.wcpnt1.newlat*10.0F), (int)(spcinfo.wcpnt1.newlon*10.0F),
            (int)(spcinfo.wcpnt2.newlat*10.0F), (int)(spcinfo.wcpnt2.newlon*10.0F),
            (int)(spcinfo.wcpnt3.newlat*10.0F), (int)(spcinfo.wcpnt3.newlon*10.0F),
            (int)(spcinfo.wcpnt4.newlat*10.0F), (int)(spcinfo.wcpnt4.newlon*10.0F));
    }

   /*
    *  Close output file.
    */

    cfl_clos ( ifpsel, &ier );
}
