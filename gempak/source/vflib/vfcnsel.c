#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern	SpcInfo_t	newinfo;
extern	SpcInfo_t	spcinfo;

void vfcnsel ( int *iret )
/************************************************************************
 * vfcnsel                                                              *
 *                                                                      *
 * This program opens, creates and closes the Weather Watch cancellation*
 * SEL text product file.					        *
 *                                                                      *
 * vfcnsel ( iret )                                            	    	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC         10/99   Created                                 *
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 * A. Hardy/GSC		 5/00	Changed cfl_aopn to cfl_wopn; Use	*
 *				AWIPS/WMO header ids.; removed 'NNNN'   *
 * A. Hardy/GSC		12/00   Removed '&' from ti_dayw and css_date   *
 * A. Hardy/SAIC	10/01   Added check for old/new WMO header flag *
 * R. Tian/SAIC		06/02	Modified to meet the SPC requirement    *
 * R. Tian/SAIC		06/02	Added 'NWS' before 'STORM PRED...'      *
 * A. Hardy/NCEP	 6/03   Added tmzn to CSS_DATE		        *
 * A. Hardy/NCEP	 7/03	Added calls to UTL and WBC library	*
 * A. Hardy/NCEP	11/03	Added watch status check for 'TEST'     *
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 * A. Hardy/NCEP	03/05	Added irmzn to wbc_dsts call seq.       *
 * G. Grosshans/SPC	04/05	Updated header to remove CANCELLATION  	*
 *          			and FAA line	                	*
 * G. Grosshans/SPC     03/06   Updated to remove forecaster name from  *
 *                              SEL-cancel product                      *
 * H. Zeng/SAIC		06/06	Removed MIDNIGHT processing in MMD hdr; *
 *				Fixed day of week for MIDNIGHT in text	*
 ***********************************************************************/
{
    FILE    *ifpsel;
    char    ifname[256], chmon[4], chdwk[4], zone1[4], ampm[3]; 
    char    stlst[256], tmzn[4], zone2[4]; 
    char    stzstr[500], sttstr[500];
    float   hdiff;
    int     ier, time, datwk, newtime;
    int     itmarr[5], iarr[5]; 
    int     itype, iyr, imon, idy, ihr, imin, isc, julian;
    int     leni, len1, irmzn;
/*-------------------------------------------------------------------*/
    ier = 0;
    newtime = 0;
    itype = 1;

   /*
    *  Create output file for appending.
    */

    sprintf ( ifname, "WW%04d.SEL.CNL", spcinfo.wnum );
    ifpsel = cfl_wopn ( ifname, &ier );

   /*
    * Set up state zones and state name information.
    */

     irmzn = 1;
     len1 = sizeof ( stzstr );
     strcpy ( stlst, spcinfo.states );
     wbc_dsts ( stlst, &len1, &irmzn, stzstr, sttstr, &ier );

   /* 
    * Get current system time for issue time of cancel product.
    */
    leni = sizeof (spcinfo.curtim);
    utl_ctim ( leni, spcinfo.curtim, &ier );

    spcinfo.sssnum = spcinfo.wnum % 10;
    fprintf ( ifpsel, "WWUS20 KWNS %s\n", spcinfo.curtim);
    fprintf ( ifpsel, "SEL%d\n", spcinfo.sssnum);
    fprintf ( ifpsel, "%cSPC WW %s\n", CHRS, spcinfo.curtim);
    fprintf ( ifpsel, "%s-%02d%s-\n\n",
              stzstr, spcinfo.etime.day, spcinfo.etime.hour );
   /*
    * Get current system time for local issue time of cancel product.
    */

    css_date ( &itype, &iyr, &imon, &idy, &ihr, &imin, &isc, &julian, 
    	       tmzn, iret);

    chmon[0] = '\0';
    chdwk[0] = '\0';
    strcpy ( zone1, "UTC"); 
    strcpy ( zone2, spcinfo.timzone); 

    time  = ihr;
    itmarr[0] = iyr;
    itmarr[1] = imon;
    itmarr[2] = idy;
    itmarr[3] = ihr;
    itmarr[4] = imin;

    ti_dayw( itmarr, &datwk, &ier );
    utl_gmon ( itmarr[1], chmon, &ier );
    utl_gdwk ( datwk, chdwk, &ier );
    ti_tzdf ( itmarr, zone1, zone2, iarr, &hdiff, &ier, 
              strlen(zone1), strlen(zone2) ); 

    utl_ampm ( iarr[3], &newtime, ampm, &ier );

    if ( itmarr[2] != iarr[2] ) {
        ti_dayw( iarr, &datwk, &ier);
        utl_gmon ( iarr[1], chmon, &ier );
        utl_gdwk ( datwk, chdwk, &ier );
    }

   /*
    * Set up urgent section.
    */

    fprintf ( ifpsel, "URGENT - IMMEDIATE BROADCAST REQUESTED\n");

   /*
    * Check if watch has been issued as a 'TEST'.
    */ 

    if ( strcmp (spcinfo.status, "TEST") == 0 ) {
        fprintf ( ifpsel, "TEST...%s WATCH - NUMBER %d...TEST\n", 
              spcinfo.wtype, spcinfo.wnum );
    }
    else {
        fprintf ( ifpsel, "%s WATCH - NUMBER %d \n", 
              spcinfo.wtype, spcinfo.wnum );
    }
    fprintf ( ifpsel, "NWS STORM PREDICTION CENTER NORMAN OK \n");

    fprintf ( ifpsel, "%d%02d %s %s %s %s %d %d\n\n", newtime, iarr[4], 
	      ampm, spcinfo.timzone, chdwk, chmon, iarr[2], iarr[0]);
			 
   /*
    * Getting all of the information to create the the time line string.
    */

    strcpy ( zone1, "UTC" );
    strcpy ( chmon, " " );
    time  = atoi(spcinfo.itime.hour);
    itmarr[0] = spcinfo.itime.year;
    itmarr[1] = spcinfo.itime.month;
    itmarr[2] = spcinfo.itime.day;
    itmarr[3] = time / 100;
    itmarr[4] = time % 100;

    ti_dayw( itmarr, &datwk, &ier );
    utl_gmon ( itmarr[1], chmon, &ier );
    utl_gdwk ( datwk, chdwk, &ier );
    ti_tzdf ( itmarr, zone1, spcinfo.timzone, iarr, &hdiff, &ier,
              strlen(zone1), strlen(spcinfo.timzone) );
    utl_ampm ( iarr[3], &newtime, ampm, &ier );

   /*
    * Change 1200 AM to MIDNIGHT.
    *
    * Set cancelled watch and time.
    */

    if ( strcmp ( spcinfo.status, "TEST") == 0 ) {
        fprintf ( ifpsel, "THE NWS STORM PREDICTION CENTER ");
        fprintf ( ifpsel, "HAS CANCELLED...TEST...\n");
    }
    else {
        fprintf ( ifpsel, "THE NWS STORM PREDICTION CENTER HAS CANCELLED \n");
    }

    if ( (newtime == 12) && (iarr[4] ==0) && strcmp(ampm, "AM") == 0 ) {

        fprintf ( ifpsel, "%s WATCH NUMBER %d ISSUED AT MIDNIGHT ",
	          spcinfo.wtype, spcinfo.wnum);
	fprintf ( ifpsel, "%s FOR PORTIONS OF \n", spcinfo.timzone);
    }
    else {

        fprintf ( ifpsel, "%s WATCH NUMBER %d ISSUED AT ", 
	          spcinfo.wtype, spcinfo.wnum); 
	fprintf (  ifpsel, "%d%02d %s %s FOR PORTIONS OF\n",  
		  newtime, iarr[4], ampm, spcinfo.timzone );
    }

   /*
    * Set cancelled states.
    */

    fprintf ( ifpsel,  sttstr);

    fprintf ( ifpsel, "\n\n");

   /*
    *  Close output file.
    */

    cfl_clos ( ifpsel, &ier );
}
