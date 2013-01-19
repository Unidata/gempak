#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

#define  EOL  "\n"
#define  LINE_LEN  71 

extern SpcInfo_t      spcinfo;

void vfwwcp ( int *iret )
/************************************************************************
 * vfwwcp                                                               *
 *                                                                      *
 * This program opens, creates and closes the WCP ( Watch Corner        *
 * Point - radar chart ) text product file.				*
 *                                                                      *
 * vfwwcp ( iret )                                             		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC		 5/00   Modified from VFWOUI			*
 * A. Hardy/GSC		12/00   Removed '&' from iret and ti_dayw	*
 * A. Hardy/GSC		 5/01   Removed parameter 'vmin' from vfgtod	*
 * A. Hardy/SAIC	10/01   Changed 'MKC' to 'SPC'			*
 * R. Tian/SAIC		06/02	Modified to meet the SPC requirement	*
 * A. Hardy/NCEP	 6/03	Added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 ***********************************************************************/
{
    FILE    *ifpwcp;
    char    ifname[256], vorstr[400], locnam[15], type[4];
    char    ampm[3], zone1[4], ampm1[3]; 
    char    chmon[4], chdwk[4], tmzn[4]; 
    float   vorlat1, vorlon1, vorlat2, vorlon2, hdiff;
    float   newlat1, newlon1, newlat2, newlon2;
    int     ier, etime[5], newtm, time;
    int     newtime, earr[5];
    int     itmarr[5], iarr[5], vtime[5], varr[5], datwk;
    int     istyp, isyr, ismon, isday, ishr, ismin, issec, ijulian;
/*-------------------------------------------------------------------*/
    ier = 0;
    newtm = 0;
    strcpy(locnam, "VOR");
    strcpy(type, "WCP");
    istyp =  1;

   /*
    *  Create output file for appending.
    */
    sprintf ( ifname, "WW%04d.WCP", spcinfo.wnum );
    ifpwcp = cfl_wopn ( ifname, &ier );

   /*
    * storing the issue, valid and ending time.
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
    vfgdat ( itmarr[1], datwk, chmon, chdwk, &ier );
    ti_tzdf ( itmarr, zone1, spcinfo.timzone, iarr, &hdiff, &ier,
              strlen(zone1), strlen(spcinfo.timzone) );
    vfampm ( iarr[3], &newtime, ampm, &ier );

    if ( itmarr[2] != iarr[2] ) {
        ti_dayw( iarr, &datwk, &ier);
        vfgdat ( iarr[1], datwk, chmon, chdwk, &ier );
    }

    time     = atoi(spcinfo.vtime.hour);
    vtime[0] = spcinfo.vtime.year;
    vtime[1] = spcinfo.vtime.month;
    vtime[2] = spcinfo.vtime.day;
    vtime[3] = time / 100;
    vtime[4] = time % 100;

    ti_tzdf ( vtime, zone1, spcinfo.timzone, varr, &hdiff, &ier,
              strlen(zone1), strlen(spcinfo.timzone) );
    vfampm ( varr[3], &newtime, ampm, &ier );

    time     = atoi(spcinfo.etime.hour);
    etime[0] = spcinfo.etime.year;
    etime[1] = spcinfo.etime.month;
    etime[2] = spcinfo.etime.day;
    etime[3] = time / 100;
    etime[4] = time % 100;
    ti_tzdf ( etime, zone1, spcinfo.timzone, earr, &hdiff, &ier,
              strlen(zone1), strlen(spcinfo.timzone) );
    vfampm ( earr[3], &newtm, ampm1, &ier );
    vfgtod ( newtime, newtm, earr[4], ampm, ampm1, datwk,
             spcinfo.genday, &ier);

   /*
    * Set up top three lines of information.
    */

    css_date ( &istyp, &isyr, &ismon, &isday, &ishr, &ismin, &issec, 
               &ijulian, tmzn, &ier);

    fprintf ( ifpwcp, "WOUS50 KSPC %02d%s\n", spcinfo.itime.day,
                       spcinfo.itime.hour);
    fprintf ( ifpwcp, "SPCSEVSPC\n");
    fprintf ( ifpwcp, "FILE CREATED %02d-%s-%02d AT %02d:%02d:%02d UTC\n\n", 
                       isyr % 100 , chmon, isday, ishr, ismin, issec);

   /*
    * Set up effective time of day information section.
    */

    if ( strcmp(spcinfo.wtype,"TORNADO") == 0 ) {
        fprintf ( ifpwcp, "TORN %s WT0%d %s\n", spcinfo.itime.hour, spcinfo.wnum,
	          spcinfo.etime.hour);
    }
    if ( strcmp(spcinfo.wtype,"SEVERE THUNDERSTORM") == 0 ) {
        fprintf ( ifpwcp, "SEVR %02d%02d%02d %s WT%04d %s\n", (iarr[0] % 10),
	          spcinfo.itime.month, spcinfo.itime.day, spcinfo.itime.hour, 
		  spcinfo.wnum, spcinfo.etime.hour);
    }

   /*
    *  Locate the VOR corner points.
    */

    vfvors ( locnam, type, vorstr, &vorlat1, &vorlon1, &vorlat2, &vorlon2,
                 iret);

    /*
     * Converting hundreths into minutes.
     */

     vftomin ( spcinfo.wcpnt1.lat,  spcinfo.wcpnt1.lon,
                &(spcinfo.wcpnt1.newlat),  &(spcinfo.wcpnt1.newlon), iret );
     vftomin ( spcinfo.wcpnt2.lat,  spcinfo.wcpnt2.lon,
                &(spcinfo.wcpnt2.newlat),  &(spcinfo.wcpnt2.newlon), iret );
     vftomin ( vorlat2,  vorlon2, &newlat2,  &newlon2, iret );
     vftomin ( spcinfo.wcpnt3.lat,  spcinfo.wcpnt3.lon,
                &(spcinfo.wcpnt3.newlat),  &(spcinfo.wcpnt3.newlon), iret );
     vftomin ( spcinfo.wcpnt4.lat,  spcinfo.wcpnt4.lon,
                &(spcinfo.wcpnt4.newlat),  &(spcinfo.wcpnt4.newlon), iret );
     vftomin ( vorlat1,  vorlon1, &newlat1,  &newlon1, iret );

    /*  The statement below will truncate the minutes at the 100's position.
     *  No rounding takes place.
     */

     fprintf ( ifpwcp, "%05d.%05d %05d.%05d %05d.%05d %05d.%05d %05d.%05d %05d.%05d\n",
         (int)(spcinfo.wcpnt1.newlat * 100.0F), (int)(spcinfo.wcpnt1.newlon * -100.0F),
         (int)(spcinfo.wcpnt2.newlat * 100.0F), (int)(spcinfo.wcpnt2.newlon * -100.0F),
         (int)(newlat2 * 100.0F), (int)(newlon2 * -100.0F),
         (int)(spcinfo.wcpnt3.newlat * 100.0F), (int)(spcinfo.wcpnt3.newlon * -100.0F),
         (int)(spcinfo.wcpnt4.newlat * 100.0F), (int)(spcinfo.wcpnt4.newlon * -100.0F),
         (int)(newlat1 * 100.0F), (int)(newlon1 * -100.0F) );

    /*
     *  Close output file.
     */

     cfl_clos ( ifpwcp, &ier );
}
