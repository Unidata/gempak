#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

#define  EOL  "\n"

extern  SpcInfo_t   newinfo;
extern  SpcInfo_t   spcinfo;

void vfwsaw ( int *iret )
/************************************************************************
 * vfwsaw                                                               *
 *                                                                      *
 * This program opens, creates and closes the Weather Watch SAW text    *
 * product file.							*
 *                                                                      *
 * vfwsaw ( iret )                                             		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          8/99   Created                                 *
 * M. Li/GSC		10/99	Modified output format			*
 * A. Hardy/GSC         11/99   Added replacement watch information     *
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 * A. Hardy/GSC		 3/00   Add check for listing replacement states*
 * A. Hardy/GSC		 5/00   Changed cfl_aopn to cfl_wopn; Use       *
 *				AWIPS/WMO header ids.; removed 'NNNN'	*
 * A. Hardy/GSC		10/00   Added ck for '0' dist. anchor/vor points*
 * A. Hardy/GSC		12/00   Removed '&' from iret			*
 * A. Hardy/SAIC	10/01   Added check for old/new WMO header flag *
 * R. Tian/SAIC		06/02	Modified to meet the SPC requirement	*
 * R. Tian/SAIC		04/03	Corrected if(iret == 0) syntax error	*
 * A. Hardy/NCEP	 6/03	Change hail size to string for decimals *
 * A. Hardy/NCEP	10/03	Modified to use utl/wbc libraries and   *
 * 				added 'TEST' if a test watch		*
 * A. Hardy/NCEP	 3/04	Added wbc_mzrm				*
 * G. Grosshans/SPC	10/04	Added ADD_WATCH_APPROX check		*
 * G. Grosshans/SPC	11/05	Added LATLON_SAW_FORMAT tag to turn     *
 *				off lat-lon info at bottom of product.	*
 * S. Jacobs/NCEP	11/05	Fixed array indices for ilat and ilon	*
 * T. Piper/SAIC	12/05	Updated for cst_wrap CSC		*
 * J. Wu/SAIC		04/06	Added parameter in cst_wrap 		*
 ***********************************************************************/
{
    FILE    *ifpsaw;
    char    blank[2]={' '}, ifname[256], wtch_type[20];
    char    cname1[32], cname2[32], sep[5], cpstates[256], states[256];
    char    newname[132], tmpsaw[500], hwmstr[500];
    char    prefs_tag[] = "LATLON_SAW_FORMAT";
    float   xx, yy;
    int     ii, jj, ier, len, ilat[4], ilon[4];
    Boolean useln;
/*-------------------------------------------------------------------*/
    ier = 0;

   /*
    *  Create output file for appending.
    */

    utl_gname ( spcinfo.ancrpt.stn1, spcinfo.ancrpt.stnnam1, 
		spcinfo.ancrpt.stateid1, &ier );
    utl_gname ( spcinfo.ancrpt.stn2, spcinfo.ancrpt.stnnam2, 
		spcinfo.ancrpt.stateid2, &ier );

    sprintf ( ifname, "WW%04d.SAW", spcinfo.wnum );
    ifpsaw = cfl_wopn ( ifname, &ier );

   /*
    * Set up header information.
    */
    if ( strcmp(spcinfo.wtype,"SEVERE THUNDERSTORM" ) == 0 ) 
                strcpy ( wtch_type, "SEVERE TSTM");
    if ( strcmp(spcinfo.wtype,"TORNADO" ) == 0 ) 
                strcpy ( wtch_type, "TORNADO");

    spcinfo.sssnum = spcinfo.wnum % 10;
    fprintf ( ifpsaw, "WWUS30 KWNS %02d%s\n", 
                      spcinfo.itime.day,spcinfo.itime.hour);
    fprintf ( ifpsaw, "SAW%d \n", spcinfo.sssnum);
    fprintf ( ifpsaw, "%cSPC AWW %02d%s\n", CHRS, 
                      spcinfo.itime.day,spcinfo.itime.hour);

   /*
    * Replace underscores in the county names with a space.
    */

    cst_rnan (  spcinfo.ancrpt.stnnam1, cname1, &ier );
    cst_rnan (  spcinfo.ancrpt.stnnam2, cname2, &ier );

   /*
    * Check if watch has been issued as a 'TEST'.
    */ 

    if ( strcmp (spcinfo.status, "TEST") == 0 ) {
        fprintf ( ifpsaw, "WW %d TEST ", spcinfo.wnum); 
    }
    else {
        fprintf ( ifpsaw, "WW %d ", spcinfo.wnum); 
    }

   /*
    * Check for the marine zones which aren't the Great Lakes or
    * coastal water (CW). Remove them, if found.
    */

    strcpy (cpstates, spcinfo.states);
    wbc_mzrm ( cpstates, states, &len, &ier );

   /*
    * Set up rest of section.
    */

    fprintf ( ifpsaw, "%s %s %02d%sZ - %02d%sZ\n",
              wtch_type, states, spcinfo.vtime.day, 
	      spcinfo.vtime.hour, spcinfo.etime.day, spcinfo.etime.hour );
    fprintf ( ifpsaw, "AXIS..%d STATUTE MILES %s OF LINE..\n",
              spcinfo.ancatt.dist, spcinfo.ancatt.dirc );

   /*
    *  Check for zero distance for anchor points.
    */
    if ( (spcinfo.ancrpt.dist1 != 0 ) && ( spcinfo.ancrpt.dist2 != 0 ) ) {
        fprintf ( ifpsaw, "%d%s %s/%s %s/ - %d%s %s/%s %s/\n",
                  spcinfo.ancrpt.dist1, spcinfo.ancrpt.dirct1, spcinfo.ancrpt.stn1,
                  cname1, spcinfo.ancrpt.stateid1,
                  spcinfo.ancrpt.dist2, spcinfo.ancrpt.dirct2, spcinfo.ancrpt.stn2,
                  cname2, spcinfo.ancrpt.stateid2);
    }
    else if  ( ( spcinfo.ancrpt.dist1 == 0 ) && ( spcinfo.ancrpt.dist2 != 0 ) ) {
        fprintf ( ifpsaw, "%s/%s %s/ - %d%s %s/%s %s/\n",
                  spcinfo.ancrpt.stn1, cname1, spcinfo.ancrpt.stateid1,
                  spcinfo.ancrpt.dist2, spcinfo.ancrpt.dirct2, spcinfo.ancrpt.stn2,
                  cname2, spcinfo.ancrpt.stateid2);
    }
    else if  ( ( spcinfo.ancrpt.dist1 != 0 ) && ( spcinfo.ancrpt.dist2 == 0 ) ) {
        fprintf ( ifpsaw, "%d%s %s/%s %s/ - %s/%s %s/\n",
                  spcinfo.ancrpt.dist1, spcinfo.ancrpt.dirct1, spcinfo.ancrpt.stn1,
                  cname1, spcinfo.ancrpt.stateid1, spcinfo.ancrpt.stn2,
                  cname2, spcinfo.ancrpt.stateid2);
    }
    else if  ( ( spcinfo.ancrpt.dist1 == 0 ) && ( spcinfo.ancrpt.dist2 == 0 ) ) {
        fprintf ( ifpsaw, "%s/%s %s/ - %s/%s %s/\n",
                  spcinfo.ancrpt.stn1, cname1, spcinfo.ancrpt.stateid1, 
		  spcinfo.ancrpt.stn2, cname2, spcinfo.ancrpt.stateid2);
    }
   /*
    * Check for zero distance for the vor points.
    */
    if ( (spcinfo.vorrpt.dist1 != 0 ) && ( spcinfo.vorrpt.dist2 != 0 ) ) {
        fprintf ( ifpsaw, "..AVIATION COORDS.. %dNM %s /%d%s %s - %d%s %s/\n",
                  spcinfo.voratt.dist, spcinfo.voratt.dirc,
                  spcinfo.vorrpt.dist1, spcinfo.vorrpt.dirct1, spcinfo.vorrpt.stn1,
                  spcinfo.vorrpt.dist2, spcinfo.vorrpt.dirct2, spcinfo.vorrpt.stn2);
    }
    else if ( (spcinfo.vorrpt.dist1 == 0 ) && ( spcinfo.vorrpt.dist2 != 0 ) ) {
        fprintf ( ifpsaw, "..AVIATION COORDS.. %dNM %s /%s - %d%s %s/\n",
                  spcinfo.voratt.dist, spcinfo.voratt.dirc, spcinfo.vorrpt.stn1, 
		  spcinfo.vorrpt.dist2, spcinfo.vorrpt.dirct2, spcinfo.vorrpt.stn2);
    }
    else if ( (spcinfo.vorrpt.dist1 != 0 ) && ( spcinfo.vorrpt.dist2 == 0 ) ) {
        fprintf ( ifpsaw, "..AVIATION COORDS.. %dNM %s /%d%s %s - %s/\n",
                  spcinfo.voratt.dist, spcinfo.voratt.dirc, spcinfo.vorrpt.dist1, 
		  spcinfo.vorrpt.dirct1, spcinfo.vorrpt.stn1, spcinfo.vorrpt.stn2);
    }
    else if ( (spcinfo.vorrpt.dist1 == 0 ) && ( spcinfo.vorrpt.dist2 == 0 ) ) {
        fprintf ( ifpsaw, "..AVIATION COORDS.. %dNM %s /%s - %s/\n",
                  spcinfo.voratt.dist, spcinfo.voratt.dirc, 
		  spcinfo.vorrpt.stn1, spcinfo.vorrpt.stn2);
    }

   /*
    * Set up hail, wind gusts and lightning section.
    */

    tmpsaw[0] = '\0';
    cst_ncpy ( sep, "TO", 2, &ier );
    len = sizeof(hwmstr);
    wbc_dhwm ( sep, &(spcinfo.hailsz), &(spcinfo.maxgust), 
                &(spcinfo.maxtops), &(spcinfo.motion.deg), 
		&(spcinfo.motion.speed), len, hwmstr, &ier );
    tmpsaw[0] = '\0';
    strcat ( tmpsaw, hwmstr);
    strcat ( tmpsaw, EOL );
    cst_wrap ( tmpsaw, blank, &len, EOL, (char *)NULL, tmpsaw, &ier );
    fprintf ( ifpsaw, tmpsaw);

   /*
    *  If there are replacement watches listed, retrieve the lists of states
    *  for each replacement watch.
    */

    if ( strcmp ( spcinfo.replcnm[0], "NONE") != 0 ) {
	strcpy ( newname, "ww");
	strcat ( newname, spcinfo.replcnm[0] );
	strcat ( newname, ".txt" );
	strcpy ( newinfo.file_info.filnam,newname);
	vfrptxt (newname, iret); 
	if ( *iret == 0 ){
            wbc_mzrm ( newinfo.states, states, &len, &ier );
            fprintf ( ifpsaw, "\nREPLACES WW %d..%s \n",
	          atoi(spcinfo.replcnm[0]), states);
	}
	else {
	    fprintf ( ifpsaw, "         WW %d.. \n", 
	              atoi (spcinfo.replcnm[0]) );
        }
   /*
    *  If there are more that 1 replacement watches listed,
    *  proceed with the loop.
    */
        for(jj = 1; jj <= spcinfo.wwrepnm; jj++) {
	    strcpy ( newname, "ww");
	    strcat ( newname, spcinfo.replcnm[jj] );
	    strcat ( newname, ".txt" );
	    strcpy ( newinfo.file_info.filnam,newname);
  	    vfrptxt (newname, iret); 
	    if ( *iret == 0 ){
            wbc_mzrm ( newinfo.states, states, &len, &ier );
	    fprintf ( ifpsaw, "REPLACES WW %d..%s \n", 
	              atoi (spcinfo.replcnm[jj]), states);
	    }
	    else {
	      fprintf ( ifpsaw, "         WW %d.. \n", 
	              atoi (spcinfo.replcnm[jj]) );
	    }
        }
	fprintf ( ifpsaw, "\n");
    }
    else {
	    fprintf ( ifpsaw, "\n");
    }

    /*
     * Check if the LATLON_SAW_FORMAT flag is TRUE and then
     * add enhanced lat-lon data to the SAW text.
     * Make sure lats and lons are rounded to 
     * 100ths of deg.
     */

    tmpsaw[0] = '\0';
    ctb_pfbool (prefs_tag, &useln, &ier );
    if ( useln == TRUE ) {
	for ( ii = 0 ; ii < 4 ; ii++ ) {
            switch ( ii ) {
                case 0:
                    xx = ((int)(spcinfo.wcpnt1.lat*100.0F)) / 100.0F;
                    yy = ((int)(spcinfo.wcpnt1.lon*-100.0F)) / 100.0F;
                    break;
                case 1:
                    xx = ((int)(spcinfo.wcpnt2.lat*100.0F)) / 100.0F;
                    yy = ((int)(spcinfo.wcpnt2.lon*-100.0F)) / 100.0F;
                    break;
                case 2:
                    xx = ((int)(spcinfo.wcpnt3.lat*100.0F)) / 100.0F;
                    yy = ((int)(spcinfo.wcpnt3.lon*-100.0F)) / 100.0F;
                    break;
                case 3:
                    xx = ((int)(spcinfo.wcpnt4.lat*100.0F)) / 100.0F;
                    yy = ((int)(spcinfo.wcpnt4.lon*-100.0F)) / 100.0F;
                    break;
		default:
                    break;
            } 
            if ( yy >= 100.0F )  yy = yy - 100.0F;
            ilat[ii] = G_NINT(xx*100.0F);
	    ilon[ii] = G_NINT(yy*100.0F);
        }
        fprintf ( ifpsaw, "LAT...LON %4d%04d %4d%04d %4d%04d %4d%04d\n\n",
            ilat[0], ilon[0],
            ilat[1], ilon[1],
            ilat[2], ilon[2],
            ilat[3], ilon[3]);
    }

    /*
     * Check if the approximation verbage needs to be included
     * in the SAW text.
     */
    tmpsaw[0] = '\0';
    strcpy (prefs_tag, "ADD_WATCH_APPROX");
    ctb_pfbool (prefs_tag, &useln, &ier );

    if ( useln == TRUE ) {
       tmpsaw[0] = '\0';
       fprintf(ifpsaw, "THIS IS AN APPROXIMATION TO THE WATCH AREA.  FOR A\n");
       fprintf(ifpsaw, "COMPLETE DEPICTION OF THE WATCH SEE WOUS64 KWNS\n");
       fprintf(ifpsaw, "FOR WOU%d.\n", spcinfo.sssnum);
       fprintf(ifpsaw, "\n");
    }


   /*
    *  Close output file.
    */

    cfl_clos ( ifpsaw, &ier );
}
