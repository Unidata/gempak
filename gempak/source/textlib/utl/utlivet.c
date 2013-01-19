#include "geminc.h"
#include "gemprm.h"

void utl_ivet ( char *lclzn, int itmarr[], int vtime[], int etime[],
	        int iarr[], int *inewtm, char *iampm, 
		char *chmon, char *chdwk, int varr[], int *vnewtm, 
		char *vampm, int earr[], int *enewtm, char *eampm, 
		int *datwk, int *iret)
/************************************************************************
 * utl_ivet                                                             *
 *                                                                      *
 * This function takes the local GMT initial, valid and ending times    *
 * and returns the 3 character id for the month and the day, the day 	*
 * number ( 1-Sun, 2-Mon, etc.), the local issue, valid and ending 	*
 * 12 hour time arrays and the AM/PM designation for each time. For	*
 * example: 10 PM 2003  6 12 22 30.					*
 *                                                                      *
 * utl_ivet ( lclzn, itmarr, vtime, etime, iarr, inewtm, iampm, chmon,  *
 *            chdwk, varr, vnewtm, vampm, earr, enewtm, eampm,		*
 *	      datwk, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      *lclzn		char		Local time zone			*
 * 	itmarr[5]	int		Issue GMT time array		*
 * 	vtime[5]	int		Valid GMT time array		*
 * 	etime[5]	int		Ending GMT time array		*
 *									*
 * Output parameters:                                                   *
 * 	iarr[5]		int		New issue time array		*
 * 	*inewtm		int		New local issue time 		*
 *      *iampm          char            Issue AM/PM designation		*
 *      *chmon		char		3 char. month id		*
 *      *chdwk		char		3 char. day of the week id	*
 * 	varr[5]		int		New valid time array		*
 * 	*vnewtm		int		New local valid time 		*
 *      *vampm          char            Valid AM/PM designation		*
 * 	earr[5]		int		New ending time array		*
 * 	*enewtm		int		New local ending time 		*
 *      *eampm          char            Ending AM/PM designation	*
 * 	*datwk		int		Numerical day of the week	*
 *      *iret           int             Return Code                     *
 *					   -14 = bad time zone string	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 5/03						*
 * T. Piper/SAIC	02/04	Removed unused variables lend and lenp	*
 * H. Zeng/SAIC		06/06	removed MIDNIGHT processing & parm jarr	*
 ***********************************************************************/
{
    int     ier, len1, wkdy;
    float   hdiff;
    char    zone1[4];
/*-------------------------------------------------------------------*/
    *iret = 0;
    ier   = 0;
    *inewtm = 0;
    *vnewtm = 0;
    *enewtm = 0;
    chmon[0] = '\0';


    wkdy = 0;
    cst_lstr ( lclzn, &len1, &ier );
    if ( len1 != 3 ) {
	*iret = -14;
	return;
    }

   /*
    * Getting all of the information to create the the time line string.
    */

    cst_ncpy ( zone1, "UTC", 4, &ier );

    ti_dayw( itmarr, &wkdy, &ier );
    utl_gmon ( itmarr[1], chmon, &ier );
    utl_gdwk ( wkdy, chdwk, &ier );

    ti_tzdf ( itmarr, zone1, lclzn, iarr, &hdiff, &ier, 
              strlen(zone1), strlen(lclzn) ); 
    utl_ampm ( iarr[3], inewtm, iampm, &ier );

    if ( itmarr[2] != iarr[2] ) {
        ti_dayw( iarr, &wkdy, &ier);
        utl_gmon ( iarr[1], chmon, &ier );
        utl_gdwk ( wkdy, chdwk, &ier );
    }

    *datwk = wkdy;
			 
   /*
    * Set up local valid times.
    */

    ti_tzdf ( vtime, zone1, lclzn, varr, &hdiff, &ier, 
              strlen(zone1), strlen(lclzn) ); 
    utl_ampm ( varr[3], vnewtm, vampm, &ier );

   /*
    * Set up local ending times.
    */

    ti_tzdf ( etime, zone1, lclzn, earr, &hdiff, &ier, 
              strlen(zone1), strlen(lclzn) ); 
    utl_ampm ( earr[3], enewtm, eampm, &ier );
}
