#include "geminc.h"
#include "gemprm.h"

void gg_wusc ( char *ugcin, char *stin, char *namin, int *numb, 
               char *cday, char *chour, int *vtecln, char *prdcod,
	       char *actn, char *offid, char *phen, char *sigcd, 
	       char *etn, int vtime[], int etime[], char *cntystr, 
	       int *ilenout, int *iret)
/************************************************************************
 * gg_wusc                                                              *
 *                                                                      *
 * This subroutine transforms three strings containing UG codes (county *
 * and/or marine zone IDs), county and/or marine zone names and state	*
 * IDs with each separated by ';' into a formatted string consisting of	*
 * the UGC string, VTEC string, and county names grouped by states.	*
 * It returns the formatted string and its length.  This formatted	*
 * string is a part of the WOU text product.				*
 *                                                                      *
 * gg_wusc ( ugcin, stin, namin, numb, cday, chour, vtecln, prdcod, 	*
 *           actn, offid, phen, sigcd, etn, vtime, etime, cntystr,	*
 *	     ilenout, iret)						*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ugcin		char		UG codes string			*
 *	*stin		char		State ID string			*
 *	*namin		char		County name & marine zone ID str*
 *	*numb		int		Number of counties		*
 *	*cday		char		Date (DD)			*
 *	*chour		char		Time (HHMM)			*
 *	*vtecln		int		VTEC line flag			*
 *					  0 - No VTEC line		*
 *					  1 - VTEC line; no product code*
 *					  2 - VTEC line; product code	*
 *	*prdcod		char		VTEC product code		* 
 *	*actn		char		VTEC action code		*
 *	*offid		char		VTEC office ID			*
 *	*phen		char		VTEC phenomena type		*
 *	*sigcd		char		VTEC significance code		*
 *	*etn		char		VTEC event trackin number	*
 *	*vtime[]	int		VTEC valid time array		*
 *	*etime[]	int		VTEC ending time array		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*cntystr	char		Formatted UGC, VTEC,& county str*
 *	*ilenout	char		Length of cntystr		*
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/03                                           *
 * A. Hardy/NCEP	 3/04		Added check for marine zones	*
 * A. Hardy/NCEP	 3/05		Add if 3rd char in mz is alpha	*
 * F. J. Yen/NCEP	12/05		Refined check for marine zones; *
 *					completed and clarified prologue*
 ***********************************************************************/
{
     int     ii, ier, numstr, num, leng;
     int     maxch, len, ilen, lenc, lens, ugcln, itype;
     char    **ugc_arr, **cnam_arr, **st_arr, **ind_arr;
/*-------------------------------------------------------------------*/
     *iret  = 0;
     ier    = 0;
     len    = 6;
     lenc   = 256;
     lens   = 2;
     leng   = 32;
     maxch  = 50;

    /*
     * Allocate memory for array size.
     */

    ugcln  = 1;
    num = *numb;
    ugc_arr = (char **)malloc(num * sizeof(char *));
    cnam_arr = (char **)malloc(num * sizeof(char *));
    st_arr = (char **)malloc(num * sizeof(char *));
    ind_arr = (char **)malloc(num * sizeof(char *));

    ind_arr[0] = '\0';
    for ( ii = 0; ii < num; ii++ ) {

	ugc_arr[ii] = (char *)malloc((len+1) * sizeof(char));
	cnam_arr[ii] = (char *)malloc((lenc+1) * sizeof(char));
	st_arr[ii] = (char *)malloc((lens+1) * sizeof(char));
	ind_arr[ii] = (char *)malloc((leng+1) * sizeof(char));
    }

    /*
     * Break apart the UG code, county/zones and state id strings into 
     * arrays.
     */

     cst_lstr ( ugcin, &ilen, &ier );
     cst_rmbl ( ugcin, ugcin, &ilen, &ier);
     cst_clst ( ugcin, ';', " ", num, maxch, ugc_arr, &numstr, &ier);

     cst_lstr ( stin, &ilen, &ier );
     cst_rmbl ( stin, stin, &ilen, &ier);
     cst_clst ( stin, ';', " ", num, maxch, st_arr, &numstr, &ier);

     cst_lstr ( namin, &ilen, &ier );
     cst_rmbl ( namin, namin, &ilen, &ier);
     cst_lcuc ( namin, namin, &ier);
     cst_clst ( namin, ';', " ", num, maxch, cnam_arr, &numstr, &ier);

   /*
    * Change marine zone ids (if any) to full names.
    */

    for ( ii = 0; ii < num; ii++ ) {
        if ( cnam_arr[ii][2] == 'Z') {
	    cst_lstr ( cnam_arr[ii], &ilen, &ier );
	    if ( ilen == 6 ) {
		if ( isdigit (cnam_arr[ii][3]) &&
		     isdigit (cnam_arr[ii][4]) &&
		     isdigit (cnam_arr[ii][5]) ) {
                    ctb_mzgnm ( cnam_arr[ii], cnam_arr[ii], &ier );
	    	    if ( ier < 0 ) {
                	cst_alnm ( cnam_arr[ii][3], &itype, &ier );
	        	if ( itype == 2  ) {
	            	    printf("Do not have full name.\n" );
			}
		    }
                }
	    }
	}

    }

    /*
     * Get list of counties and zones in a UGC formatted string.
     */

     leng = 10000;
     wbc_dcty ( ugc_arr, cnam_arr, st_arr, &num, cday, chour, &leng, 
                &ugcln, vtecln, prdcod, actn, offid, phen, sigcd, 
		etn, vtime, etime, ind_arr, cntystr, &ier );

     cst_lstr ( cntystr, &leng, &ier);
     *ilenout = leng;
     
    /*
     * Free memory.
     */

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
