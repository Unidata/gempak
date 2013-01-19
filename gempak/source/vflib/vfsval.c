#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern SpcInfo_t      spcinfo;

void vfsval ( char *wtype, char *etime, char *fcstr, int ncyfip, 
                           int icyfip[], int *iret )
/************************************************************************
 * vfsval                                                               *
 *                                                                      *
 * This program sets the values of some of the variables in the 	*
 * global structure in the VF library. 		                        *
 *                                                                      *
 * vfsval ( wtype, etime, fcstr, ncyfip, icyfip, iret )                 *
 *                                                                      *
 * Input parameters:                                                    *
 *      *wtype          char	Watch type                      	*
 *	*etime		char	Expiration time (YYMMDD/HHMM)		*
 *	*fcstr		char	Forecaster name				*
 *	ncyfip		int	Number of county FIPS codes		*
 *	icyfip[]	int	Array of county FIPS codes		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		06/02	Coded					* 
 * D.W.Plummer/NCEP	10/02	Added initialization of spcinfo struct	*
 * A. Hardy/NCEP	 3/04	Added logic for missing state ids;	*
 *				COUNTY->MZ_CNTY; nc-1 -> nc		*
 ***********************************************************************/
{
    int		idtarr[5];
    char	info[128], ctylat[7], ctylon[7];
    int		nret, ier, ilen, ibeg, ipos;
    size_t	ii, nc;

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Set watch type.
     */
    strncpy ( spcinfo.wtype, wtype, sizeof(spcinfo.wtype) );

    /*
     * Set expiration time.
     */
    ti_ctoi ( etime, idtarr, &ier, strlen(etime) );
    spcinfo.etime.year  = idtarr[0];
    spcinfo.etime.month = idtarr[1];
    spcinfo.etime.day   = idtarr[2];
    sprintf ( spcinfo.etime.hour, "%02d%02d", idtarr[3], idtarr[4] );

    /*
     * Set forecaster.
     */
    strncpy ( spcinfo.frcstr, fcstr, sizeof(spcinfo.frcstr) );

    clo_init ( &ier );
    /*
     * Initialize spcinfo structure.
     */
    spcinfo.total = 0;
    strcpy ( spcinfo.states , "\0");
    for ( ii = 0; ii < sizeof(spcinfo.cnty)/sizeof(struct county_info); ii++ ) {
	spcinfo.cnty[ii].ugc[0]   = '\0';
	spcinfo.cnty[ii].state[0] = '\0';
	spcinfo.cnty[ii].cname[0] = '\0';
	spcinfo.cnty[ii].fips[0]  = '\0';
	spcinfo.cnty[ii].wfo[0]   = '\0';
	spcinfo.cnty[ii].ctylat  = RMISSD;
	spcinfo.cnty[ii].ctylon  = RMISSD;
    }
    /*
     * Set county names.
     */
    for ( ii = 0, nc = 0; 
          ii < (size_t)ncyfip && ii < sizeof(spcinfo.cnty)/sizeof(struct county_info); ++ii ) {
        clo_findnum ( "MZ_CNTY", icyfip[ii], sizeof(info), &nret, info, &ier );
	if ( ier == 0 ) {
	    cst_gtag ( "STID",   info, "", spcinfo.cnty[nc].ugc, &ier );
	    cst_gtag ( "ST",     info, "", spcinfo.cnty[nc].state, &ier );
	    cst_gtag ( "NAME",   info, "", spcinfo.cnty[nc].cname, &ier );
	    cst_gtag ( "LAT",    info, "", ctylat, &ier );
	    cst_gtag ( "LON",    info, "", ctylon, &ier );
	    cst_gtag ( "STNM",   info, "", spcinfo.cnty[nc].fips, &ier );
	    cst_gtag ( "COL10",  info, "", spcinfo.cnty[nc].wfo, &ier );
	    cst_crnm ( ctylat, &spcinfo.cnty[nc].ctylat, &ier );
	    cst_crnm ( ctylon, &spcinfo.cnty[nc].ctylon, &ier );
	    ++nc;
	}
    }
    spcinfo.total = nc;

    /*
     * Create state string if one doesn't exist.
     */
     
     cst_lstr (spcinfo.states, &ilen, &ier );
     if (ilen == 0 ) { 
         strcpy (spcinfo.states, spcinfo.cnty[0].state); 
         strcat (spcinfo.states, " "); 

         for ( ii = 1; ii < nc; ii++) {
	     ibeg = 0;
             cst_lstr ( spcinfo.states, &ilen, &ier );
	     cst_srch ( ibeg, ilen, spcinfo.cnty[ii].state, spcinfo.states, 
	                &ipos, &ier );
             if ( ipos < 0 ) {
	         cst_ncat (spcinfo.states,  spcinfo.cnty[ii].state, &ilen, &ier);
                 strcat (spcinfo.states, " "); 
	     }
         }
     }
}
