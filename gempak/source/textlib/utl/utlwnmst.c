#include "geminc.h"
#include "gemprm.h"

void utl_wnmst ( char *wfoid, char *wname, char *wstate, int *iret )
/************************************************************************
 * utl_wnmst                                                            *
 *                                                                      *
 * This program uses the WFO IDs to find the WFO name and state id.	* 
 *                                                                      *
 * utl_wnmst ( wfoid, wname, wstate, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *wfoid          char	The 3 letter identifier for the WFO    	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*wname		char	Full WFO name				*
 *	*wstate		char	WFO 2-char state id			*
 *      *iret           int     Return Code				*
 *				   -14 = Bad WFO ID			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 7/03						*
 ***********************************************************************/
{
    char	info[180];
    int		nret, ier, len;

/*---------------------------------------------------------------------*/

    *iret = 0;

    cst_lstr ( wfoid, &len, &ier );
    if ( len != 3 ) {
	*iret = -15;
	return;
    }

    /*
     * Initialize the CLO library.
     */

    clo_init(&ier);

    /*
     * Find the corresponding line in WFO.TBL based on the WFO.
     */

    clo_findstn("WFO", wfoid, "", 1, sizeof(info), &nret, info, &ier);

    if ( ier == 0 ) { 

       /*
        * Retrieve those needed parts.
        */

	cst_gtag ( "NAME", info, " ", wname, &ier );
        cst_gtag ( "ST", info, " ", wstate, &ier );

        /*
         * Replace any underscores with space in the WFO name.
         */

        cst_rnan(wname, wname, &ier);
    }
    else {
	*iret = -14;
    }
}
