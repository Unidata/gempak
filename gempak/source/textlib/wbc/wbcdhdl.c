#include "geminc.h"
#include "gemprm.h"

void wbc_dhdl ( char *wtype, char *status, int *wnum, int lenh, 
	        char *hdlstr, int *iret )
/************************************************************************
 * wbc_dhdl								*
 *                                                                      *
 * This function creates the 'URGENT...' headline section. The format:  *
 *                                                                      *
 *		URGENT - IMMEDIATE BROADCAST REQUESTED			*
 *		(type) WATCH NUMBER (xxxx)				*
 *		NWS STORM PREDICTION CENTER NORMAN OK			*
 *                                                                      *
 * where:								*
 *	type - TORNADO or SEVERE THUNDERSTORM				*
 *	xxxx - Watch number						*
 *                                                                      *
 * wbc_dhdl ( wtype, wnum, lenh, hdlstr, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*wtype		char		Watch type 			*
 *					   TORNADO 			*
 *					   SEVERE THUNDERSTORM		*
 *      *status		char		Watch status - TEST or ACTIVE	*
 *	*wnum		int		Watch number			*
 *	lenh		int		Max length of hdlstr		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*hdlstr		char		'URGENT...' string 		*
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP          5/03						*
 * A. Hardy/NCEP	11/03		Added watch status check 	*
 ***********************************************************************/
{
    int     len, len1,  ier;
    char    tmpstr[500], hold[200];
/*-------------------------------------------------------------------*/
    *iret = 0;
    hold[0]   = '\0'; 
    tmpstr[0] = '\0'; 
    hdlstr[0] = '\0'; 

   /*
    * Create 'URGENT...' paragraph.
    */

    cst_ncpy ( tmpstr, "URGENT - IMMEDIATE BROADCAST REQUESTED\n", 40, &ier);
    sprintf  ( hold, "%s WATCH NUMBER %d", wtype, *wnum );

   /*
    * Check if it is a 'TEST' watch.
    */

    if ( strcmp (status, "TEST" ) == 0 ) {
        cst_ncat ( tmpstr, "TEST...", &len, &ier);
        cst_ncat ( tmpstr, hold, &len, &ier);
        cst_ncat ( tmpstr, "...TEST", &len, &ier);
    }
    else {
        cst_ncat ( tmpstr, hold, &len, &ier);
    }

    cst_ncat ( tmpstr, "\nNWS STORM PREDICTION CENTER NORMAN OK \n", &len, &ier);

    len1 = G_MIN ( lenh, (int)strlen(tmpstr) );
    cst_ncpy ( hdlstr, tmpstr, len1, &ier );
}
