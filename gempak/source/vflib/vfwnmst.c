#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

void vfwnmst ( char *wfoid, char *wname, char *wstate, int *iret )
/************************************************************************
 * vfwnmst                                                              *
 *                                                                      *
 * This program uses the WFO IDs to find the WFO Names and States.	* 
 *                                                                      *
 * vfwnmst ( wfoid, wname, wstate, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *wfoid          char	The 3 letter identifier for the WFO    	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*wname		char	WFO name				*
 *	*wstate		char	WFO state				*
 *      *iret           int     Return Code	                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		06/02	Coded					* 
 * R. Tian/SIC		07/03	Changed to call cst_gtag		*
 ***********************************************************************/
{
    char	info[128];
    int		nret, ier;

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Init CLO library.
     */
    clo_init(&ier);

    /*
     * Find the corresponding line in WFO.TBL based on the WFO.
     */
    clo_findstn("WFO", wfoid, "", 1, sizeof(info), &nret, info, &ier);

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
