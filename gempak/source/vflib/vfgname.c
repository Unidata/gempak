#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern	SpcInfo_t    newinfo;
extern	SpcInfo_t    spcinfo;

void vfgname ( int *iret )
/************************************************************************
 * vfgname                                                              *
 *                                                                      *
 * This function gets the station information for the anchor points.  	*
 *                                                                      *
 * vfgname ( iret )                                            		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          8/99   Created                                 *
 * A. Hardy/GSC         11/99   Removed unused variables		*
 * A. Hardy/GSC         01/00   Changed type from int to char           *
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 * A. Hardy/GSC		 5/00   Removed FILE declarations		*
 * A. Hardy/GSC		 5/01   Initialized iret to 0			*
 * R. Tian/SAIC		 7/03	Changed to call cst_gtag		*
 ***********************************************************************/
{
    char    stninfo[128];
    char    hunt[1], type[7];
    int     ier, nstns, maxlen, isrch;
/*-------------------------------------------------------------------*/
    *iret = 0;
    ier = 0;
    strcpy (type, "ANCHOR");
    hunt[0] = '\0';
    maxlen = sizeof(stninfo);
    isrch = 1;

    /*
     * Find the first point's information.
     */

    clo_init ( &ier );
    clo_findstn ( type, spcinfo.ancrpt.stn1, hunt, isrch, maxlen,
                  &nstns, stninfo, &ier ); 
    cst_gtag ( "NAME", stninfo, " ", spcinfo.ancrpt.stnnam1, &ier );
    cst_gtag ( "ST", stninfo, " ", spcinfo.ancrpt.stateid1, &ier );

    /*
     * Find the second point's information.
     */

    clo_findstn ( type, spcinfo.ancrpt.stn2, hunt, 1, maxlen,
                  &nstns, stninfo, &ier); 
    cst_gtag ( "NAME", stninfo, " ", spcinfo.ancrpt.stnnam2, &ier );
    cst_gtag ( "ST", stninfo, " ", spcinfo.ancrpt.stateid2, &ier );
}
