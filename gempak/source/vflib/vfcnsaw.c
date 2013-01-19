#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern	SpcInfo_t	newinfo;
extern	SpcInfo_t	spcinfo;

void vfcnsaw ( int *iret )
/************************************************************************
 * vfcnsaw                                                              *
 *                                                                      *
 * This program opens, creates and closes the Weather Watch cancel SAW  *
 * text product file.							*
 *                                                                      *
 * vfcnsaw ( iret )							*
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
 * A. Hardy/GSC		 5/00   Changed cfl_aopn to cfl_wopn; Use	*
 *				AWIPS/WMO header ids.; removed 'NNNN'	*
 * A. Hardy/GSC		12/00   Added '&' to ier for vfgname,vfctim     *
 * A. Hardy/GSC		 5/01   Initialized iret to 0			*
 * A. Hardy/SAIC	10/01   Added check for old/new WMO header flag *
 * R. Tian/SAIC		06/02	Modified to meet the SPC requirement	*
 * A. Hardy/NCEP	 7/03   Added calls to UTL library		*
 * A. Hardy/NCEp	11/03	Added watch status check for 'TEST'	*
 ***********************************************************************/
{
    FILE    *ifpsaw;
    char    ifname[256];
    int     ier, leni;
/*-------------------------------------------------------------------*/
    *iret = 0;
    ier = 0;

   /*
    *  Create output file for appending.
    */

    utl_gname ( spcinfo.ancrpt.stn1, spcinfo.ancrpt.stnnam1, 
		spcinfo.ancrpt.stateid1, &ier );
    utl_gname ( spcinfo.ancrpt.stn2, spcinfo.ancrpt.stnnam2, 
		spcinfo.ancrpt.stateid2, &ier );

    sprintf ( ifname, "WW%04d.SAW.CNL", spcinfo.wnum );
    ifpsaw = cfl_wopn ( ifname, &ier );

   /*
    * Get current GEMPAK time for issue time of cancel product.
    */

    leni = sizeof(spcinfo.curtim);
    utl_ctim ( leni, spcinfo.curtim, &ier );

   /*
    * Set up header information.
    */
    spcinfo.sssnum = spcinfo.wnum % 10;
    fprintf ( ifpsaw, "WWUS30 KWNS %s\n", spcinfo.curtim);
    fprintf( ifpsaw, "SAW%d\n", spcinfo.sssnum);

    fprintf ( ifpsaw, "%cSPC AWW %s\n", CHRS, spcinfo.curtim);
   /*
    * Set up cancelled line.
    * Check if watch has been issued as a 'TEST'.
    */ 

    if ( strcmp (spcinfo.status, "TEST") == 0 ) {
        fprintf ( ifpsaw, "WW %d TEST %s CANCELLED\n\n", 
	          spcinfo.wnum, spcinfo.wtype); 
    }
    else {
        fprintf ( ifpsaw, "WW %d %s CANCELLED\n\n", 
	          spcinfo.wnum, spcinfo.wtype); 
    }

   /*
    *  Close output file.
    */

    cfl_clos ( ifpsaw, &ier );
}
