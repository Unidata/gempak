#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

#define  EOL  "\n"

void vfasaw ( char *rcntsw, int *iwtnum, char *amdcde, int *iret )
/************************************************************************
 * vfasaw                                                               *
 *                                                                      *
 * This program opens, creates and closes the Weather Watch amended SAW	*
 * text product file having an extension in time.			*
 *                                                                      *
 * vfasaw ( rcntsw, iwtnum, amdcde, iret )	    			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*rcntsw		char		Most recent SAW bulletin	*
 *	*iwtnum		int		watch number			*
 *	*amdcde		char		Amendment Code(AAx, x=A,B,C,...)*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	 1/07	Created                                 *
 ***********************************************************************/
{
    FILE    *ifpsaw;
    char    ifname[256];
    int	    ier;
/*-------------------------------------------------------------------*/
    ier = 0;

   /*
    *  Create output file for amended SAW.
    */
    sprintf ( ifname, "WW%04d.SAW.AM%c", *iwtnum, amdcde[2] );
    ifpsaw = cfl_wopn ( ifname, &ier );

    fprintf ( ifpsaw, "%s", rcntsw );
    fprintf ( ifpsaw, "\n");
	
   /*
    *  Close output file.
    */

    cfl_clos ( ifpsaw, &ier );
}
