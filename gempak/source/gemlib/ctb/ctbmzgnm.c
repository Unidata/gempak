#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"


void ctb_mzgnm ( char *stid, char *fulnam, int *iret )
/************************************************************************
 * ctb_mzgnm								*
 *									*
 * This sugroutine takes a marine zone station id (eg. ANZ301) and 	*
 * finds the full name of the zone.					*
 *									*
 * ctb_mzgnm ( stid, fulnam, iret )					*
 *									*
 * Input parameters:							*
 *	*stid		char		Marine zone station id		*
 *									*
 * Output parameters:							*
 *	*fulnam		char		Full name of marine zone id  	*
 *	*iret		int		Return code			*
 *					  -1 = marine names not stored  * 
 **									*
 * Log:									*
 * A. Hardy/NCEP	 3/04						*
 ***********************************************************************/
{
    int			ii, ier, nstns;
    char		dir[5];
    Boolean 		nomtch;
    static Marzon_t 	mznms;
    static int		mzread = 0;
/*---------------------------------------------------------------------*/
    *iret = 0;

    if ( mzread == 0 )  {
	strcpy ( dir, "stns");
        ctb_mzrd ( MZNAMES_TBL, dir, &nstns, &mznms, &ier );
        mzread = 1;
    }

    cst_lcuc ( stid, stid, &ier);

   /*
    * Set station counter to zero.
    */
    ii = 0;
    nomtch = False;

    while ( ( !nomtch ) && ( ii < mznms.nummz ) ) {
        if (strcmp ( stid, mznms.mzones[ii].mzid ) == 0 ) {
	            strcpy( fulnam, mznms.mzones[ii].name );
		    nomtch = True;
	}
        else {
	        ii++;
	}
    }

    if ( !nomtch ) {
        *iret = -1;
        strcpy (fulnam, " " );
    }
}
