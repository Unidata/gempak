#include "cvgcmn.h"

void cvg_updateplace(Boolean place_all, int *iret)
/*****************************************************************************
 * cvg_updateplace
 * 
 * Asks the CVG to run the auto placement and update the locations in the 
 * placement structures used to hold the information from nmap2.
 *
 * Input parameters:
 *  place_all   Boolean         Flag to force the placement of all objects
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 **
 * Log:
 * S.Danz/AWC           07/06   Created
 ****************************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;
    /*
     *  If the nmap2 information is available, then update it
     */
    if (cvg_metadata) {
        /*
         * First, if the flag is set then clear all the objects
         */
        if (place_all) {
            cap_psclrplaced(cvg_placements, iret);
        }

        /*
         * Ok, all ready, so place all the objects where they belong
         */
        cap_psplace(cvg_placements, cvg_metadata, iret);
    }
}
