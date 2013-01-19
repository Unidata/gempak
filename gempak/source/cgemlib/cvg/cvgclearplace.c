#include "cvgcmn.h"

void cvg_clearplace(int *iret)
/*****************************************************************************
 * cvg_clearplace
 * 
 * Asks the CVG to clear out the global meta-data and placement 
 * structures that are used to manage auto placement information 
 * from nmap2.
 *
 * Input parameters:
 *  None
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 **
 * Log:
 * S.Danz/AWC           07/06   Created
 ****************************************************************************/
{
    int		        ier;
/*---------------------------------------------------------------------*/

    *iret = 0;
    /*
     *  Clear the structures to manage the metadata and object placement
     *  information if they exist.
     */
    if (cvg_metadata) {
        cmd_osdel(cvg_metadata, &ier);
        cvg_metadata = NULL;
        cap_psdel(cvg_placements, &ier);
        cvg_placements = NULL;
        G_FREE(cvg_group_check, cvg_group_info);
        cvg_group_check = NULL;
    }
}
