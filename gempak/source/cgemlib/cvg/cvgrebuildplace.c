#include "cvgcmn.h"

void cvg_rebuildplace(char *fname, int *iret)
/*****************************************************************************
 * cvg_rebuildplace
 * 
 * Rebuilds the content of the global meta-data and placement structures that 
 * are be used to manage auto placement information from nmap2 using the VG
 * information from the file given. It is expected that the cvg_initplace is
 * called before this to prepare the structures.
 *
 * Input parameters:
 *  *fname      char		Name of file to load
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 **
 * Log:
 * S.Danz/AWC           07/06   Created
 * S.Danz/AWC           02/07   Use cvg_layer4place() instead of cvg_ld4place
 *				so that layers and filters are used to setup
 *				auto placement information
 ****************************************************************************/
{
    int		ier;
/*---------------------------------------------------------------------*/

    *iret = 0;
    /*
     *  If there are structures, then go ahead and rebuild them.  Otherwise
     *  we don't do anything since there is nothing to rebuild.
     */
    if (cvg_metadata) {
        /*
         * Clear out the old stuff first
         */
        memset(cvg_group_check, 0, sizeof(cvg_group_info));
        cvg_group_check->refs_used = 0;
        cvg_group_check->objs_used = 0;
        cmd_osclear(cvg_metadata, &ier);
        cap_psclear(cvg_placements, &ier);

        /*
         * Fill in the global structures with the information needed.
         * This will setup the new device info at the same time
         */
        cvg_layer4place(fname, iret);

        /*
         * Now, rerun the placement
         */
        cap_psplace(cvg_placements, cvg_metadata, &ier);
    }
}
