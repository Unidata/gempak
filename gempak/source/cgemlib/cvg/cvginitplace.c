#include "cvgcmn.h"

void cvg_initplace(int *iret)
/*****************************************************************************
 * cvg_initplace
 * 
 * Sets up the CVG to maintain a set of global meta-data and placement 
 * structures that will be used to manage auto placement information 
 * from nmap2.  The cvg_load will create a separate set of structures during 
 * a load/display session of a VG file *not* part of product generation.
 *
 * Input parameters:
 *  None
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                               -52 = Could not allocate memory
 **
 * Log:
 * S.Danz/AWC           07/06   Created
 ****************************************************************************/
{
    float       dxl, dxr, dyt, dyb;
    float       pxl, pxr, pyt, pyb;
    float       vxl, vxr, vyt, vyb;
    float       width, height, ref_size;
    Boolean		autopl_enabled;
    int		ier, one=1;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Create the structures to manage the metadata and object placement
     *  information.  If there were structures already in place, leave them
     *  alone.
     */
    if (!cvg_metadata) {
        ctb_pfbool( "ENABLE_AUTOPLACE", &autopl_enabled, &ier );
        if (autopl_enabled) {
            cmd_osnew(&cvg_metadata, &ier);
            if (ier != 0) {
                *iret = -52;
                return;
            }

            cap_psnew(&cvg_placements, &ier);
            if (ier != 0) {
                cmd_osdel(&cvg_metadata, &ier);
                *iret = -52;
                return;
            }

            G_CALLOC(cvg_group_check, cvg_group_info, one, "cvg_load: cvg_group_check");
            if (cvg_group_check == NULL) {
                cap_psdel(&cvg_placements, &ier);
                cmd_osdel(&cvg_metadata, &ier);
                *iret = -52;
            }

            /*
             * Check for config info
             */
            cvg_plrtbl(iret);

            /*
             * Setup the placement set with the necessary information
             * to clip against the device.
             */
            gqbnd( sys_D, &dxl, &dyb, &dxr, &dyt, iret, strlen(sys_D) );
            gqbnd( sys_P, &pxl, &pyb, &pxr, &pyt, iret, strlen(sys_P) );
            width = fabs(dxr - dxl) + 1;
            height = fabs(dyt - dyb) + 1;
            if (width > height) {
                ref_size = width;
            } else {
                ref_size = height;
            }

            /*
             * Adjust the device dimensions down to the drawable portion
             * of the device
             */
            dxl = pxl * ref_size;
            dxr = pxr * ref_size;

            /*
             * Ugh.  If the Y coordinate system is flipped for the device
             * (as say for the X driver), then we need to adjust the Y 
             * margins differently since the plot/view coordinates are with
             * (0,0) in the lower left and the device is in the upper left
             */
            if (dyb > dyt) {
                gqbnd( sys_V, &vxl, &vyb, &vxr, &vyt, iret, strlen(sys_V) );
                /* 
                 * V is the viewable, P is the plot, the difference is the
                 * margins on either side.  Apply those to the appropriate
                 * flipped Y dimension
                 */
                dyb = ( vyb + (vyt - pyt) ) * ref_size;
                dyt = ( vyt - (pyb - vyb) ) * ref_size;
            }  else {
                dyb = pyb * ref_size;
                dyt = pyt * ref_size;
            }
            
            /* Catch the corners */
            if (dyb < 0) dyb = 0;
            if (dxl < 0) dxl = 0;
            if (dyt > height) dyt = height;
            if (dxr > width) dxr = width;

            cap_pssetarea(cvg_placements, dxl, dxr, dyb, dyt, iret);
            cmd_ossetarea(cvg_metadata, dxl, dxr, dyb, dyt, iret);
        }
    }
}
