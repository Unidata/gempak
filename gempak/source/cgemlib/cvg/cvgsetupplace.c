#include "cvgcmn.h"

void cvg_setupplace(int *iret)
/*****************************************************************************
 * cvg_setupplace
 * 
 * Sets up the device bounds information for the CMD and CAP libraries.
 *
 * Input parameters:
 *   none
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           02/07   Created
 ****************************************************************************/
{
    float       dxl, dxr, dyt, dyb;
    float       pxl, pxr, pyt, pyb;
    float       vxl, vxr, vyt, vyb;
    float       width, height, ref_size;
/*---------------------------------------------------------------------*/

    /*
     * Check for config info
     */
    cvg_plrtbl(iret);

    /*
     * If there was no valid configuration file for placement, just leave
     * (its also set invalid if placement was disabled in prefs.tbl)
     */
    if (!cvg_cap_conf_info_valid) {
        return;
    }

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

    return;
}
