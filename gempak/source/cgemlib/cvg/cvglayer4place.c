#include "cvgcmn.h"

void cvg_layer4place(char *filename, int *iret)
/*****************************************************************************
 * cvg_layer4place
 * 
 * Loads the given file into the currently active CMD and CAP data structures.
 * HOWEVER, it only loads elements that are currently visible based on the
 * layer and filter settings (i.e. this should only be used from NMAP2).
 * If the filename is NULL, then the workfile is used.  This also sets up the
 * device bounds information using cvg_setupplace.
 *
 * Input parameters:
 *  *filename   char            Name of file to load into the CMD and CAP 
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = error opening VG file 
 *                                -2 = error closing VG file
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           02/07   Created
 * M. Li/SAIC           03/07   Updated cvg_matchfilter                 
 ****************************************************************************/
{
    VG_DBStruct el;
    FILE        *fptr;
    char        grp[4], newfil[LLPATH];
    long        filesize;
    int         ier, ier1, at_eof, loglev, curpos;
    filter_t    el_filter, timeMatched;
    Boolean     filter_match, matchAny;
    int         layer, cur_layer;
/*---------------------------------------------------------------------*/

    at_eof        = 0;
    loglev        = 0;
    curpos        = 0;
    *iret         = 0;

    /*
     * Set up device information
     */
    cvg_setupplace(iret);

    /*
     * If there was no valid configuration file for placement, just leave
     * (its also set invalid if placement was disabled in prefs.tbl)
     */
    if (!cvg_cap_conf_info_valid) {
        return;
    }

    if (!filename) {
        filename = cvg_getworkfile();
    }

    cfl_inqr(filename, NULL, &filesize, newfil, &ier);
    if (ier < 0) {
        *iret = -1;
        strcpy(grp, "CVG");
        er_lmsg( &loglev, grp, &ier, filename, &ier1, strlen(grp),strlen(filename));
        return;
    }

    cvg_open(filename, 0, &fptr, &ier);
    if (( ier != 0 ) || ( fptr == NULL )) {
        *iret = -1;
        strcpy(grp, "CVG");
        er_lmsg( &loglev, grp, &ier, filename, &ier1, strlen(grp),strlen(filename));
        return;
    }

    /*
     * Get what the current layer is
     */
    cur_layer = pglayer_getCurLayer ();

    /*
     * Set the indicator that we are currently 'loading' the meta data
     * so that the cvg_rdele() doesn't try adjusting each element 
     * while we load since there will be nothing computed (yet) and the
     * lookups would be a waste of time.
     */
    cvg_meta_loading = 1;

    while (curpos  < filesize && !at_eof) {
        cvg_rdhdr(filename, fptr, curpos, filesize, &el, &at_eof, iret);
        cvg_rdele(&el, curpos, el.hdr.recsz, fptr, iret);

        /*
         * If it doesn't match the filter, just ignore it
         */
        cvg_getElFilter ( &el, el_filter, &ier ); 
        cvg_matchfilter ( el_filter, matchAny, &filter_match, timeMatched, &ier );
        if (filter_match) {
            /*
             * If it is not on an active layer, ignore it 
             */
            layer = crg_getLayer ( curpos );
            if (layer == cur_layer || pglayer_getDsplOn(layer)) {
                /*
                 * So, it should be visible, so add the element to the structures
                 */
                cvg_el2place(curpos, &el, iret);
            }
        }

        curpos += el.hdr.recsz;     

        /*
         * Free TCA break point/GFA block memory
         */
        if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        } else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }
    }

    /*
     * Now that is done, the cvg_rdele() can/should use the information
     * from the placement system to adjust the elements. NOTE: We haven't
     * placed anything yet, but its fair game for the cvg_rdele() to look
     * at the info at this point.  The caller has to decide when the 'right'
     * time is to call for the placement to be calculated.
     */
    cvg_meta_loading = 0;

    cvg_clos(fptr, &ier);
    if ( ier != 0 )
        *iret = -2;

    return;
}
