#include "cvgcmn.h"

void cvg_checkplace(VG_DBStruct *el, int to_delete, int location, int *found, 
float inf_bbox[4], int *iret)
/*****************************************************************************
 * cvg_checkplace
 * 
 * Asks the CVG to check the given VG object against the global meta-data and 
 * placement structures that are used to manage auto placement information 
 * from nmap2 to see if the VG object impacts any of the elements being placed.
 * For each placed element impacted, mark it as not placed so it will be 
 * updated on the next placement refresh.
 *
 * Input parameters:
 *  *el             VG_DBStruct     VG object to check against placement objects
 *  to_delete       int             Indicates if the element is to be deleted
 *  location        int             File offset for the element
 *
 * Output parameters:
 *  *found          int             Number of objects that the element could 
 *                                  impact
 *  *inf_bbox       float           Bounding box for the coordinates of the
 *                                  area influenced by the objects impacted
 *                                  in the order, minx, maxx, miny, maxy
 *  *iret           int             Return code
 *                                    0 = Function successful
 **
 * Log:
 * S.Danz/AWC           07/06   Created
 ****************************************************************************/
{
    int         ier;
    Handle      id;
    float       bbox[4], tmpbb[4];
/*---------------------------------------------------------------------*/

    ier = 0;
    *iret = 0;
    *found = 0;
    inf_bbox[0] = 0.0;
    inf_bbox[1] = 0.0;
    inf_bbox[2] = 0.0;
    inf_bbox[3] = 0.0;

    /*
     *  If structures exist, then check against them 
     */
    if (cvg_placements) {
        /*
         * Get the extent of influence for this object.  
         */
        id = cvg_el2hndl(location, el, &ier);
        cap_psgetplarea(cvg_placements, cvg_metadata, id, to_delete, bbox, &ier);

        /*
         * If its composite (like GFA) then get impact of the object and its 
         * attribute text box and merge the areas
         */
        if ( el->hdr.vg_type == GFA_ELM) {
            id += 1;
            cap_psgetplarea(cvg_placements, cvg_metadata, id, to_delete, tmpbb, &ier);
            bbox[0] = G_MIN(bbox[0], tmpbb[0]);
            bbox[1] = G_MAX(bbox[1], tmpbb[1]);
            bbox[2] = G_MIN(bbox[2], tmpbb[2]);
            bbox[3] = G_MAX(bbox[3], tmpbb[3]);
        }

        /*
         * Mark the placement objects that intersect the area
         * and retreive the area impacted
         */
        cap_psmarkint(cvg_placements, cvg_metadata, bbox, found, inf_bbox, iret);
    }
}
