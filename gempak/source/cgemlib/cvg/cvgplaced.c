#include "cvgcmn.h"

Boolean cvg_placed(int elpos, VG_DBStruct *el)
/*****************************************************************************
 * cvg_placed
 * 
 * Use the file offset and contents of the VG element and check if the 
 * object is placed, or anything that its a reference to is placed
 *
 * Input parameters:
 *  elpos       int             File offset for the VG element
 *  *el         VG_DBStruct     VG element to check
 *
 * Output parameters:
 *  None
 *
 * Return value:
 *              Boolean         FALSE if anything is not placed, 
 *				TRUE  if everything is placed
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    int         result, placed, iret, unplaced_found;
    Handle      id, ref;
    Placement   place;
/*---------------------------------------------------------------------*/

    result = 0;
    if (cvg_metadata && cvg_placements) {
        id = cvg_el2hndl(elpos, el, &iret);
        /*
         * First see if we are in CAP and if we have been placed
         */
        cap_psgetpl(cvg_placements, id, &place, &iret);
        if (place) {
            cap_plgetplaced(place, &placed, &iret);
            if (placed) {
                result = 1;
            }
        }

        /*
         * Then see if we are a reference, and if we are, if our
         * objects are placed
         */

        if (!result) {
            unplaced_found = 0;
            cap_psiterinit(cvg_placements, &iret);
            cap_psiternext(cvg_placements, &place, &iret);
            while (place) {
                cap_plgetref(place, &ref, &iret);
                if (ref == id) {
                    cap_plgetplaced(place, &placed, &iret);
                    if (placed) {
                        result = 1;
                    } else {
                        unplaced_found = 1;
                    }
                }
                cap_psiternext(cvg_placements, &place, &iret);
            }

            /*
             * If we find anything non placed, then the result should be 0
             */
            if (unplaced_found) {
                result = 0;
            }
        }
    }

    return result;
}
