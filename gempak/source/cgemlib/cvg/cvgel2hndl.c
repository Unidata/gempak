#include "cvgcmn.h"

Handle cvg_el2hndl(int elpos, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_el2hndl
 * 
 * Use the file offset and contents of the VG element to create a Handle for
 * use with CMD and CAP functions.
 *
 * Input parameters:
 *  elpos       int             File offset for the VG element
 *  *el         VG_DBStruct     VG element to have handle created
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *              Handle          Handle value for use with CMD/CAP functions
 **
 * Log:
 * S.Danz/AWC            3/06   Created
 ****************************************************************************/
{
    if (!el) {
        *iret = -1;
        return (*iret);
    }

    return (elpos * 100);
}
