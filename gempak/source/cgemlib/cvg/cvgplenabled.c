#include "cvgcmn.h"

int cvg_plenabled(void)
/*****************************************************************************
 * cvg_plenabled
 * 
 * Return 1 if autoplacement is setup and ready to be used, 0 otherwise
 *
 * Input parameters:
 *  None
 *
 * Output parameters:
 *  None
 *
 * Return value:
 *  int             1 if auto placement is enabled, 0 otherwise
 **
 * Log:
 * S.Danz/AWC            7/06   Created
 ****************************************************************************/
{
    /*
     * If the placement data structures are configured, then we should be
     * good to go
     */
    if (cvg_placements) {
        return 1;
    }

    return 0;
}
