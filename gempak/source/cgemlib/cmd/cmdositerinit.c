#include "cmdcmn.h"

void cmd_ositerinit(CMDObjectSet objects, int *iret)
/*****************************************************************************
 * cmd_ositerinit
 *
 * Initializes the CMDObjectSet iterator so that the cmd_ositernext can be
 * used to iterate over the CMDObjects contained in the set.
 *
 * Input parameters:
 *  objects     CMDObjectSet    handle to CMDObjectSet to iterate 
 *
 * Output parameters:
 *  *iret       int    Return code
 *                          0 = Function successful
 *                         -1 = Invalid pointer in arguments
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    CMDObjectSetContainer  *o;
/*---------------------------------------------------------------------*/

    if (!objects) {
        *iret = -1;
        return;
    }

    *iret = 0;
    o = (CMDObjectSetContainer *)objects;
    o->iter = 0;

    return;
}
