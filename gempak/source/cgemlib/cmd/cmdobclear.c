#include "cmdcmn.h"

void cmd_obclear(CMDObjectContainer *object)
/*****************************************************************************
 * cmd_obclear
 * 
 * Clear the contents of the given CMDObjectContainer to prevent old values
 * from causing problems if the container is reused.
 *
 * Input parameters:
 *  *object CMDObjectContainer  Container to clear
 *
 * Output parameters:
 *  None
 **
 * Log:
 * S.Danz/AWC            2/06   Created
 ****************************************************************************/
{
    if (object) {
        if (object->vertex_x) 
            G_FREE(object->vertex_x, float);
        if (object->vertex_y) 
            G_FREE(object->vertex_y, float);

        memset(object, 0, sizeof(CMDObjectContainer));
        object->id = -1;
    }
}
