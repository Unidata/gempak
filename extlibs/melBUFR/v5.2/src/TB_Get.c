/*
 * TableB_Get - VERSION: %I%  %E% %T%
 */
/*
 * TableB_Get - Search Table B's list of descriptors for a
 * descriptor which matches the given FXY value.  Return a NULL pointer
 * if one cannot be found.
 */

#include <mel_bufr.h>
extern TableB_t* TableB;

#if PROTOTYPE_NEEDED

Descriptor_t* TableB_Get( FXY_t FXYval )

#else

Descriptor_t* TableB_Get( FXYval )
FXY_t FXYval;

#endif
{

    TableB_Entry_t* BE;

    for( BE=TableB->head->next; BE != TableB->tail; BE=BE->next )
    {
        if( BE->item->fxy_value == FXYval )
            return BE->item;
    }

    /* Matching descriptor wasn't found. */

    return (Descriptor_t*) NULL;
}
