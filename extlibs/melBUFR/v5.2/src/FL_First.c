/*
 * FXY_List_First - VERSION: %I%  %E% %T%
 */
/*
 * FXY_List_First() - Return a pointer to the first element in an FXY_List_t.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

FXY_Entry_t* FXY_List_First( FXY_List_t* FL )

#else

FXY_Entry_t* FXY_List_First( FL )
FXY_List_t* FL;

#endif
{
    if( FL == NULL )
        return (FXY_Entry_t*) NULL;
    else
        return FL->head->next;
}
