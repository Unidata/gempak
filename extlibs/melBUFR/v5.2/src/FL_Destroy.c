/*
 * FXY_List_Destroy - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void FXY_List_Destroy( FXY_List_t* FL )

#else

void FXY_List_Destroy( FL )
FXY_List_t* FL;

#endif
{
    FXY_Entry_t *fe, *next_FE;

    if( FL == NULL )
        return;

    for( fe=FL->head->next; fe != FL->tail; fe=next_FE )
    {
        next_FE = fe->next;
        free( (void*) fe );
    }

    FL->head->next = FL->tail;

    free( (void*) FL->head );
    free( (void*) FL->tail );
    free( (void*) FL );
}
